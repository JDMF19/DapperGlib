using Dapper;
using DapperGlib.Exceptions;
using DapperGlib.Util;
using Newtonsoft.Json;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using DapperGlib.Internal;
using System.Threading;
using DapperGlib.Relationships;
using System.Linq.Expressions;

namespace DapperGlib
{
    public class QueryBuilder<TModel> : Builder<TModel>
    {
        public QueryBuilder()
        {

        }

        public QueryBuilder(QueryBuilder<TModel> Clone)
        {
            var deserializeSettings =
                new JsonSerializerSettings
                {
                    ObjectCreationHandling =
                        ObjectCreationHandling.Replace
                };

            Query = new(Clone.Query.ToString());

            SubQueries =
                JsonConvert.DeserializeObject<List<object>>(
                    JsonConvert.SerializeObject(Clone.SubQueries),
                    deserializeSettings
                ) ?? new();


            RelationshipProjections = new List<string>(Clone.RelationshipProjections);

            OrderList =
                JsonConvert.DeserializeObject<List<string>>(
                    JsonConvert.SerializeObject(Clone.OrderList),
                    deserializeSettings
                ) ?? new();

            SelectList =
                JsonConvert.DeserializeObject<string[]>(
                    JsonConvert.SerializeObject(Clone.SelectList),
                    deserializeSettings
                ) ?? Array.Empty<string>();

            SkipString = Clone.SkipString?.Trim();
            TakeString = Clone.TakeString?.Trim();
            ConditionsAdded = Clone.ConditionsAdded;

            QueryCommandTimeout = Clone.QueryCommandTimeout;

            ParameterContext = Clone.ParameterContext.Clone();

            EagerLoads = Clone.EagerLoads.Clone();

        }

        public QueryBuilder(string query)
        {
            Query = new StringBuilder(query);
        }

        #region CRUD

        internal QueryBuilder<TModel> SimpleQuery()
        {
            string table = GetTableName();
            Query = new StringBuilder($"SELECT _selector_all FROM {table} ");

            return this;
        }

        internal QueryBuilder<TModel> InsertQuery<T>(T Item)
        {
            List<PropertyInfo> properties = GetFillableProperties();

            PropertyInfo? primaryKey = GetPropertyInfoByAttribute(Item!, typeof(PrimaryKey));

            if (primaryKey == null)
            {
                throw new ModelConfigurationException(
                    $"Primary key is not defined for model " +
                    $"'{typeof(TModel).Name}'. " +
                    $"Add the [PrimaryKey] attribute to the appropriate property."
                );
            }

            bool incrementing = IsIncrementing();

            if (incrementing)
            {
                EnsureGeneratedPrimaryKeyWritable(primaryKey);
            }


            var names = new List<string>();

            var values = new List<string>();

            foreach (var property in properties)
            {
                names.Add(
                    property.Name
                );

                values.Add(
                    $"@{property.Name}"
                );
            }

            /*
             * Si la PK NO es generada por la BD,
             * debemos incluirla en el INSERT.
             *
             * Evitamos duplicarla en caso de que
             * también haya sido marcada como [Fillable].
             */
            if (!incrementing && !names.Any(
                    x => string.Equals(
                        x,
                        primaryKey.Name,
                        StringComparison.OrdinalIgnoreCase
                    )
                ))
            {
                names.Add(
                    primaryKey.Name
                );

                values.Add(
                    $"@{primaryKey.Name}"
                );
            }

            string table =
                GetTableName();

            if (incrementing)
            {
                /*
                 * Utilizamos OUTPUT INSERTED para obtener
                 * exactamente la PK generada por este INSERT.
                 * sql_variant permite recibir PK de distintos
                 * tipos: int, bigint, Guid, string, etc.
                 */
                Query = new StringBuilder(
                        $"DECLARE @__dglib_inserted TABLE " +
                        $"([Value] sql_variant); " +

                        $"INSERT INTO {table} " +
                        $"({string.Join(",", names)}) " +

                        $"OUTPUT INSERTED.{primaryKey.Name} " +
                        $"INTO @__dglib_inserted ([Value]) " +

                        $"VALUES " +
                        $"({string.Join(",", values)}); " +

                        $"SELECT [Value] " +
                        $"FROM @__dglib_inserted;"
                    );
            }
            else
            {
                Query = new StringBuilder(
                        $"INSERT INTO {table} " +
                        $"({string.Join(",", names)}) " +
                        $"VALUES " +
                        $"({string.Join(",", values)})"
                    );
            }

            return this;
        }


        internal QueryBuilder<TModel> InsertDynamicQuery(IEnumerable<string> propertyNames)
        {
            List<string> names = propertyNames.ToList();

            if (names.Count == 0)
            {
                throw new QueryBuilderException("Create requires at least one property to insert.");
            }

            PropertyInfo? primaryKey = GetPropertyInfoByAttribute(typeof(PrimaryKey));

            if (primaryKey == null)
            {
                throw new ModelConfigurationException($"Primary key is not defined for model '{typeof(TModel).Name}'. Add the [PrimaryKey] attribute to the appropriate property.");
            }

            bool incrementing = IsIncrementing();

            if (incrementing)
            {
                EnsureGeneratedPrimaryKeyWritable(primaryKey);
            }

            if (incrementing && names.Any(name => string.Equals(name, primaryKey.Name, StringComparison.OrdinalIgnoreCase)))
            {
                throw new QueryBuilderException($"Primary key '{primaryKey.Name}' cannot be provided because model '{typeof(TModel).Name}' uses an incrementing primary key.");
            }

            if (!incrementing && !names.Any(name => string.Equals(name, primaryKey.Name, StringComparison.OrdinalIgnoreCase)))
            {
                throw new QueryBuilderException($"Primary key '{primaryKey.Name}' must be provided because model '{typeof(TModel).Name}' does not use an incrementing primary key.");
            }

            List<string> values = names.Select(name => $"@{name}").ToList();

            string table = GetTableName();

            if (incrementing)
            {
                Query = new StringBuilder($"DECLARE @__dglib_inserted TABLE ([Value] sql_variant); INSERT INTO {table} ({string.Join(",", names)}) OUTPUT INSERTED.{primaryKey.Name} INTO @__dglib_inserted ([Value]) VALUES ({string.Join(",", values)}); SELECT [Value] FROM @__dglib_inserted;");
            }
            else
            {
                Query = new StringBuilder($"INSERT INTO {table} ({string.Join(",", names)}) VALUES ({string.Join(",", values)})");
            }

            return this;
        }


        internal static int GetInsertManyChunkSize()
        {
            PropertyInfo? primaryKey = GetPropertyInfoByAttribute(typeof(PrimaryKey));

            if (primaryKey == null)
            {
                throw new ModelConfigurationException($"Primary key is not defined for model '{typeof(TModel).Name}'. Add the [PrimaryKey] attribute to the appropriate property.");
            }

            bool incrementing = IsIncrementing();
            List<PropertyInfo> properties = GetInsertManyProperties(primaryKey, incrementing);

            if (properties.Count == 0)
            {
                throw new ModelConfigurationException($"Model '{typeof(TModel).Name}' does not contain properties that can be inserted.");
            }

            const int parameterBudget = 2000;

            int rowsByParameters = parameterBudget / properties.Count;

            if (rowsByParameters <= 0)
            {
                throw new ModelConfigurationException($"Model '{typeof(TModel).Name}' contains too many insertable properties for a SQL Server command.");
            }

            return Math.Min(1000, rowsByParameters);
        }

        internal static BulkInsertCommand BuildInsertManyCommand(IReadOnlyList<TModel> items, bool returnGeneratedKeys)
        {
            if (items == null)
            {
                throw new ArgumentNullException(nameof(items));
            }

            if (items.Count == 0)
            {
                throw new ArgumentException("Bulk insert requires at least one item.", nameof(items));
            }

            if (items.Any(item => item is null))
            {
                throw new ArgumentException($"Bulk insert for model '{typeof(TModel).Name}' cannot contain null items.", nameof(items));
            }

            PropertyInfo? primaryKey = GetPropertyInfoByAttribute(typeof(PrimaryKey));

            if (primaryKey == null)
            {
                throw new ModelConfigurationException($"Primary key is not defined for model '{typeof(TModel).Name}'. Add the [PrimaryKey] attribute to the appropriate property.");
            }

            bool incrementing = IsIncrementing();
            bool returnsGeneratedKeys = incrementing && returnGeneratedKeys;

            if (returnsGeneratedKeys)
            {
                EnsureGeneratedPrimaryKeyWritable(primaryKey);
            }

            List<PropertyInfo> properties = GetInsertManyProperties(primaryKey, incrementing);

            if (properties.Count == 0)
            {
                throw new ModelConfigurationException($"Model '{typeof(TModel).Name}' does not contain properties that can be inserted.");
            }

            const int parameterBudget = 2000;
            int chunkSize = Math.Min(1000, parameterBudget / properties.Count);

            if (items.Count > chunkSize)
            {
                throw new ArgumentException($"Bulk insert for model '{typeof(TModel).Name}' exceeds the maximum chunk size of {chunkSize} rows for {properties.Count} parameters per row.", nameof(items));
            }

            string table = GetTableName();
            string[] columns = properties.Select(property => property.Name).ToArray();

            var parameters = new DynamicParameters();
            var rows = new List<string>(items.Count);

            for (int rowIndex = 0; rowIndex < items.Count; rowIndex++)
            {
                TModel item = items[rowIndex];
                var values = new List<string>(properties.Count + (incrementing ? 1 : 0));

                foreach (PropertyInfo property in properties)
                {
                    string parameterName = $"__dglib_{rowIndex}_{property.Name}";

                    parameters.Add(parameterName, property.GetValue(item));
                    values.Add($"@{parameterName}");
                }

                if (returnsGeneratedKeys)
                {
                    values.Add(rowIndex.ToString(System.Globalization.CultureInfo.InvariantCulture));
                }

                rows.Add($"({string.Join(",", values)})");
            }

            string sql;

            if (returnsGeneratedKeys)
            {
                string sourceColumns = string.Join(",", columns.Concat(new[] { "__dglib_index" }));
                string sourceValues = string.Join(",", columns.Select(column => $"source.{column}"));

                sql =
                    $"MERGE INTO {table} AS target " +
                    $"USING (VALUES {string.Join(",", rows)}) AS source ({sourceColumns}) " +
                    $"ON 1 = 0 " +
                    $"WHEN NOT MATCHED THEN " +
                    $"INSERT ({string.Join(",", columns)}) " +
                    $"VALUES ({sourceValues}) " +
                    $"OUTPUT source.__dglib_index AS [Index], INSERTED.{primaryKey.Name} AS [Value];";
            }
            else
            {
                sql =
                    $"INSERT INTO {table} " +
                    $"({string.Join(",", columns)}) " +
                    $"VALUES {string.Join(",", rows)};";
            }

            return new BulkInsertCommand(sql, parameters, returnsGeneratedKeys, items.Count);

        }

        internal static List<PropertyInfo> GetInsertManyProperties(PropertyInfo primaryKey, bool incrementing)
        {
            List<PropertyInfo> properties = GetFillableProperties();

            if (!incrementing && !properties.Any(property => string.Equals(property.Name, primaryKey.Name, StringComparison.OrdinalIgnoreCase)))
            {
                properties.Add(primaryKey);
            }

            return properties;
        }

        public void Update(dynamic args)
        {
            object parameters = (object)args;

            BuildDynamicUpdateQuery(parameters);

            ExecuteCommand(ToParameterizedSql(), parameters);
        }

        public Task<int> UpdateAsync(dynamic args)
        {
            return UpdateAsyncCore((object)args, CancellationToken.None);
        }

        public Task<int> UpdateAsync(dynamic args, CancellationToken cancellationToken)
        {
            return UpdateAsyncCore((object)args, cancellationToken);
        }

        private async Task<int> UpdateAsyncCore(object args, CancellationToken cancellationToken)
        {
            BuildDynamicUpdateQuery(args);

            return await ExecuteCommandAsync(ToParameterizedSql(), args, cancellationToken).ConfigureAwait(false);
        }

        private void BuildDynamicUpdateQuery(object args)
        {
            if (args == null)
            {
                throw new ArgumentNullException(nameof(args));
            }

            PropertyInfo[] properties = args.GetType().GetProperties();

            if (properties.Length == 0)
            {
                throw new QueryBuilderException("Update requires at least one property to update.");
            }

            string[] values = properties.Select(property => $"{property.Name} = @{property.Name}").ToArray();

            string table = GetTableName();
            string currentQuery = Query.ToString();

            var regex = new Regex(Regex.Escape("FROM"));
            var match = regex.Match(currentQuery);

            string suffix = match.Success ? currentQuery.Substring(match.Index) : "";

            Query = new StringBuilder($"UPDATE {table} SET {string.Join(",", values)} {suffix}");
        }

        internal QueryBuilder<TModel> UpdateQuery<T>(T Item)
        {

            List<PropertyInfo> Properties = GetFillableProperties();

            object[] Values = new object[Properties.Count];

            foreach (var (property, index) in Properties.Select((property, index) => (property, index)))
            {

                string PropertyName = property.Name;
                object PropertyValue = property.GetValue(Item, null) ?? (new());

                Values[index] = $"{PropertyName} = @{PropertyName}";
            }

            string table = GetTableName();
            string? primaryKey = GetPrimaryKey();

            if (primaryKey == null)
            {
                throw new ModelConfigurationException(
                    $"Primary key is not defined for model " +
                    $"'{typeof(TModel).Name}'. " +
                    $"Add the [PrimaryKey] attribute to the appropriate property."
                );
            }

            Query = new StringBuilder($"UPDATE {table} SET {String.Join(",", Values)} WHERE {primaryKey} = @{primaryKey}");

            return this;
        }

        internal QueryBuilder<TModel> UpdateDynamicQuery<T>(dynamic args)
        {

            var Properties = args.GetType().GetProperties();

            object[] Values = new object[Properties.Length];
            int index = 0;
            foreach (var property in Properties)
            {
                var PropertyName = property.Name;
                Values[index] = $"{PropertyName} = @{PropertyName}";

                index++;
            }

            string table = GetTableName();
            string? primaryKey = GetPrimaryKey();

            if (primaryKey == null)
            {
                throw new ModelConfigurationException(
                    $"Primary key is not defined for model " +
                    $"'{typeof(T).Name}'. " +
                    $"Add the [PrimaryKey] attribute to the appropriate property."
                );
            }

            Query = new StringBuilder($"UPDATE {table} SET {String.Join(",", Values)} WHERE {primaryKey} = @{primaryKey}");

            return this;
        }

        internal void SimpleDelete<T>(T Item)
        {
            string table = GetTableName();
            string? primaryKey = GetPrimaryKey();

            if (primaryKey == null)
            {
                throw new ModelConfigurationException(
                   $"Primary key is not defined for model " +
                   $"'{typeof(T).Name}'. " +
                   $"Add the [PrimaryKey] attribute to the appropriate property."
               );
            }

            Query = new StringBuilder($"{Clauses.DELETE} FROM {table} WHERE {primaryKey} = @{primaryKey}");

            ExecuteCommand(
                 ToParameterizedSql(),
                 Item
             );

        }

        internal Task<int> SimpleDeleteAsync<T>(T item)
        {
            return SimpleDeleteAsync(item, CancellationToken.None);
        }

        internal async Task<int> SimpleDeleteAsync<T>(T item, CancellationToken cancellationToken)
        {
            string table =
                GetTableName();

            string? primaryKey =
                GetPrimaryKey();

            if (primaryKey == null)
            {
                throw new ModelConfigurationException(
                    $"Primary key is not defined for model " +
                    $"'{typeof(T).Name}'. " +
                    $"Add the [PrimaryKey] attribute to the appropriate property."
                );
            }

            Query =
                new StringBuilder(
                    $"{Clauses.DELETE} FROM {table} " +
                    $"WHERE {primaryKey} = @{primaryKey}"
                );

            return await ExecuteCommandAsync(
                ToParameterizedSql(),
                item,
                cancellationToken
            )
            .ConfigureAwait(false);
        }

        internal void Truncate()
        {
            string table =
                GetTableName();

            Query =
                new StringBuilder(
                    $"{Clauses.TRUNCATE} TABLE {table}"
                );

            ExecuteCommand(
                ToParameterizedSql()
            );
        }

        internal Task<int> TruncateAsync()
        {
            return TruncateAsync(
                CancellationToken.None
            );
        }

        internal async Task<int> TruncateAsync(
            CancellationToken cancellationToken)
        {
            string table =
                GetTableName();

            Query =
                new StringBuilder(
                    $"{Clauses.TRUNCATE} TABLE {table}"
                );

            return await ExecuteCommandAsync(
                ToParameterizedSql(),
                cancellationToken:
                    cancellationToken
            )
            .ConfigureAwait(false);
        }

        public void Delete()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            var regex = new Regex(Regex.Escape("FROM"));
            var match = regex.Match(Query.ToString());

            string replaced = string.Concat(" DELETE ", Query.ToString().AsSpan(match.Index));

            Query = new(replaced);

            ExecuteCommand(
                ToParameterizedSql()
            );

        }

        public Task<int> DeleteAsync()
        {
            return DeleteAsync(
                CancellationToken.None
            );
        }

        public async Task<int> DeleteAsync(CancellationToken cancellationToken)
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            var regex =
                new Regex(
                    Regex.Escape("FROM")
                );

            var match =
                regex.Match(
                    Query.ToString()
                );

            string replaced =
                string.Concat(
                    " DELETE ",
                    Query.ToString().AsSpan(match.Index)
                );

            Query =
                new StringBuilder(replaced);

            return await ExecuteCommandAsync(
                ToParameterizedSql(),
                cancellationToken:
                    cancellationToken
            )
            .ConfigureAwait(false);
        }

        #endregion


        public QueryBuilder<TModel> With(string relationship)
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            EagerLoads.Add(typeof(TModel), relationship);

            return this;
        }

        public QueryBuilder<TModel> With(params string[] relationships)
        {
            if (relationships == null)
            {
                throw new ArgumentNullException(nameof(relationships));
            }

            if (relationships.Length == 0)
            {
                throw new ArgumentException("With requires at least one relationship.", nameof(relationships));
            }

            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            foreach (string relationship in relationships)
            {
                EagerLoads.Add(typeof(TModel), relationship);
            }

            return this;
        }

        public QueryBuilder<TModel> With<TRelated>(Expression<Func<TModel, IEnumerable<TRelated>>> relationship) where TRelated : Model<TRelated>, new()
        {
            PropertyInfo property = RelationshipExpression.GetProperty(relationship, typeof(TModel));
            RelationshipDefinition definition = RelationshipMetadataCache.GetRequired(typeof(TModel), property.Name);

            if (definition.Kind != RelationshipKind.HasMany)
            {
                throw new RelationshipException($"Relationship '{definition.Name}' on model '{typeof(TModel).Name}' is configured as '{definition.Kind}' and cannot be used as a collection eager-loading relationship.");
            }

            if (definition.RelatedType != typeof(TRelated))
            {
                throw new RelationshipException($"Relationship '{definition.Name}' on model '{typeof(TModel).Name}' points to '{definition.RelatedType.Name}', but With was requested with '{typeof(TRelated).Name}'.");
            }

            return With(property.Name);
        }

        public QueryBuilder<TModel> With<TRelated>(Expression<Func<TModel, IEnumerable<TRelated>>> relationship, Action<EagerLoadBuilder<TRelated>> constraint) where TRelated : Model<TRelated>, new()
        {
            PropertyInfo property = RelationshipExpression.GetProperty(relationship, typeof(TModel));
            RelationshipDefinition definition = RelationshipMetadataCache.GetRequired(typeof(TModel), property.Name);

            if (definition.Kind != RelationshipKind.HasMany)
            {
                throw new RelationshipException($"Relationship '{definition.Name}' on model '{typeof(TModel).Name}' is configured as '{definition.Kind}' and cannot be used as a collection eager-loading relationship.");
            }

            if (definition.RelatedType != typeof(TRelated))
            {
                throw new RelationshipException($"Relationship '{definition.Name}' on model '{typeof(TModel).Name}' points to '{definition.RelatedType.Name}', but With was requested with '{typeof(TRelated).Name}'.");
            }

            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            EagerLoads.Add(typeof(TModel), property.Name, constraint);

            return this;
        }

        public QueryBuilder<TModel> With<TRelated>(Expression<Func<TModel, TRelated?>> relationship) where TRelated : Model<TRelated>, new()
        {
            PropertyInfo property = RelationshipExpression.GetProperty(relationship, typeof(TModel));
            RelationshipDefinition definition = RelationshipMetadataCache.GetRequired(typeof(TModel), property.Name);

            if (definition.Kind == RelationshipKind.HasMany)
            {
                throw new RelationshipException($"Relationship '{definition.Name}' on model '{typeof(TModel).Name}' is configured as 'HasMany' and must be used as a collection eager-loading relationship.");
            }

            if (definition.RelatedType != typeof(TRelated))
            {
                throw new RelationshipException($"Relationship '{definition.Name}' on model '{typeof(TModel).Name}' points to '{definition.RelatedType.Name}', but With was requested with '{typeof(TRelated).Name}'.");
            }

            return With(property.Name);
        }

        public QueryBuilder<TModel> With<TRelated>(Expression<Func<TModel, TRelated?>> relationship, Action<EagerLoadBuilder<TRelated>> constraint) where TRelated : Model<TRelated>, new()
        {
            PropertyInfo property = RelationshipExpression.GetProperty(relationship, typeof(TModel));
            RelationshipDefinition definition = RelationshipMetadataCache.GetRequired(typeof(TModel), property.Name);

            if (definition.Kind == RelationshipKind.HasMany)
            {
                throw new RelationshipException($"Relationship '{definition.Name}' on model '{typeof(TModel).Name}' is configured as 'HasMany' and must be used as a collection eager-loading relationship.");
            }

            if (definition.RelatedType != typeof(TRelated))
            {
                throw new RelationshipException($"Relationship '{definition.Name}' on model '{typeof(TModel).Name}' points to '{definition.RelatedType.Name}', but With was requested with '{typeof(TRelated).Name}'.");
            }

            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            EagerLoads.Add(typeof(TModel), property.Name, constraint);

            return this;
        }

        public QueryBuilder<TModel> With(params Expression<Func<TModel, object?>>[] relationships)
        {
            if (relationships == null)
            {
                throw new ArgumentNullException(nameof(relationships));
            }

            if (relationships.Length == 0)
            {
                throw new ArgumentException("With requires at least one relationship.", nameof(relationships));
            }

            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            foreach (Expression<Func<TModel, object?>> relationship in relationships)
            {
                PropertyInfo property = RelationshipExpression.GetProperty(relationship, typeof(TModel));
                EagerLoads.Add(typeof(TModel), property.Name);
            }

            return this;
        }

        public QueryBuilder<TModel> With<TRelated>(string relationship, Action<EagerLoadBuilder<TRelated>> constraint) where TRelated : Model<TRelated>, new()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            EagerLoads.Add(typeof(TModel), relationship, constraint);

            return this;
        }


        #region Retrieving

        public TModel First()
        {
            if (!CheckQueryInit()) SimpleQuery();

            TModel item = QueryFirst<TModel>(ToParameterizedSql());

            EagerLoader.Load(new[] { item }, EagerLoads, QueryCommandTimeout);

            return item;
        }

        public TResult First<TResult>()
        {
            EnsureNoEagerLoadsForProjection(nameof(First));

            if (!CheckQueryInit()) SimpleQuery();

            return QueryFirst<TResult>(ToParameterizedSql());
        }

        public Task<TModel> FirstAsync()
        {
            return FirstAsync(
                CancellationToken.None
            );
        }

        public async Task<TModel> FirstAsync(CancellationToken cancellationToken)
        {
            if (!CheckQueryInit()) SimpleQuery();

            TModel item = await QueryFirstAsync<TModel>(ToParameterizedSql(), cancellationToken: cancellationToken).ConfigureAwait(false);

            await EagerLoader.LoadAsync(new[] { item }, EagerLoads, QueryCommandTimeout, cancellationToken).ConfigureAwait(false);

            return item;
        }

        public Task<TResult> FirstAsync<TResult>()
        {
            return FirstAsync<TResult>(
                CancellationToken.None
            );
        }

        public Task<TResult> FirstAsync<TResult>(CancellationToken cancellationToken)
        {
            EnsureNoEagerLoadsForProjection(nameof(FirstAsync));

            if (!CheckQueryInit()) SimpleQuery();

            return QueryFirstAsync<TResult>(ToParameterizedSql(), cancellationToken: cancellationToken);
        }


        public TModel? FirstOrDefault()
        {
            if (!CheckQueryInit()) SimpleQuery();

            TModel? item = QueryFirstOrDefault<TModel>(ToParameterizedSql());

            if (item == null) return item;

            EagerLoader.Load(new[] { item }, EagerLoads, QueryCommandTimeout);

            return item;
        }

        public TResult? FirstOrDefault<TResult>()
        {
            EnsureNoEagerLoadsForProjection(nameof(FirstOrDefault));

            if (!CheckQueryInit()) SimpleQuery();

            return QueryFirstOrDefault<TResult>(ToParameterizedSql());
        }

        public Task<TModel?> FirstOrDefaultAsync()
        {
            return FirstOrDefaultAsync(
                CancellationToken.None
            );
        }

        public async Task<TModel?> FirstOrDefaultAsync(CancellationToken cancellationToken)
        {
            if (!CheckQueryInit()) SimpleQuery();

            TModel? item = await QueryFirstOrDefaultAsync<TModel>(ToParameterizedSql(), cancellationToken: cancellationToken).ConfigureAwait(false);

            if (item == null) return item;

            await EagerLoader.LoadAsync(new[] { item }, EagerLoads, QueryCommandTimeout, cancellationToken).ConfigureAwait(false);

            return item;
        }

        public Task<TResult?> FirstOrDefaultAsync<TResult>()
        {
            return FirstOrDefaultAsync<TResult>(
                CancellationToken.None
            );
        }

        public Task<TResult?> FirstOrDefaultAsync<TResult>(CancellationToken cancellationToken)
        {
            EnsureNoEagerLoadsForProjection(nameof(FirstOrDefaultAsync));

            if (!CheckQueryInit()) SimpleQuery();

            return QueryFirstOrDefaultAsync<TResult>(ToParameterizedSql(), cancellationToken: cancellationToken);
        }


        /*
         * ============================================================
         * SINGLE
         * ============================================================
         */

        public TModel Single()
        {
            if (!CheckQueryInit()) SimpleQuery();

            TModel item = QuerySingle<TModel>(ToParameterizedSql());

            EagerLoader.Load(new[] { item }, EagerLoads, QueryCommandTimeout);

            return item;
        }


        public TResult Single<TResult>()
        {
            EnsureNoEagerLoadsForProjection(nameof(Single));

            if (!CheckQueryInit()) SimpleQuery();

            return QuerySingle<TResult>(ToParameterizedSql());
        }


        public Task<TModel> SingleAsync()
        {
            return SingleAsync(
                CancellationToken.None
            );
        }


        public async Task<TModel> SingleAsync(CancellationToken cancellationToken)
        {
            if (!CheckQueryInit()) SimpleQuery();

            TModel item = await QuerySingleAsync<TModel>(ToParameterizedSql(), cancellationToken: cancellationToken).ConfigureAwait(false);

            await EagerLoader.LoadAsync(new[] { item }, EagerLoads, QueryCommandTimeout, cancellationToken).ConfigureAwait(false);

            return item;
        }


        public Task<TResult> SingleAsync<TResult>()
        {
            return SingleAsync<TResult>(
                CancellationToken.None
            );
        }


        public Task<TResult> SingleAsync<TResult>(CancellationToken cancellationToken)
        {
            EnsureNoEagerLoadsForProjection(nameof(SingleAsync));

            if (!CheckQueryInit()) SimpleQuery();

            return QuerySingleAsync<TResult>(ToParameterizedSql(), cancellationToken: cancellationToken);
        }

        /*
         * ============================================================
         * SINGLE OR DEFAULT
         * ============================================================
         */

        public TModel? SingleOrDefault()
        {
            if (!CheckQueryInit()) SimpleQuery();

            TModel? item = QuerySingleOrDefault<TModel>(ToParameterizedSql());

            if (item == null) return item;

            EagerLoader.Load(new[] { item }, EagerLoads, QueryCommandTimeout);

            return item;
        }


        public TResult? SingleOrDefault<TResult>()
        {
            EnsureNoEagerLoadsForProjection(nameof(SingleOrDefault));

            if (!CheckQueryInit()) SimpleQuery();

            return QuerySingleOrDefault<TResult>(ToParameterizedSql());
        }


        public Task<TModel?> SingleOrDefaultAsync()
        {
            return SingleOrDefaultAsync(
                CancellationToken.None
            );
        }


        public async Task<TModel?> SingleOrDefaultAsync(CancellationToken cancellationToken)
        {
            if (!CheckQueryInit()) SimpleQuery();

            TModel? item = await QuerySingleOrDefaultAsync<TModel>(ToParameterizedSql(), cancellationToken: cancellationToken).ConfigureAwait(false);

            if (item == null) return item;

            await EagerLoader.LoadAsync(new[] { item }, EagerLoads, QueryCommandTimeout, cancellationToken).ConfigureAwait(false);

            return item;
        }


        public Task<TResult?> SingleOrDefaultAsync<TResult>()
        {
            return SingleOrDefaultAsync<TResult>(
                CancellationToken.None
            );
        }


        public Task<TResult?> SingleOrDefaultAsync<TResult>(CancellationToken cancellationToken)
        {
            EnsureNoEagerLoadsForProjection(nameof(SingleOrDefaultAsync));

            if (!CheckQueryInit()) SimpleQuery();

            return QuerySingleOrDefaultAsync<TResult>(ToParameterizedSql(), cancellationToken: cancellationToken);
        }


        public TModel Find<TKey>(TKey id)
        {
            TModel? item = FindOrDefault(id);

            if (item == null)
            {
                throw new ModelNotFoundException(typeof(TModel), id!);
            }

            return item;
        }

        public TModel? FindOrDefault<TKey>(TKey id)
        {
            string primaryKey = GetRequiredPrimaryKeyNameForFind();
            QueryBuilder<TModel> builder = Clone();

            builder.Where(primaryKey, id);

            return builder.FirstOrDefault();
        }

        public Task<TModel> FindAsync<TKey>(TKey id)
        {
            return FindAsync(id, CancellationToken.None);
        }

        public async Task<TModel> FindAsync<TKey>(TKey id, CancellationToken cancellationToken)
        {
            TModel? item = await FindOrDefaultAsync(id, cancellationToken).ConfigureAwait(false);

            if (item == null)
            {
                throw new ModelNotFoundException(typeof(TModel), id!);
            }

            return item;
        }

        public Task<TModel?> FindOrDefaultAsync<TKey>(TKey id)
        {
            return FindOrDefaultAsync(id, CancellationToken.None);
        }

        public async Task<TModel?> FindOrDefaultAsync<TKey>(TKey id, CancellationToken cancellationToken)
        {
            string primaryKey = GetRequiredPrimaryKeyNameForFind();
            QueryBuilder<TModel> builder = Clone();

            builder.Where(primaryKey, id);

            return await builder.FirstOrDefaultAsync(cancellationToken).ConfigureAwait(false);
        }


        public List<TModel> ToList()
        {
            List<TModel> items = QueryList<TModel>(ToParameterizedSql());

            EagerLoader.Load(items, EagerLoads, QueryCommandTimeout);

            return items;
        }

        public List<TResult> ToList<TResult>()
        {
            EnsureNoEagerLoadsForProjection(nameof(ToList));

            return QueryList<TResult>(ToParameterizedSql());
        }

        public Task<List<TModel>> ToListAsync()
        {
            return ToListAsync(
                CancellationToken.None
            );
        }

        public async Task<List<TModel>> ToListAsync(CancellationToken cancellationToken)
        {
            List<TModel> items = await QueryListAsync<TModel>(ToParameterizedSql(), cancellationToken: cancellationToken).ConfigureAwait(false);

            await EagerLoader.LoadAsync(items, EagerLoads, QueryCommandTimeout, cancellationToken).ConfigureAwait(false);

            return items;
        }

        public Task<List<TResult>> ToListAsync<TResult>()
        {
            return ToListAsync<TResult>(
                CancellationToken.None
            );
        }

        public Task<List<TResult>> ToListAsync<TResult>(CancellationToken cancellationToken)
        {
            EnsureNoEagerLoadsForProjection(nameof(ToListAsync));

            return QueryListAsync<TResult>(ToParameterizedSql(), cancellationToken: cancellationToken);
        }

        public bool Exists()
        {
            EnsureNoEagerLoadsForProjection(nameof(Exists));

            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            string sql =
                ToParameterizedSql();

            int? result =
                ExecuteScalar<int?>(
                    $"SELECT 1 " +
                    $"WHERE {Clauses.EXISTS} ({sql})"
                );

            return result.HasValue;
        }

        public Task<bool> ExistsAsync()
        {
            return ExistsAsync(
                CancellationToken.None
            );
        }

        public async Task<bool> ExistsAsync(CancellationToken cancellationToken)
        {
            EnsureNoEagerLoadsForProjection(nameof(ExistsAsync));

            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            string sql =
                ToParameterizedSql();

            int? result =
                await ExecuteScalarAsync<int?>(
                    $"SELECT 1 " +
                    $"WHERE {Clauses.EXISTS} ({sql})",
                    cancellationToken:
                        cancellationToken
                )
                .ConfigureAwait(false);

            return result.HasValue;
        }

        public bool DoesntExist()
        {
            EnsureNoEagerLoadsForProjection(nameof(DoesntExist));

            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            string sql =
                ToParameterizedSql();

            int? result =
                ExecuteScalar<int?>(
                    $"SELECT 1 " +
                    $"WHERE {LogicalOperators.NOT} " +
                    $"{Clauses.EXISTS} ({sql})"
                );

            return result.HasValue;
        }

        public Task<bool> DoesntExistAsync()
        {
            return DoesntExistAsync(
                CancellationToken.None
            );
        }

        public async Task<bool> DoesntExistAsync(CancellationToken cancellationToken)
        {
            EnsureNoEagerLoadsForProjection(nameof(DoesntExistAsync));

            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            string sql =
                ToParameterizedSql();

            int? result =
                await ExecuteScalarAsync<int?>(
                    $"SELECT 1 " +
                    $"WHERE {LogicalOperators.NOT} " +
                    $"{Clauses.EXISTS} ({sql})",
                    cancellationToken:
                        cancellationToken
                )
                .ConfigureAwait(false);

            return result.HasValue;
        }

        public int Count()
        {
            EnsureNoEagerLoadsForProjection(nameof(Count));

            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return ExecuteScalar<int>(
                SqlAggregate(
                    "count(*) as CountColumn"
                )
            );
        }

        public Task<int> CountAsync()
        {
            return CountAsync(
                CancellationToken.None
            );
        }

        public Task<int> CountAsync(CancellationToken cancellationToken)
        {
            EnsureNoEagerLoadsForProjection(nameof(CountAsync));

            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return ExecuteScalarAsync<int>(
                SqlAggregate(
                    "count(*) as CountColumn"
                ),
                cancellationToken:
                    cancellationToken
            );
        }

        public string Value(string Column)
        {
            EnsureNoEagerLoadsForProjection(nameof(Value));

            Column =
                ValidateColumn(
                    Column,
                    nameof(Value)
                );

            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QueryFirst<string>(
                SqlAggregate(
                    $"{Column} as ValueColumn"
                )
            );
        }

        public TValue Value<TValue>(string Column)
        {
            EnsureNoEagerLoadsForProjection(nameof(Value));

            Column =
                ValidateColumn(
                    Column,
                    nameof(Value)
                );

            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QueryFirst<TValue>(
                SqlAggregate(
                    $"{Column} as ValueColumn"
                )
            );
        }

        public Task<string> ValueAsync(string Column)
        {
            return ValueAsync(
                Column,
                CancellationToken.None
            );
        }

        public Task<string> ValueAsync(string Column, CancellationToken cancellationToken)
        {
            EnsureNoEagerLoadsForProjection(nameof(ValueAsync));

            Column =
                ValidateColumn(
                    Column,
                    nameof(ValueAsync)
                );

            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QueryFirstAsync<string>(
                SqlAggregate(
                    $"{Column} as ValueColumn"
                ),
                cancellationToken:
                    cancellationToken
            );
        }

        public Task<TValue> ValueAsync<TValue>(string Column)
        {
            return ValueAsync<TValue>(
                Column,
                CancellationToken.None
            );
        }

        public Task<TValue> ValueAsync<TValue>(string Column, CancellationToken cancellationToken)
        {
            EnsureNoEagerLoadsForProjection(nameof(ValueAsync));

            Column =
                ValidateColumn(
                    Column,
                    nameof(ValueAsync)
                );

            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QueryFirstAsync<TValue>(
                SqlAggregate(
                    $"{Column} as ValueColumn"
                ),
                cancellationToken:
                    cancellationToken
            );
        }

        public List<TValue> Pluck<TValue>(string Column)
        {
            EnsureNoEagerLoadsForProjection(nameof(Pluck));

            Column =
                ValidateColumn(
                    Column,
                    nameof(Pluck)
                );

            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QueryList<TValue>(
                SqlAggregate(
                    Column
                )
            );
        }

        public Task<List<TValue>> PluckAsync<TValue>(string Column)
        {
            return PluckAsync<TValue>(
                Column,
                CancellationToken.None
            );
        }

        public Task<List<TValue>> PluckAsync<TValue>(string Column, CancellationToken cancellationToken)
        {
            EnsureNoEagerLoadsForProjection(nameof(PluckAsync));

            Column =
                ValidateColumn(
                    Column,
                    nameof(PluckAsync)
                );

            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QueryListAsync<TValue>(
                SqlAggregate(
                    Column
                ),
                cancellationToken:
                    cancellationToken
            );
        }


        public double Max(string Column)
        {
            EnsureNoEagerLoadsForProjection(nameof(Max));

            Column =
                ValidateColumn(
                    Column,
                    nameof(Max)
                );

            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QueryFirst<double>(
                SqlAggregate(
                    $"max({Column})"
                )
            );
        }

        public double Min(string Column)
        {
            EnsureNoEagerLoadsForProjection(nameof(Min));

            Column =
                ValidateColumn(
                    Column,
                    nameof(Min)
                );

            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QueryFirst<double>(
                SqlAggregate(
                    $"min({Column})"
                )
            );
        }

        public double Avg(string Column)
        {
            EnsureNoEagerLoadsForProjection(nameof(Avg));

            Column =
                ValidateColumn(
                    Column,
                    nameof(Avg)
                );

            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QueryFirst<double>(
                SqlAggregate(
                    $"avg({Column})"
                )
            );
        }

        public double Sum(string Column)
        {
            EnsureNoEagerLoadsForProjection(nameof(Sum));

            Column =
                ValidateColumn(
                    Column,
                    nameof(Sum)
                );

            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QueryFirst<double>(
                SqlAggregate(
                    $"sum({Column})"
                )
            );
        }

        public Task<double> MaxAsync(string Column)
        {
            return MaxAsync(
                Column,
                CancellationToken.None
            );
        }

        public Task<double> MaxAsync(string Column, CancellationToken cancellationToken)
        {
            EnsureNoEagerLoadsForProjection(nameof(MaxAsync));

            Column =
                ValidateColumn(
                    Column,
                    nameof(MaxAsync)
                );

            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QueryFirstAsync<double>(
                SqlAggregate(
                    $"max({Column})"
                ),
                cancellationToken:
                    cancellationToken
            );
        }

        public Task<double> MinAsync(string Column)
        {
            return MinAsync(
                Column,
                CancellationToken.None
            );
        }

        public Task<double> MinAsync(string Column, CancellationToken cancellationToken)
        {
            EnsureNoEagerLoadsForProjection(nameof(MinAsync));

            Column =
                ValidateColumn(
                    Column,
                    nameof(MinAsync)
                );

            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QueryFirstAsync<double>(
                SqlAggregate(
                    $"min({Column})"
                ),
                cancellationToken:
                    cancellationToken
            );
        }

        public Task<double> AvgAsync(
            string Column)
        {
            return AvgAsync(
                Column,
                CancellationToken.None
            );
        }

        public Task<double> AvgAsync(string Column, CancellationToken cancellationToken)
        {
            EnsureNoEagerLoadsForProjection(nameof(AvgAsync));

            Column =
                ValidateColumn(
                    Column,
                    nameof(AvgAsync)
                );

            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QueryFirstAsync<double>(
                SqlAggregate(
                    $"avg({Column})"
                ),
                cancellationToken:
                    cancellationToken
            );
        }

        public Task<double> SumAsync(string Column)
        {
            return SumAsync(
                Column,
                CancellationToken.None
            );
        }

        public Task<double> SumAsync(string Column, CancellationToken cancellationToken)
        {
            EnsureNoEagerLoadsForProjection(nameof(SumAsync));

            Column =
                ValidateColumn(
                    Column,
                    nameof(SumAsync)
                );

            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QueryFirstAsync<double>(
                SqlAggregate(
                    $"sum({Column})"
                ),
                cancellationToken:
                    cancellationToken
            );
        }


        #endregion

        public QueryBuilder<TModel> Select(params string[] Columns)
        {
            SelectList =
                ValidateColumns(
                    Columns,
                    nameof(Select)
                );

            return this;
        }

        public QueryBuilder<TModel> Skip(int Rows)
        {

            if (!HasOrderClause())
            {
                AddOrderClause(" (SELECT NULL) ");
            }

            if (SkipString == null)
            {
                SkipString = $" {Clauses.OFFSET} {Rows} ROWS ";
                Query.Append(" skip_string ");
            }

            return this;
        }

        public QueryBuilder<TModel> Take(int Rows)
        {

            if (!HasOrderClause())
            {
                AddOrderClause(" (SELECT NULL) ");
            }

            if (SkipString == null)
            {
                SkipString = $" {Clauses.OFFSET} 0 ROWS ";
                Query.Append(" skip_string ");
            }

            if (TakeString == null)
            {
                TakeString = $" FETCH NEXT {Rows} ROWS ONLY ";
                Query.Append(" take_string ");
            }

            return this;
        }

        public QueryBuilder<TModel> WhereRaw(string Query)
        {
            AddRaw(Query, LogicalOperators.AND);

            return this;
        }

        public QueryBuilder<TModel> OrWhereRaw(string Query)
        {
            AddRaw(Query, LogicalOperators.OR);

            return this;
        }

        public QueryBuilder<TModel> Where(string Column, object? Value)
        {
            InitWhere(Column, Value);
            return this;
        }

        public QueryBuilder<TModel> Where(string Column, string ComparisonOperator, object? Value)
        {
            InitWhere(Column, Value, ComparisonOperator);
            return this;
        }

        public QueryBuilder<TModel> Where(Func<SubQuery<TModel>, SubQuery<TModel>> Builder)
        {
            GroupCondition(Builder, LogicalOperators.AND);
            return this;
        }

        public QueryBuilder<TModel> Where<TValue>(Expression<Func<TModel, TValue>> column, TValue value)
        {
            return Where(ModelPropertyExpression.GetName(column, nameof(Where)), value);
        }

        public QueryBuilder<TModel> Where<TValue>(Expression<Func<TModel, TValue>> column, string comparisonOperator, TValue value)
        {
            return Where(ModelPropertyExpression.GetName(column, nameof(Where)), comparisonOperator, value);
        }


        public QueryBuilder<TModel> WhereLike(string Column, string Pattern)
        {
            if (string.IsNullOrWhiteSpace(Column))
            {
                throw new QueryBuilderException(
                    "WhereLike requires a valid column name."
                );
            }

            if (Pattern == null)
            {
                throw new QueryBuilderException(
                    $"WhereLike('{Column}') cannot receive a null pattern."
                );
            }

            InitWhere(
                Column,
                Pattern,
                "LIKE"
            );

            return this;
        }

        public QueryBuilder<TModel> WhereLike(Expression<Func<TModel, string?>> column, string pattern)
        {
            return WhereLike(ModelPropertyExpression.GetName(column, nameof(WhereLike)), pattern);
        }

        public QueryBuilder<TModel> WhereContains(string Column, string Value)
        {
            if (string.IsNullOrWhiteSpace(Column))
            {
                throw new QueryBuilderException(
                    "WhereContains requires a valid column name."
                );
            }

            if (Value == null)
            {
                throw new QueryBuilderException(
                    $"WhereContains('{Column}') cannot receive a null value."
                );
            }

            string pattern =
                $"%{EscapeLikePattern(Value)}%";

            return WhereLike(
                Column,
                pattern
            );
        }

        public QueryBuilder<TModel> WhereContains(Expression<Func<TModel, string?>> column, string value)
        {
            return WhereContains(ModelPropertyExpression.GetName(column, nameof(WhereContains)), value);
        }


        public QueryBuilder<TModel> OrWhere(Func<SubQuery<TModel>, SubQuery<TModel>> Builder)
        {
            GroupCondition(Builder, LogicalOperators.OR);
            return this;
        }

        public QueryBuilder<TModel> OrWhere(string Column, object? Value)
        {
            InitWhere(Column, Value, null, LogicalOperators.OR);
            return this;
        }

        public QueryBuilder<TModel> OrWhere(string Column, string ComparisonOperator, object? Value)
        {
            InitWhere(Column, Value, ComparisonOperator, LogicalOperators.OR);
            return this;
        }

        public QueryBuilder<TModel> OrWhere<TValue>(Expression<Func<TModel, TValue>> column, TValue value)
        {
            return OrWhere(ModelPropertyExpression.GetName(column, nameof(OrWhere)), value);
        }

        public QueryBuilder<TModel> OrWhere<TValue>(Expression<Func<TModel, TValue>> column, string comparisonOperator, TValue value)
        {
            return OrWhere(ModelPropertyExpression.GetName(column, nameof(OrWhere)), comparisonOperator, value);
        }


        public QueryBuilder<TModel> WhereNot(Func<SubQuery<TModel>, SubQuery<TModel>> Builder)
        {
            GroupCondition(Builder, LogicalOperators.AND, true);
            return this;
        }

        public QueryBuilder<TModel> OrWhereNot(Func<SubQuery<TModel>, SubQuery<TModel>> Builder)
        {
            GroupCondition(Builder, LogicalOperators.OR, true);
            return this;
        }


        public QueryBuilder<TModel> WhereIn(string Column, object[] Values)
        {
            return WhereIn<object>(
                Column,
                Values
            );
        }

        public QueryBuilder<TModel> WhereIn<TValue>(string Column, IEnumerable<TValue> Values)
        {
            InitWhereIn(
                Column,
                Values,
                LogicalOperators.IN,
                nameof(WhereIn),
                rejectNullValues: true
            );

            return this;
        }

        public QueryBuilder<TModel> WhereIn<TValue>(Expression<Func<TModel, TValue>> column, IEnumerable<TValue> values)
        {
            return WhereIn(ModelPropertyExpression.GetName(column, nameof(WhereIn)), values);
        }

        public QueryBuilder<TModel> WhereNotIn(string Column, object[] Values)
        {
            return WhereNotIn<object>(
                Column,
                Values
            );
        }

        public QueryBuilder<TModel> WhereNotIn<TValue>(string Column, IEnumerable<TValue> Values)
        {
            InitWhereIn(
                Column,
                Values,
                LogicalOperators.NOT_IN,
                nameof(WhereNotIn),
                rejectNullValues: true
            );

            return this;
        }

        public QueryBuilder<TModel> WhereNotIn<TValue>(Expression<Func<TModel, TValue>> column, IEnumerable<TValue> values)
        {
            return WhereNotIn(ModelPropertyExpression.GetName(column, nameof(WhereNotIn)), values);
        }

        public QueryBuilder<TModel> WhereNull(string Column)
        {
            InitWhere(Column, null);
            return this;
        }

        public QueryBuilder<TModel> WhereNotNull(string Column)
        {
            InitWhere(Column, null, null, LogicalOperators.NOT);
            return this;
        }

        public QueryBuilder<TModel> WhereNull<TValue>(Expression<Func<TModel, TValue>> column)
        {
            return WhereNull(ModelPropertyExpression.GetName(column, nameof(WhereNull)));
        }

        public QueryBuilder<TModel> WhereNotNull<TValue>(Expression<Func<TModel, TValue>> column)
        {
            return WhereNotNull(ModelPropertyExpression.GetName(column, nameof(WhereNotNull)));
        }

        public QueryBuilder<TModel> WhereDate(string Column, string Date)
        {
            InitWhere(Column, Date, null, LogicalOperators.DATE);
            return this;
        }

        public QueryBuilder<TModel> WhereDate<TValue>(Expression<Func<TModel, TValue>> column, string date)
        {
            return WhereDate(ModelPropertyExpression.GetName(column, nameof(WhereDate)), date);
        }

        public QueryBuilder<TModel> WhereYear(string Column, string Year)
        {
            InitWhere(Column, Year, null, LogicalOperators.WHEREYEAR);
            return this;
        }

        public QueryBuilder<TModel> WhereYear<TValue>(Expression<Func<TModel, TValue>> column, string year)
        {
            return WhereYear(ModelPropertyExpression.GetName(column, nameof(WhereYear)), year);
        }

        public QueryBuilder<TModel> WhereMonth(string Column, string Month)
        {
            InitWhere(Column, Month, null, LogicalOperators.WHEREMONTH);
            return this;
        }

        public QueryBuilder<TModel> WhereMonth<TValue>(Expression<Func<TModel, TValue>> column, string month)
        {
            return WhereMonth(ModelPropertyExpression.GetName(column, nameof(WhereMonth)), month);
        }

        public QueryBuilder<TModel> WhereDay(string Column, string Day)
        {
            InitWhere(Column, Day, null, LogicalOperators.WHEREDAY);
            return this;
        }

        public QueryBuilder<TModel> WhereDay<TValue>(Expression<Func<TModel, TValue>> column, string day)
        {
            return WhereDay(ModelPropertyExpression.GetName(column, nameof(WhereDay)), day);
        }


        public QueryBuilder<TModel> OrWhereYear(string Column, string Year)
        {
            InitWhere(Column, Year, null, LogicalOperators.ORWHEREYEAR);
            return this;
        }

        public QueryBuilder<TModel> OrWhereYear<TValue>(Expression<Func<TModel, TValue>> column, string year)
        {
            return OrWhereYear(ModelPropertyExpression.GetName(column, nameof(OrWhereYear)), year);
        }

        public QueryBuilder<TModel> OrWhereMonth(string Column, string Month)
        {
            InitWhere(Column, Month, null, LogicalOperators.ORWHEREMONTH);
            return this;
        }

        public QueryBuilder<TModel> OrWhereMonth<TValue>(Expression<Func<TModel, TValue>> column, string month)
        {
            return OrWhereMonth(ModelPropertyExpression.GetName(column, nameof(OrWhereMonth)), month);
        }

        public QueryBuilder<TModel> OrWhereDay(string Column, string Day)
        {
            InitWhere(Column, Day, null, LogicalOperators.ORWHEREDAY);
            return this;
        }

        public QueryBuilder<TModel> OrWhereDay<TValue>(Expression<Func<TModel, TValue>> column, string day)
        {
            return OrWhereDay(ModelPropertyExpression.GetName(column, nameof(OrWhereDay)), day);
        }


        /// <summary>
        ///    
        /// </summary>
        /// <param name="Invert">Reverses the order in the query of the Column and Date parameters</param>
        /// <param name="ComparisonType">The comparison types are Year, Month, Day, Minute</param>
        public QueryBuilder<TModel> WhereDateDiff(string Column, string Date, int Difference, DateDiff ComparisonType, bool Invert = false)
        {

            LogicalOperators logicalOperator = Enum.TryParse(ComparisonType.ToString(), out LogicalOperators outValue) ? outValue : LogicalOperators.YEAR;

            InitWhere(Column, Date, null, logicalOperator, Difference, Invert);

            return this;
        }

        /// <summary>
        ///     
        /// </summary>
        /// <param name="Invert">Reverses the order in the query of the Column and Date parameters</param>
        /// <param name="ComparisonType">The comparison types are Year, Month, Day, Minute</param>
        public QueryBuilder<TModel> WhereDateDiff(string Column, string Date, string ComparisonOperator, int Difference, DateDiff ComparisonType, bool Invert = false)
        {
            LogicalOperators logicalOperator = Enum.TryParse(ComparisonType.ToString(), out LogicalOperators outValue) ? outValue : LogicalOperators.YEAR;
            InitWhere(Column, Date, ComparisonOperator, logicalOperator, Difference, Invert);
            return this;
        }


        public QueryBuilder<TModel> WhereDateDiff<TValue>(Expression<Func<TModel, TValue>> column, string date, int difference, DateDiff comparisonType, bool invert = false)
        {
            return WhereDateDiff(ModelPropertyExpression.GetName(column, nameof(WhereDateDiff)), date, difference, comparisonType, invert);
        }

        public QueryBuilder<TModel> WhereDateDiff<TValue>(Expression<Func<TModel, TValue>> column, string date, string comparisonOperator, int difference, DateDiff comparisonType, bool invert = false)
        {
            return WhereDateDiff(ModelPropertyExpression.GetName(column, nameof(WhereDateDiff)), date, comparisonOperator, difference, comparisonType, invert);
        }


        public QueryBuilder<TModel> WhereColumn(string FirstColumn, string SecondColumn)
        {
            FirstColumn =
                ValidateColumn(
                    FirstColumn,
                    nameof(WhereColumn)
                );

            SecondColumn =
                ValidateColumn(
                    SecondColumn,
                    nameof(WhereColumn)
                );

            InitWhere(
                FirstColumn,
                SecondColumn,
                null,
                LogicalOperators.COLUMN
            );

            return this;
        }

        public QueryBuilder<TModel> WhereColumn(string FirstColumn, string ComparisonOperator, string SecondColumn)
        {
            FirstColumn =
                ValidateColumn(
                    FirstColumn,
                    nameof(WhereColumn)
                );

            SecondColumn =
                ValidateColumn(
                    SecondColumn,
                    nameof(WhereColumn)
                );

            InitWhere(
                FirstColumn,
                SecondColumn,
                ComparisonOperator,
                LogicalOperators.COLUMN
            );

            return this;
        }

        public QueryBuilder<TModel> WhereColumn<TFirst, TSecond>(Expression<Func<TModel, TFirst>> firstColumn, Expression<Func<TModel, TSecond>> secondColumn)
        {
            return WhereColumn(ModelPropertyExpression.GetName(firstColumn, nameof(WhereColumn)), ModelPropertyExpression.GetName(secondColumn, nameof(WhereColumn)));
        }

        public QueryBuilder<TModel> WhereColumn<TFirst, TSecond>(Expression<Func<TModel, TFirst>> firstColumn, string comparisonOperator, Expression<Func<TModel, TSecond>> secondColumn)
        {
            return WhereColumn(ModelPropertyExpression.GetName(firstColumn, nameof(WhereColumn)), comparisonOperator, ModelPropertyExpression.GetName(secondColumn, nameof(WhereColumn)));
        }

        public QueryBuilder<TModel> WhereBetween(string Column, Between Value)
        {
            InitWhere(Column, Value, null, LogicalOperators.BETWEEN);
            return this;
        }

        public QueryBuilder<TModel> WhereBetween<TValue>(Expression<Func<TModel, TValue>> column, Between value)
        {
            return WhereBetween(ModelPropertyExpression.GetName(column, nameof(WhereBetween)), value);
        }

        public QueryBuilder<TModel> WhereNotBetween(string Column, Between Value)
        {
            InitWhere(Column, Value, null, LogicalOperators.NOT_BETWEEN);
            return this;
        }

        public QueryBuilder<TModel> WhereNotBetween<TValue>(Expression<Func<TModel, TValue>> column, Between value)
        {
            return WhereNotBetween(ModelPropertyExpression.GetName(column, nameof(WhereNotBetween)), value);
        }

        public QueryBuilder<TModel> WhereDateBetween(string Column, DateBetween Value)
        {
            InitWhere(Column, Value, null, LogicalOperators.DATEBETWEEN);
            return this;
        }

        public QueryBuilder<TModel> WhereDateBetween<TValue>(Expression<Func<TModel, TValue>> column, DateBetween value)
        {
            return WhereDateBetween(ModelPropertyExpression.GetName(column, nameof(WhereDateBetween)), value);
        }

        public QueryBuilder<TModel> WhereHas<TRelationship>(string Relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? Builder = null)
        {
            WhereHasBuilder(Clauses.EXISTS, LogicalOperators.AND, Relationship, Builder);
            return this;
        }

        public QueryBuilder<TModel> WhereHas<TRelationship>(string Relationship, string ComparisonOperator, int Value)
        {
            WhereHasBuilder<TRelationship>(Clauses.EXISTS, LogicalOperators.AND, Relationship, null, ComparisonOperator, Value);
            return this;
        }

        public QueryBuilder<TModel> WhereHas<TRelationship>(string Relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>> Builder, string ComparisonOperator, int Value)
        {
            WhereHasBuilder(Clauses.EXISTS, LogicalOperators.AND, Relationship, Builder, ComparisonOperator, Value);
            return this;
        }

        public QueryBuilder<TModel> OrWhereHas<TRelationship>(string Relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? Builder = null)
        {
            WhereHasBuilder(Clauses.EXISTS, LogicalOperators.OR, Relationship, Builder);
            return this;
        }

        public QueryBuilder<TModel> OrWhereHas<TRelationship>(string Relationship, string ComparisonOperator, int Value)
        {
            WhereHasBuilder<TRelationship>(Clauses.EXISTS, LogicalOperators.OR, Relationship, null, ComparisonOperator, Value);
            return this;
        }

        public QueryBuilder<TModel> OrWhereHas<TRelationship>(string Relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>> Builder, string ComparisonOperator, int Value)
        {
            WhereHasBuilder(Clauses.EXISTS, LogicalOperators.OR, Relationship, Builder, ComparisonOperator, Value);
            return this;
        }

        public QueryBuilder<TModel> WhereDoesntHave<TRelationship>(string Relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? Builder = null)
        {
            WhereHasBuilder(Clauses.NOT_EXISTS, LogicalOperators.AND, Relationship, Builder);
            return this;
        }

        public QueryBuilder<TModel> OrWhereDoesntHave<TRelationship>(string Relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? Builder = null)
        {
            WhereHasBuilder(Clauses.NOT_EXISTS, LogicalOperators.OR, Relationship, Builder);
            return this;
        }

        public QueryBuilder<TModel> WhereHas<TRelationship>(Expression<Func<TModel, IEnumerable<TRelationship>>> relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? builder = null) where TRelationship : Model<TRelationship>, new()
        {
            return WhereHas(GetHasManyRelationshipName(relationship), builder);
        }

        public QueryBuilder<TModel> WhereHas<TRelationship>(Expression<Func<TModel, TRelationship?>> relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? builder = null) where TRelationship : Model<TRelationship>, new()
        {
            return WhereHas(GetSingleRelationshipName(relationship), builder);
        }

        public QueryBuilder<TModel> WhereHas<TRelationship>(Expression<Func<TModel, IEnumerable<TRelationship>>> relationship, string comparisonOperator, int value) where TRelationship : Model<TRelationship>, new()
        {
            return WhereHas<TRelationship>(GetHasManyRelationshipName(relationship), comparisonOperator, value);
        }

        public QueryBuilder<TModel> WhereHas<TRelationship>(Expression<Func<TModel, TRelationship?>> relationship, string comparisonOperator, int value) where TRelationship : Model<TRelationship>, new()
        {
            return WhereHas<TRelationship>(GetSingleRelationshipName(relationship), comparisonOperator, value);
        }

        public QueryBuilder<TModel> WhereHas<TRelationship>(Expression<Func<TModel, IEnumerable<TRelationship>>> relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>> builder, string comparisonOperator, int value) where TRelationship : Model<TRelationship>, new()
        {
            return WhereHas(GetHasManyRelationshipName(relationship), builder, comparisonOperator, value);
        }

        public QueryBuilder<TModel> WhereHas<TRelationship>(Expression<Func<TModel, TRelationship?>> relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>> builder, string comparisonOperator, int value) where TRelationship : Model<TRelationship>, new()
        {
            return WhereHas(GetSingleRelationshipName(relationship), builder, comparisonOperator, value);
        }

        public QueryBuilder<TModel> OrWhereHas<TRelationship>(Expression<Func<TModel, IEnumerable<TRelationship>>> relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? builder = null) where TRelationship : Model<TRelationship>, new()
        {
            return OrWhereHas(GetHasManyRelationshipName(relationship), builder);
        }

        public QueryBuilder<TModel> OrWhereHas<TRelationship>(Expression<Func<TModel, TRelationship?>> relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? builder = null) where TRelationship : Model<TRelationship>, new()
        {
            return OrWhereHas(GetSingleRelationshipName(relationship), builder);
        }

        public QueryBuilder<TModel> OrWhereHas<TRelationship>(Expression<Func<TModel, IEnumerable<TRelationship>>> relationship, string comparisonOperator, int value) where TRelationship : Model<TRelationship>, new()
        {
            return OrWhereHas<TRelationship>(GetHasManyRelationshipName(relationship), comparisonOperator, value);
        }

        public QueryBuilder<TModel> OrWhereHas<TRelationship>(Expression<Func<TModel, TRelationship?>> relationship, string comparisonOperator, int value) where TRelationship : Model<TRelationship>, new()
        {
            return OrWhereHas<TRelationship>(GetSingleRelationshipName(relationship), comparisonOperator, value);
        }

        public QueryBuilder<TModel> OrWhereHas<TRelationship>(Expression<Func<TModel, IEnumerable<TRelationship>>> relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>> builder, string comparisonOperator, int value) where TRelationship : Model<TRelationship>, new()
        {
            return OrWhereHas(GetHasManyRelationshipName(relationship), builder, comparisonOperator, value);
        }

        public QueryBuilder<TModel> OrWhereHas<TRelationship>(Expression<Func<TModel, TRelationship?>> relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>> builder, string comparisonOperator, int value) where TRelationship : Model<TRelationship>, new()
        {
            return OrWhereHas(GetSingleRelationshipName(relationship), builder, comparisonOperator, value);
        }

        public QueryBuilder<TModel> WhereDoesntHave<TRelationship>(Expression<Func<TModel, IEnumerable<TRelationship>>> relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? builder = null) where TRelationship : Model<TRelationship>, new()
        {
            return WhereDoesntHave(GetHasManyRelationshipName(relationship), builder);
        }

        public QueryBuilder<TModel> WhereDoesntHave<TRelationship>(Expression<Func<TModel, TRelationship?>> relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? builder = null) where TRelationship : Model<TRelationship>, new()
        {
            return WhereDoesntHave(GetSingleRelationshipName(relationship), builder);
        }

        public QueryBuilder<TModel> OrWhereDoesntHave<TRelationship>(Expression<Func<TModel, IEnumerable<TRelationship>>> relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? builder = null) where TRelationship : Model<TRelationship>, new()
        {
            return OrWhereDoesntHave(GetHasManyRelationshipName(relationship), builder);
        }

        public QueryBuilder<TModel> OrWhereDoesntHave<TRelationship>(Expression<Func<TModel, TRelationship?>> relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? builder = null) where TRelationship : Model<TRelationship>, new()
        {
            return OrWhereDoesntHave(GetSingleRelationshipName(relationship), builder);
        }


        public QueryBuilder<TModel> When(bool Condition, Func<SubQuery<TModel>, SubQuery<TModel>>? Builder = null)
        {
            InitWhen(Condition, Builder);
            return this;
        }

        public QueryBuilder<TModel> Distinct()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            if (RelationshipProjections.Count > 0)
            {
                throw new QueryBuilderException("Distinct is incompatible with relationship projections such as WithCount.");
            }

            Query.Replace("_selector_all", $"{Clauses.DISTINCT} *");

            return this;
        }

        public QueryBuilder<TModel> Distinct(string Columns)
        {
            Columns =
                ValidateColumn(
                    Columns,
                    nameof(Distinct)
                );

            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            if (RelationshipProjections.Count > 0)
            {
                throw new QueryBuilderException("Distinct is incompatible with relationship projections such as WithCount.");
            }

            Query.Replace(
                "_selector_all",
                $"{Clauses.DISTINCT} {Columns}"
            );

            return this;
        }


        public QueryBuilder<TModel> WithCount(string relationship, string? alias = null)
        {
            if (!CheckQueryInit()) SimpleQuery();

            RelationshipDefinition definition = RelationshipMetadataCache.GetRequired(typeof(TModel), relationship);

            return AddRelationshipCount(definition, alias);
        }

        public QueryBuilder<TModel> WithCount<TRelated>(string relationship, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            if (constraint == null) throw new ArgumentNullException(nameof(constraint));
            if (!CheckQueryInit()) SimpleQuery();

            RelationshipDefinition definition = RelationshipMetadataCache.GetRequired(typeof(TModel), relationship);

            return AddRelationshipCount(definition, constraint, alias);
        }

        public QueryBuilder<TModel> WithCount<TRelated>(Expression<Func<TModel, IEnumerable<TRelated>>> relationship, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return WithCount(GetHasManyRelationshipName(relationship), alias);
        }

        public QueryBuilder<TModel> WithCount<TRelated>(Expression<Func<TModel, IEnumerable<TRelated>>> relationship, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return WithCount<TRelated>(GetHasManyRelationshipName(relationship), constraint, alias);
        }

        public QueryBuilder<TModel> WithCount<TRelated>(Expression<Func<TModel, TRelated?>> relationship, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return WithCount(GetSingleRelationshipName(relationship), alias);
        }

        public QueryBuilder<TModel> WithCount<TRelated>(Expression<Func<TModel, TRelated?>> relationship, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return WithCount<TRelated>(GetSingleRelationshipName(relationship), constraint, alias);
        }

        public QueryBuilder<TModel> WithExists(string relationship, string? alias = null)
        {
            if (!CheckQueryInit()) SimpleQuery();

            RelationshipDefinition definition = RelationshipMetadataCache.GetRequired(typeof(TModel), relationship);

            return AddRelationshipExists(definition, alias);
        }

        public QueryBuilder<TModel> WithExists<TRelated>(string relationship, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            if (constraint == null) throw new ArgumentNullException(nameof(constraint));
            if (!CheckQueryInit()) SimpleQuery();

            RelationshipDefinition definition = RelationshipMetadataCache.GetRequired(typeof(TModel), relationship);

            return AddRelationshipExists(definition, constraint, alias);
        }

        public QueryBuilder<TModel> WithExists<TRelated>(Expression<Func<TModel, IEnumerable<TRelated>>> relationship, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return WithExists(GetHasManyRelationshipName(relationship), alias);
        }

        public QueryBuilder<TModel> WithExists<TRelated>(Expression<Func<TModel, IEnumerable<TRelated>>> relationship, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return WithExists<TRelated>(GetHasManyRelationshipName(relationship), constraint, alias);
        }

        public QueryBuilder<TModel> WithExists<TRelated>(Expression<Func<TModel, TRelated?>> relationship, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return WithExists(GetSingleRelationshipName(relationship), alias);
        }

        public QueryBuilder<TModel> WithExists<TRelated>(Expression<Func<TModel, TRelated?>> relationship, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return WithExists<TRelated>(GetSingleRelationshipName(relationship), constraint, alias);
        }

        public QueryBuilder<TModel> WithSum(string relationship, string column, string? alias = null)
        {
            if (!CheckQueryInit()) SimpleQuery();

            RelationshipDefinition definition = RelationshipMetadataCache.GetRequired(typeof(TModel), relationship);

            return AddRelationshipAggregate(definition, column, "SUM", "Sum", nameof(WithSum), alias);
        }

        public QueryBuilder<TModel> WithSum<TRelated>(string relationship, string column, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            if (constraint == null) throw new ArgumentNullException(nameof(constraint));
            if (!CheckQueryInit()) SimpleQuery();

            RelationshipDefinition definition = RelationshipMetadataCache.GetRequired(typeof(TModel), relationship);

            return AddRelationshipAggregate(definition, column, "SUM", "Sum", nameof(WithSum), constraint, alias);
        }

        public QueryBuilder<TModel> WithSum<TRelated>(Expression<Func<TModel, IEnumerable<TRelated>>> relationship, string column, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return WithSum(GetHasManyRelationshipName(relationship), column, alias);
        }

        public QueryBuilder<TModel> WithSum<TRelated>(Expression<Func<TModel, IEnumerable<TRelated>>> relationship, string column, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return WithSum(GetHasManyRelationshipName(relationship), column, constraint, alias);
        }

        public QueryBuilder<TModel> WithSum<TRelated>(Expression<Func<TModel, TRelated?>> relationship, string column, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return WithSum(GetSingleRelationshipName(relationship), column, alias);
        }

        public QueryBuilder<TModel> WithSum<TRelated>(Expression<Func<TModel, TRelated?>> relationship, string column, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return WithSum(GetSingleRelationshipName(relationship), column, constraint, alias);
        }

        public QueryBuilder<TModel> WithAvg(string relationship, string column, string? alias = null)
        {
            if (!CheckQueryInit()) SimpleQuery();

            RelationshipDefinition definition = RelationshipMetadataCache.GetRequired(typeof(TModel), relationship);

            return AddRelationshipAggregate(definition, column, "AVG", "Avg", nameof(WithAvg), alias);
        }

        public QueryBuilder<TModel> WithAvg<TRelated>(string relationship, string column, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            if (constraint == null) throw new ArgumentNullException(nameof(constraint));
            if (!CheckQueryInit()) SimpleQuery();

            RelationshipDefinition definition = RelationshipMetadataCache.GetRequired(typeof(TModel), relationship);

            return AddRelationshipAggregate(definition, column, "AVG", "Avg", nameof(WithAvg), constraint, alias);
        }

        public QueryBuilder<TModel> WithAvg<TRelated>(Expression<Func<TModel, IEnumerable<TRelated>>> relationship, string column, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return WithAvg(GetHasManyRelationshipName(relationship), column, alias);
        }

        public QueryBuilder<TModel> WithAvg<TRelated>(Expression<Func<TModel, IEnumerable<TRelated>>> relationship, string column, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return WithAvg(GetHasManyRelationshipName(relationship), column, constraint, alias);
        }

        public QueryBuilder<TModel> WithAvg<TRelated>(Expression<Func<TModel, TRelated?>> relationship, string column, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return WithAvg(GetSingleRelationshipName(relationship), column, alias);
        }

        public QueryBuilder<TModel> WithAvg<TRelated>(Expression<Func<TModel, TRelated?>> relationship, string column, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return WithAvg(GetSingleRelationshipName(relationship), column, constraint, alias);
        }

        public QueryBuilder<TModel> WithMin(string relationship, string column, string? alias = null)
        {
            if (!CheckQueryInit()) SimpleQuery();

            RelationshipDefinition definition = RelationshipMetadataCache.GetRequired(typeof(TModel), relationship);

            return AddRelationshipAggregate(definition, column, "MIN", "Min", nameof(WithMin), alias);
        }

        public QueryBuilder<TModel> WithMin<TRelated>(string relationship, string column, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            if (constraint == null) throw new ArgumentNullException(nameof(constraint));
            if (!CheckQueryInit()) SimpleQuery();

            RelationshipDefinition definition = RelationshipMetadataCache.GetRequired(typeof(TModel), relationship);

            return AddRelationshipAggregate(definition, column, "MIN", "Min", nameof(WithMin), constraint, alias);
        }

        public QueryBuilder<TModel> WithMin<TRelated>(Expression<Func<TModel, IEnumerable<TRelated>>> relationship, string column, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return WithMin(GetHasManyRelationshipName(relationship), column, alias);
        }

        public QueryBuilder<TModel> WithMin<TRelated>(Expression<Func<TModel, IEnumerable<TRelated>>> relationship, string column, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return WithMin(GetHasManyRelationshipName(relationship), column, constraint, alias);
        }

        public QueryBuilder<TModel> WithMin<TRelated>(Expression<Func<TModel, TRelated?>> relationship, string column, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return WithMin(GetSingleRelationshipName(relationship), column, alias);
        }

        public QueryBuilder<TModel> WithMin<TRelated>(Expression<Func<TModel, TRelated?>> relationship, string column, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return WithMin(GetSingleRelationshipName(relationship), column, constraint, alias);
        }

        public QueryBuilder<TModel> WithMax(string relationship, string column, string? alias = null)
        {
            if (!CheckQueryInit()) SimpleQuery();

            RelationshipDefinition definition = RelationshipMetadataCache.GetRequired(typeof(TModel), relationship);

            return AddRelationshipAggregate(definition, column, "MAX", "Max", nameof(WithMax), alias);
        }

        public QueryBuilder<TModel> WithMax<TRelated>(string relationship, string column, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            if (constraint == null) throw new ArgumentNullException(nameof(constraint));
            if (!CheckQueryInit()) SimpleQuery();

            RelationshipDefinition definition = RelationshipMetadataCache.GetRequired(typeof(TModel), relationship);

            return AddRelationshipAggregate(definition, column, "MAX", "Max", nameof(WithMax), constraint, alias);
        }

        public QueryBuilder<TModel> WithMax<TRelated>(Expression<Func<TModel, IEnumerable<TRelated>>> relationship, string column, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return WithMax(GetHasManyRelationshipName(relationship), column, alias);
        }

        public QueryBuilder<TModel> WithMax<TRelated>(Expression<Func<TModel, IEnumerable<TRelated>>> relationship, string column, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return WithMax(GetHasManyRelationshipName(relationship), column, constraint, alias);
        }

        public QueryBuilder<TModel> WithMax<TRelated>(Expression<Func<TModel, TRelated?>> relationship, string column, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return WithMax(GetSingleRelationshipName(relationship), column, alias);
        }

        public QueryBuilder<TModel> WithMax<TRelated>(Expression<Func<TModel, TRelated?>> relationship, string column, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return WithMax(GetSingleRelationshipName(relationship), column, constraint, alias);
        }



        public QueryBuilder<TModel> GroupBy(params string[] Columns)
        {
            Columns =
                ValidateColumns(
                    Columns,
                    nameof(GroupBy)
                );

            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            if (!HasGroupByClause())
            {
                string clauseName =
                    string.Join(
                        " ",
                        Clauses.GROUP_BY
                            .ToString()
                            .Split("_")
                    );

                string cols =
                    string.Join(
                        ",",
                        Columns
                    );

                Query.Append(
                    $" {clauseName} {cols} "
                );
            }

            return this;
        }

        public QueryBuilder<TModel> Having(string Column, string ComparisonOperator, object Value)
        {
            Column =
                ValidateColumn(
                    Column,
                    nameof(Having)
                );

            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            if (!Query.ToString()
                    .Contains(
                        Clauses.HAVING.ToString()
                    )
                &&
                HasGroupByClause())
            {
                Query.Append(
                    $" {Clauses.HAVING} " +
                    $"{Column} " +
                    $"{ComparisonOperator} " +
                    $"{AddParameter(Value)}"
                );
            }

            return this;
        }

        public QueryBuilder<TModel> HavingRaw(string Raw)
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            if (!Query.ToString().Contains(Clauses.HAVING.ToString()) && HasGroupByClause())
            {
                Query.Append($" {Clauses.HAVING} {Raw} ");
            }

            return this;
        }

        public QueryBuilder<TModel> OrderBy(string Column, string Direction = "ASC")
        {
            Column =
                ValidateColumn(
                    Column,
                    nameof(OrderBy)
                );

            AddOrderClause(
                Column,
                Direction
            );

            return this;
        }

        public QueryBuilder<TModel> InRandomOrder()
        {
            AddOrderClause(" NEWID() ", "ASC");

            return this;
        }


        private void EnsureNoEagerLoadsForProjection(string methodName)
        {
            if (EagerLoads.HasLoads)
            {
                throw new RelationshipException($"{methodName} cannot be used with With() because eager loading requires materializing model '{typeof(TModel).Name}'. Remove With() or use the non-projection terminal method.");
            }
        }

        private static string GetRequiredPrimaryKeyNameForFind()
        {
            PropertyInfo? primaryKey = GetPropertyInfoByAttribute(typeof(PrimaryKey));

            if (primaryKey == null)
            {
                throw new ModelConfigurationException($"Primary key is not defined for model '{typeof(TModel).Name}'. Add the [PrimaryKey] attribute to the appropriate property.");
            }

            return primaryKey.Name;
        }

        private QueryBuilder<TModel> AddRelationshipCount(RelationshipDefinition definition, string? alias)
        {
            EnsureRelationshipProjectionCanBeAdded(nameof(WithCount));

            object relatedInstance = CreateRelationshipModelInstance(definition.RelatedType);

            EnsureRelationshipProjectionSameConnection(definition, relatedInstance, nameof(WithCount));

            string relatedTable = GetTableName(relatedInstance);
            string ownTable = GetTableName();
            string projectionAlias = ValidateRelationshipProjectionAlias(alias ?? $"{definition.Name}_Count");

            string countQuery = $"SELECT COUNT(*) FROM {relatedTable} WHERE {relatedTable}.{definition.RelatedKey} = {ownTable}.{definition.LocalKey}";

            AddRelationshipProjection(countQuery, projectionAlias);

            return this;
        }

        private QueryBuilder<TModel> AddRelationshipCount<TRelated>(RelationshipDefinition definition, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias) where TRelated : Model<TRelated>, new()
        {
            EnsureRelationshipProjectionCanBeAdded(nameof(WithCount));

            if (definition.RelatedType != typeof(TRelated))
            {
                throw new RelationshipException($"Relationship '{definition.Name}' on model '{typeof(TModel).Name}' points to '{definition.RelatedType.Name}', but WithCount was requested with '{typeof(TRelated).Name}'.");
            }

            EnsureRelationshipProjectionSameConnection(definition, QueryBuilder<TRelated>.GetConnectionString(), nameof(WithCount));

            string relatedTable = QueryBuilder<TRelated>.GetTableName();
            string ownTable = GetTableName();
            string projectionAlias = ValidateRelationshipProjectionAlias(alias ?? $"{definition.Name}_Count");

            var countBuilder = new SubQuery<TRelated>($"SELECT COUNT(*) FROM {relatedTable} WHERE {relatedTable}.{definition.RelatedKey} = {ownTable}.{definition.LocalKey}", Clauses.EXISTS, ParameterContext)
            {
                ConditionsAdded = 1
            };

            SubQuery<TRelated>? constrainedBuilder = constraint(countBuilder);

            if (constrainedBuilder == null)
            {
                throw new RelationshipException($"WithCount constraint for relationship '{definition.Name}' returned null.");
            }

            if (!ReferenceEquals(constrainedBuilder, countBuilder))
            {
                throw new RelationshipException($"WithCount constraint for relationship '{definition.Name}' must configure and return the SubQuery instance provided by DapperGlib.");
            }

            AddRelationshipProjection(countBuilder.ToParameterizedSql(), projectionAlias);

            return this;
        }


        private QueryBuilder<TModel> AddRelationshipExists(RelationshipDefinition definition, string? alias)
        {
            EnsureRelationshipProjectionCanBeAdded(nameof(WithExists));

            object relatedInstance = CreateRelationshipModelInstance(definition.RelatedType);

            EnsureRelationshipProjectionSameConnection(definition, relatedInstance, nameof(WithExists));

            string relatedTable = GetTableName(relatedInstance);
            string ownTable = GetTableName();
            string projectionAlias = ValidateRelationshipProjectionAlias(alias ?? $"{definition.Name}_Exists");

            string existsQuery = $"SELECT 1 FROM {relatedTable} WHERE {relatedTable}.{definition.RelatedKey} = {ownTable}.{definition.LocalKey}";
            string projection = $"CASE WHEN EXISTS ({existsQuery}) THEN CAST(1 AS bit) ELSE CAST(0 AS bit) END";

            AddRelationshipProjection(projection, projectionAlias);

            return this;
        }

        private QueryBuilder<TModel> AddRelationshipExists<TRelated>(RelationshipDefinition definition, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias) where TRelated : Model<TRelated>, new()
        {
            EnsureRelationshipProjectionCanBeAdded(nameof(WithExists));

            if (definition.RelatedType != typeof(TRelated))
            {
                throw new RelationshipException($"Relationship '{definition.Name}' on model '{typeof(TModel).Name}' points to '{definition.RelatedType.Name}', but WithExists was requested with '{typeof(TRelated).Name}'.");
            }

            EnsureRelationshipProjectionSameConnection(definition, QueryBuilder<TRelated>.GetConnectionString(), nameof(WithExists));

            string relatedTable = QueryBuilder<TRelated>.GetTableName();
            string ownTable = GetTableName();
            string projectionAlias = ValidateRelationshipProjectionAlias(alias ?? $"{definition.Name}_Exists");

            var existsBuilder = new SubQuery<TRelated>($"SELECT 1 FROM {relatedTable} WHERE {relatedTable}.{definition.RelatedKey} = {ownTable}.{definition.LocalKey}", Clauses.EXISTS, ParameterContext)
            {
                ConditionsAdded = 1
            };

            SubQuery<TRelated>? constrainedBuilder = constraint(existsBuilder);

            if (constrainedBuilder == null)
            {
                throw new RelationshipException($"WithExists constraint for relationship '{definition.Name}' returned null.");
            }

            if (!ReferenceEquals(constrainedBuilder, existsBuilder))
            {
                throw new RelationshipException($"WithExists constraint for relationship '{definition.Name}' must configure and return the SubQuery instance provided by DapperGlib.");
            }

            string projection = $"CASE WHEN EXISTS ({existsBuilder.ToParameterizedSql()}) THEN CAST(1 AS bit) ELSE CAST(0 AS bit) END";

            AddRelationshipProjection(projection, projectionAlias);

            return this;
        }


        private QueryBuilder<TModel> AddRelationshipAggregate(RelationshipDefinition definition, string column, string aggregateFunction, string aggregateSuffix, string operationName, string? alias)
        {
            EnsureRelationshipProjectionCanBeAdded(operationName);

            object relatedInstance = CreateRelationshipModelInstance(definition.RelatedType);

            EnsureRelationshipProjectionSameConnection(definition, relatedInstance, operationName);

            string relatedColumn = ValidateRelationshipAggregateColumn(definition.RelatedType, column, operationName);
            string relatedTable = GetTableName(relatedInstance);
            string ownTable = GetTableName();
            string projectionAlias = ValidateRelationshipProjectionAlias(alias ?? $"{definition.Name}_{relatedColumn}_{aggregateSuffix}");

            string aggregateQuery = $"SELECT {aggregateFunction}({relatedTable}.{relatedColumn}) FROM {relatedTable} WHERE {relatedTable}.{definition.RelatedKey} = {ownTable}.{definition.LocalKey}";

            AddRelationshipProjection(aggregateQuery, projectionAlias);

            return this;
        }

        private QueryBuilder<TModel> AddRelationshipAggregate<TRelated>(RelationshipDefinition definition, string column, string aggregateFunction, string aggregateSuffix, string operationName, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias) where TRelated : Model<TRelated>, new()
        {
            EnsureRelationshipProjectionCanBeAdded(operationName);

            if (definition.RelatedType != typeof(TRelated))
            {
                throw new RelationshipException($"Relationship '{definition.Name}' on model '{typeof(TModel).Name}' points to '{definition.RelatedType.Name}', but {operationName} was requested with '{typeof(TRelated).Name}'.");
            }

            EnsureRelationshipProjectionSameConnection(definition, QueryBuilder<TRelated>.GetConnectionString(), operationName);

            string relatedColumn = ValidateRelationshipAggregateColumn(typeof(TRelated), column, operationName);
            string relatedTable = QueryBuilder<TRelated>.GetTableName();
            string ownTable = GetTableName();
            string projectionAlias = ValidateRelationshipProjectionAlias(alias ?? $"{definition.Name}_{relatedColumn}_{aggregateSuffix}");

            var aggregateBuilder = new SubQuery<TRelated>($"SELECT {aggregateFunction}({relatedTable}.{relatedColumn}) FROM {relatedTable} WHERE {relatedTable}.{definition.RelatedKey} = {ownTable}.{definition.LocalKey}", Clauses.EXISTS, ParameterContext)
            {
                ConditionsAdded = 1
            };

            SubQuery<TRelated>? constrainedBuilder = constraint(aggregateBuilder);

            if (constrainedBuilder == null)
            {
                throw new RelationshipException($"{operationName} constraint for relationship '{definition.Name}' returned null.");
            }

            if (!ReferenceEquals(constrainedBuilder, aggregateBuilder))
            {
                throw new RelationshipException($"{operationName} constraint for relationship '{definition.Name}' must configure and return the SubQuery instance provided by DapperGlib.");
            }

            AddRelationshipProjection(aggregateBuilder.ToParameterizedSql(), projectionAlias);

            return this;
        }



        private void AddRelationshipProjection(string sql, string alias)
        {
            string queryText = Query.ToString();
            Match match = Regex.Match(queryText, Regex.Escape("_selector_all"));

            if (!match.Success)
            {
                throw new QueryBuilderException("Relationship projection cannot be added because the query does not contain a model selector.");
            }

            RelationshipProjections.Add($", ({sql}) AS [{alias}] ");

            int selectorEnd = match.Index + "_selector_all".Length;

            Query = new StringBuilder(queryText.Insert(selectorEnd, $" relationship_projection_{RelationshipProjections.Count} "));
        }

        private void EnsureRelationshipProjectionCanBeAdded(string operationName)
        {
            if (Query.ToString().Contains(Clauses.DISTINCT.ToString(), StringComparison.OrdinalIgnoreCase))
            {
                throw new QueryBuilderException($"{operationName} is incompatible with Distinct.");
            }
        }

        private static object CreateRelationshipModelInstance(Type modelType)
        {
            try
            {
                return Activator.CreateInstance(modelType) ?? throw new RelationshipException($"Unable to create an instance of related model '{modelType.Name}'.");
            }
            catch (RelationshipException)
            {
                throw;
            }
            catch (Exception ex)
            {
                throw new RelationshipException($"Unable to create an instance of related model '{modelType.Name}'.", ex);
            }
        }

        private void EnsureRelationshipProjectionSameConnection(RelationshipDefinition definition, object relatedInstance, string operationName)
        {
            PropertyInfo? connectionProperty = relatedInstance.GetType().GetProperty("Connection", BindingFlags.Instance | BindingFlags.Public);
            string relatedConnection = connectionProperty?.GetValue(relatedInstance) as string ?? "SqlConnection";

            EnsureRelationshipProjectionSameConnection(definition, relatedConnection, operationName);
        }

        private void EnsureRelationshipProjectionSameConnection(RelationshipDefinition definition, string relatedConnection, string operationName)
        {
            string ownConnection = GetConnectionString();

            if (!string.Equals(ownConnection, relatedConnection, StringComparison.Ordinal))
            {
                throw new RelationshipException($"Relationship '{definition.Name}' between '{typeof(TModel).Name}' and '{definition.RelatedType.Name}' cannot be used with {operationName} because they use different connection keys. Parent connection: '{ownConnection}'. Related connection: '{relatedConnection}'. Cross-connection relationship projections are not supported.");
            }
        }

        private static string ValidateRelationshipProjectionAlias(string alias)
        {
            if (string.IsNullOrWhiteSpace(alias))
            {
                throw new QueryBuilderException("Relationship projection alias cannot be null or empty.");
            }

            string value = alias.Trim();

            if (value.Length > 128 || !Regex.IsMatch(value, @"^[A-Za-z_][A-Za-z0-9_]*$"))
            {
                throw new QueryBuilderException($"Relationship projection alias '{alias}' is invalid. Use letters, numbers and underscores, beginning with a letter or underscore.");
            }

            return value;
        }

        private static string ValidateRelationshipAggregateColumn(Type relatedType, string column, string operationName)
        {
            if (string.IsNullOrWhiteSpace(column))
            {
                throw new QueryBuilderException($"{operationName} requires a valid related column.");
            }

            string value = column.Trim();

            if (!Regex.IsMatch(value, @"^[A-Za-z_][A-Za-z0-9_]*$"))
            {
                throw new QueryBuilderException($"Related aggregate column '{column}' is invalid.");
            }

            PropertyInfo? property = relatedType.GetProperty(value, BindingFlags.Instance | BindingFlags.Public | BindingFlags.IgnoreCase);

            if (property == null)
            {
                throw new QueryBuilderException($"{operationName} cannot aggregate column '{column}' because property '{column}' was not found on related model '{relatedType.Name}'.");
            }

            return property.Name;
        }


        internal void AddClause(Clauses Clause)
        {
            if (!Query.ToString().Contains(Clause.ToString()))
            {
                Query.Append($" {Clause.ToString()} ");
            }
        }

        protected void AddOrderClause(string Column, string? Direction = null)
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            var order = new StringBuilder("");

            string ClauseName = string.Join(" ", Clauses.ORDER_BY.ToString().Split("_"));

            if (OrderList.Count == 0)
            {
                order.Append($" {ClauseName} ");
            }
            else
            {
                order.Append(", ");
            }

            order.Append($" {Column} ");

            if (Direction != null)
            {
                order.Append($" {Direction} ");
            }

            OrderList.Add(order.ToString());

            Query.Append($" order_clause_{OrderList.Count} ");
        }

        internal static string? GetPrimaryKey()
        {
            PropertyInfo? primaryAttribute = Instance.GetType().GetProperties().Where(prop => Attribute.IsDefined(prop, typeof(PrimaryKey))).FirstOrDefault();
            return primaryAttribute?.Name;
        }

        internal static string? GetPrimaryKey(object Instance)
        {
            PropertyInfo? primaryAttribute = Instance.GetType().GetProperties().Where(prop => Attribute.IsDefined(prop, typeof(PrimaryKey))).FirstOrDefault();
            return primaryAttribute?.Name;
        }

        internal static bool IsIncrementing()
        {
            PropertyInfo Incrementing = Instance.GetType().GetProperties().Where(prop => prop.Name == "Incrementing").First();

            bool PropertyValue = (bool)Incrementing.GetValue(Instance, null)!;

            return PropertyValue;
        }

        internal static bool IsIncrementing(object Instance)
        {
            PropertyInfo Incrementing = Instance.GetType().GetProperties().Where(prop => prop.Name == "Incrementing").First();

            bool PropertyValue = (bool)Incrementing.GetValue(Instance, null)!;

            return PropertyValue;
        }

        protected static List<PropertyInfo> GetFillableProperties()
        {
            List<PropertyInfo> Properties = Instance.GetType().GetProperties().Where(prop => Attribute.IsDefined(prop, typeof(Fillable))).ToList();

            return Properties;
        }

        private static void EnsureGeneratedPrimaryKeyWritable(PropertyInfo primaryKey)
        {
            if (!primaryKey.CanWrite)
            {
                throw new ModelConfigurationException(
                    $"Primary key property '{primaryKey.Name}' " +
                    $"on model '{typeof(TModel).Name}' is read-only."
                );
            }
        }

        public QueryBuilder<TModel> Clone()
        {
            return new QueryBuilder<TModel>(this);
        }

        public QueryBuilder<TModel> Timeout(int seconds)
        {
            if (seconds <= 0)
            {
                throw new ArgumentOutOfRangeException(
                    nameof(seconds),
                    seconds,
                    "Timeout must be greater than zero seconds."
                );
            }

            QueryCommandTimeout = seconds;

            return this;
        }


    }
}
