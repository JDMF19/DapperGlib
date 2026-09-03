using Dapper;
using DapperGlib.Exceptions;
using DapperGlib.Util;
using Newtonsoft.Json;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;

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

            CountsRelationship =
                JsonConvert.DeserializeObject<List<string>>(
                    JsonConvert.SerializeObject(Clone.CountsRelationship),
                    deserializeSettings
                ) ?? new();

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
            List<PropertyInfo> properties =
                GetFillableProperties();

            PropertyInfo? primaryKey =
                GetPropertyInfoByAttribute(
                    Item!,
                    typeof(PrimaryKey)
                );

            if (primaryKey == null)
            {
                throw new ModelConfigurationException(
                    $"Primary key is not defined for model " +
                    $"'{typeof(T).Name}'. " +
                    $"Add the [PrimaryKey] attribute to the appropriate property."
                );
            }

            bool incrementing = IsIncrementing();

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

        public void Update(dynamic args)
        {

            var json = JsonConvert.SerializeObject(args);
            var item = (TModel)JsonConvert.DeserializeObject<TModel>(json);

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

            var regex = new Regex(Regex.Escape("FROM"));
            var match = regex.Match(Query.ToString());

            string replaced = string.Concat($"UPDATE {table} SET {String.Join(",", Values)} ", Query.ToString().AsSpan(match.Index));

            Query = new(replaced);

            ExecuteCommand(ToParameterizedSql(), item);
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
            var json = JsonConvert.SerializeObject(args);

            var item = (TModel)JsonConvert.DeserializeObject<TModel>(json)!;

            var properties =
                args.GetType().GetProperties();

            object[] values =
                new object[properties.Length];

            int index = 0;

            foreach (var property in properties)
            {
                string propertyName =
                    property.Name;

                values[index] =
                    $"{propertyName} = @{propertyName}";

                index++;
            }

            string table =
                GetTableName();

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
                    $"UPDATE {table} SET {string.Join(",", values)} ",
                    Query.ToString().AsSpan(match.Index)
                );

            Query = new StringBuilder(replaced);

            return await ExecuteCommandAsync(ToParameterizedSql(), item, cancellationToken).ConfigureAwait(false);
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

        #region Retrieving

        public TModel First()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QueryFirst<TModel>(
                ToParameterizedSql()
            );
        }

        public T First<T>()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QueryFirst<T>(
                ToParameterizedSql()
            );
        }

        public Task<TModel> FirstAsync()
        {
            return FirstAsync(
                CancellationToken.None
            );
        }

        public Task<TModel> FirstAsync(CancellationToken cancellationToken)
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QueryFirstAsync<TModel>(
                ToParameterizedSql(),
                cancellationToken: cancellationToken
            );
        }

        public Task<TResult> FirstAsync<TResult>()
        {
            return FirstAsync<TResult>(
                CancellationToken.None
            );
        }

        public Task<TResult> FirstAsync<TResult>(CancellationToken cancellationToken)
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QueryFirstAsync<TResult>(
                ToParameterizedSql(),
                cancellationToken: cancellationToken
            );
        }


        public TModel? FirstOrDefault()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QueryFirstOrDefault<TModel>(
                ToParameterizedSql()
            );
        }

        public T? FirstOrDefault<T>()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QueryFirstOrDefault<T>(
                ToParameterizedSql()
            );
        }

        public Task<TModel?> FirstOrDefaultAsync()
        {
            return FirstOrDefaultAsync(
                CancellationToken.None
            );
        }

        public Task<TModel?> FirstOrDefaultAsync(CancellationToken cancellationToken)
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QueryFirstOrDefaultAsync<TModel>(
                ToParameterizedSql(),
                cancellationToken:
                    cancellationToken
            );
        }

        public Task<TResult?> FirstOrDefaultAsync<TResult>()
        {
            return FirstOrDefaultAsync<TResult>(
                CancellationToken.None
            );
        }

        public Task<TResult?> FirstOrDefaultAsync<TResult>(CancellationToken cancellationToken)
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QueryFirstOrDefaultAsync<TResult>(
                ToParameterizedSql(),
                cancellationToken:
                    cancellationToken
            );
        }


        /*
         * ============================================================
         * SINGLE
         * ============================================================
         */

        public TModel Single()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QuerySingle<TModel>(
                ToParameterizedSql()
            );
        }


        public TResult Single<TResult>()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QuerySingle<TResult>(
                ToParameterizedSql()
            );
        }


        public Task<TModel> SingleAsync()
        {
            return SingleAsync(
                CancellationToken.None
            );
        }


        public Task<TModel> SingleAsync(
            CancellationToken cancellationToken)
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QuerySingleAsync<TModel>(
                ToParameterizedSql(),
                cancellationToken:
                    cancellationToken
            );
        }


        public Task<TResult> SingleAsync<TResult>()
        {
            return SingleAsync<TResult>(
                CancellationToken.None
            );
        }


        public Task<TResult> SingleAsync<TResult>(
            CancellationToken cancellationToken)
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QuerySingleAsync<TResult>(
                ToParameterizedSql(),
                cancellationToken:
                    cancellationToken
            );
        }

        /*
         * ============================================================
         * SINGLE OR DEFAULT
         * ============================================================
         */

        public TModel? SingleOrDefault()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QuerySingleOrDefault<TModel>(
                ToParameterizedSql()
            );
        }


        public TResult? SingleOrDefault<TResult>()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QuerySingleOrDefault<TResult>(
                ToParameterizedSql()
            );
        }


        public Task<TModel?> SingleOrDefaultAsync()
        {
            return SingleOrDefaultAsync(
                CancellationToken.None
            );
        }


        public Task<TModel?> SingleOrDefaultAsync(
            CancellationToken cancellationToken)
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QuerySingleOrDefaultAsync<TModel>(
                ToParameterizedSql(),
                cancellationToken:
                    cancellationToken
            );
        }


        public Task<TResult?> SingleOrDefaultAsync<TResult>()
        {
            return SingleOrDefaultAsync<TResult>(
                CancellationToken.None
            );
        }


        public Task<TResult?> SingleOrDefaultAsync<TResult>(
            CancellationToken cancellationToken)
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            return QuerySingleOrDefaultAsync<TResult>(
                ToParameterizedSql(),
                cancellationToken:
                    cancellationToken
            );
        }


        public List<TModel> ToList()
        {
            return QueryList<TModel>(
                ToParameterizedSql()
            );
        }

        public List<TResult> ToList<TResult>()
        {
            return QueryList<TResult>(
                ToParameterizedSql()
            );
        }

        public Task<List<TModel>> ToListAsync()
        {
            return ToListAsync(
                CancellationToken.None
            );
        }

        public Task<List<TModel>> ToListAsync(CancellationToken cancellationToken)
        {
            return QueryListAsync<TModel>(
                ToParameterizedSql(),
                cancellationToken:
                    cancellationToken
            );
        }

        public Task<List<TResult>> ToListAsync<TResult>()
        {
            return ToListAsync<TResult>(
                CancellationToken.None
            );
        }

        public Task<List<TResult>> ToListAsync<TResult>(CancellationToken cancellationToken)
        {
            return QueryListAsync<TResult>(
                ToParameterizedSql(),
                cancellationToken:
                    cancellationToken
            );
        }

        public bool Exists()
        {
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

        public Task<string> ValueAsync(
    string Column)
        {
            return ValueAsync(
                Column,
                CancellationToken.None
            );
        }

        public Task<string> ValueAsync(
            string Column,
            CancellationToken cancellationToken)
        {
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

        public Task<TValue> ValueAsync<TValue>(
            string Column)
        {
            return ValueAsync<TValue>(
                Column,
                CancellationToken.None
            );
        }

        public Task<TValue> ValueAsync<TValue>(
            string Column,
            CancellationToken cancellationToken)
        {
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

        public Task<List<TValue>> PluckAsync<TValue>(
            string Column,
            CancellationToken cancellationToken)
        {
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

        public Task<double> MaxAsync(
    string Column)
        {
            return MaxAsync(
                Column,
                CancellationToken.None
            );
        }

        public Task<double> MaxAsync(
            string Column,
            CancellationToken cancellationToken)
        {
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

        public Task<double> MinAsync(
            string Column)
        {
            return MinAsync(
                Column,
                CancellationToken.None
            );
        }

        public Task<double> MinAsync(
            string Column,
            CancellationToken cancellationToken)
        {
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

        public Task<double> AvgAsync(
            string Column,
            CancellationToken cancellationToken)
        {
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

        public Task<double> SumAsync(
            string Column)
        {
            return SumAsync(
                Column,
                CancellationToken.None
            );
        }

        public Task<double> SumAsync(
            string Column,
            CancellationToken cancellationToken)
        {
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

        public QueryBuilder<TModel> WhereDate(string Column, string Date)
        {
            InitWhere(Column, Date, null, LogicalOperators.DATE);
            return this;
        }

        public QueryBuilder<TModel> WhereYear(string Column, string Year)
        {
            InitWhere(Column, Year, null, LogicalOperators.WHEREYEAR);
            return this;
        }

        public QueryBuilder<TModel> WhereMonth(string Column, string Month)
        {
            InitWhere(Column, Month, null, LogicalOperators.WHEREMONTH);
            return this;
        }

        public QueryBuilder<TModel> WhereDay(string Column, string Day)
        {
            InitWhere(Column, Day, null, LogicalOperators.WHEREDAY);
            return this;
        }

        public QueryBuilder<TModel> OrWhereYear(string Column, string Year)
        {
            InitWhere(Column, Year, null, LogicalOperators.ORWHEREYEAR);
            return this;
        }

        public QueryBuilder<TModel> OrWhereMonth(string Column, string Month)
        {
            InitWhere(Column, Month, null, LogicalOperators.ORWHEREMONTH);
            return this;
        }

        public QueryBuilder<TModel> OrWhereDay(string Column, string Day)
        {
            InitWhere(Column, Day, null, LogicalOperators.ORWHEREDAY);
            return this;
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

        public QueryBuilder<TModel> WhereBetween(string Column, Between Value)
        {
            InitWhere(Column, Value, null, LogicalOperators.BETWEEN);
            return this;
        }

        public QueryBuilder<TModel> WhereNotBetween(string Column, Between Value)
        {
            InitWhere(Column, Value, null, LogicalOperators.NOT_BETWEEN);
            return this;
        }

        public QueryBuilder<TModel> WhereDateBetween(string Column, DateBetween Value)
        {
            InitWhere(Column, Value, null, LogicalOperators.DATEBETWEEN);
            return this;
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

            if (CountsRelationship.Count > 0)
            {
                throw new ApplicationException("Distinct method is incompatible with the WithCount method");
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

            if (CountsRelationship.Count > 0)
            {
                throw new ApplicationException(
                    "Distinct method is incompatible " +
                    "with the WithCount method"
                );
            }

            Query.Replace(
                "_selector_all",
                $"{Clauses.DISTINCT} {Columns}"
            );

            return this;
        }

        public QueryBuilder<TModel> WithCount(string Relationship, string? Alias = null)
        {

            try
            {
                if (!CheckQueryInit())
                {
                    SimpleQuery();
                }

                if (Query != null && Query.ToString().Contains(Clauses.DISTINCT.ToString()))
                {
                    throw new ApplicationException("WithCount method is incompatible with the Distinct method");
                }

                var property = Instance.GetType().GetProperty(Relationship);
                var method = Instance.GetType().GetMethod(Relationship);

                dynamic? result = null;

                if (property != null)
                {
                    result = property.GetValue(Instance)!;
                }
                else if (method != null)
                {
                    result = method.Invoke(Instance, null)!;
                }

                var parts = result!.GetQuery().Split("=");

                string OwnTable = GetTableName();

                string countQuery = string.Concat(parts[0], $" = {OwnTable}.{result.LocalKey}").Replace("_selector_all", "count(*)");

                var regex = new Regex(Regex.Escape("_selector_all"));
                var match = regex.Match(Query!.ToString());

                int IndexSelector = match.Index + "_selector_all".Length;

                string alias = Alias ?? $"{Relationship}_Count";

                CountsRelationship.Add($", ({countQuery}) as {alias} ");

                string replaced = Query.ToString().Insert(IndexSelector, $" count_relationship_{CountsRelationship.Count} ");

                Query = new(replaced);
            }
            catch (NullReferenceException)
            {
                throw new RelationshipException(
                    $"Relationship '{Relationship}' was not found " +
                    $"on model '{Instance.GetType().Name}'."
                );
            }


            return this;
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
