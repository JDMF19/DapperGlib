using Dapper;
using DapperGlib.Exceptions;
using DapperGlib.Interfaces;
using DapperGlib.Util;
using Newtonsoft.Json;
using System.Reflection;
using System.Threading;

namespace DapperGlib
{

    public abstract class Model<T> : IModel where T : Model<T>, new()
    {


        internal static readonly GlipContext _context = new();

        public virtual bool Incrementing { get; } = true;
        public virtual string? Table { get; }
        public virtual string? Schema { get; }
        public virtual string Connection { get; set; } = "SqlConnection";


        protected static readonly object Instance = Activator.CreateInstance(typeof(T)) ?? (new());

        public Model()
        {

        }

        #region CRUD
        public void Insert()
        {
            var builder =
                new QueryBuilder<T>()
                    .InsertQuery(this);

            if (QueryBuilder<T>.IsIncrementing())
            {
                object? primaryKeyValue =
                    builder.ExecuteScalar<object?>(
                        builder.ToParameterizedSql(),
                        this
                    );

                AssignGeneratedPrimaryKey(
                    this,
                    primaryKeyValue
                );

                return;
            }

            builder.ExecuteCommand(
                builder.ToParameterizedSql(),
                this
            );
        }

        public Task InsertAsync()
        {
            return InsertAsync(
                CancellationToken.None
            );
        }

        public async Task InsertAsync(
            CancellationToken cancellationToken)
        {
            var builder =
                new QueryBuilder<T>()
                    .InsertQuery(this);

            if (QueryBuilder<T>.IsIncrementing())
            {
                object? primaryKeyValue =
                    await builder
                        .ExecuteScalarAsync<object?>(
                            builder.ToParameterizedSql(),
                            this,
                            cancellationToken
                        )
                        .ConfigureAwait(false);

                AssignGeneratedPrimaryKey(
                    this,
                    primaryKeyValue
                );

                return;
            }

            await builder
                .ExecuteCommandAsync(
                    builder.ToParameterizedSql(),
                    this,
                    cancellationToken
                )
                .ConfigureAwait(false);
        }

        public static T Create(T Item)
        {
            if (Item == null)
            {
                throw new ArgumentNullException(
                    nameof(Item)
                );
            }

            var builder =
                new QueryBuilder<T>()
                    .InsertQuery(Item);

            if (QueryBuilder<T>.IsIncrementing())
            {
                object? primaryKeyValue =
                    builder.ExecuteScalar<object?>(
                        builder.ToParameterizedSql(),
                        Item
                    );

                AssignGeneratedPrimaryKey(
                    Item,
                    primaryKeyValue
                );
            }
            else
            {
                builder.ExecuteCommand(
                    builder.ToParameterizedSql(),
                    Item
                );
            }

            return Item;
        }

        public static Task<T> CreateAsync(T Item)
        {
            return CreateAsync(
                Item,
                CancellationToken.None
            );
        }

        public static async Task<T> CreateAsync(T Item, CancellationToken cancellationToken)
        {
            if (Item == null)
            {
                throw new ArgumentNullException(
                    nameof(Item)
                );
            }

            var builder =
                new QueryBuilder<T>()
                    .InsertQuery(Item);

            if (QueryBuilder<T>.IsIncrementing())
            {
                object? primaryKeyValue =
                    await builder
                        .ExecuteScalarAsync<object?>(
                            builder.ToParameterizedSql(),
                            Item,
                            cancellationToken
                        )
                        .ConfigureAwait(false);

                AssignGeneratedPrimaryKey(
                    Item,
                    primaryKeyValue
                );
            }
            else
            {
                await builder
                    .ExecuteCommandAsync(
                        builder.ToParameterizedSql(),
                        Item,
                        cancellationToken
                    )
                    .ConfigureAwait(false);
            }

            return Item;
        }

        public static void UpdateAll(dynamic args)
        {
            new QueryBuilder<T>().Update(args);
        }

        public static Task<int> UpdateAllAsync(dynamic args)
        {
            return new QueryBuilder<T>().UpdateAsync(args);
        }

        public static Task<int> UpdateAllAsync(dynamic args, CancellationToken cancellationToken)
        {
            return new QueryBuilder<T>()
                .UpdateAsync(
                    args,
                    cancellationToken
                );
        }


        public void Update()
        {
            var builder =
                new QueryBuilder<T>()
                    .UpdateQuery(this);

            builder.ExecuteCommand(
                builder.ToParameterizedSql(),
                this
            );
        }

        public Task UpdateAsync()
        {
            return UpdateAsync(
                CancellationToken.None
            );
        }

        public async Task UpdateAsync(
            CancellationToken cancellationToken)
        {
            var builder =
                new QueryBuilder<T>()
                    .UpdateQuery(this);

            await builder
                .ExecuteCommandAsync(
                    builder.ToParameterizedSql(),
                    this,
                    cancellationToken
                )
                .ConfigureAwait(false);
        }

        public void Update(dynamic args)
        {
            var operation =
                PrepareDynamicUpdate(
                    (object)args
                );

            operation.Builder
                .ExecuteCommand(
                    operation.Builder
                        .ToParameterizedSql(),
                    operation.Item
                );

            ApplyDynamicUpdateValues(
                (object)args,
                operation.Properties
            );
        }

        public Task UpdateAsync(dynamic args)
        {
            return UpdateDynamicAsyncCore(
                (object)args,
                CancellationToken.None
            );
        }

        public Task UpdateAsync(dynamic args, CancellationToken cancellationToken)
        {
            return UpdateDynamicAsyncCore(
                (object)args,
                cancellationToken
            );
        }

        private async Task UpdateDynamicAsyncCore(object args, CancellationToken cancellationToken)
        {
            var operation =
                PrepareDynamicUpdate(
                    args
                );

            await operation.Builder
                .ExecuteCommandAsync(
                    operation.Builder
                        .ToParameterizedSql(),
                    operation.Item,
                    cancellationToken
                )
                .ConfigureAwait(false);

            ApplyDynamicUpdateValues(
                args,
                operation.Properties
            );
        }

        public void Delete()
        {
            new QueryBuilder<T>().SimpleDelete(this);
        }

        public Task<int> DeleteAsync()
        {
            return new QueryBuilder<T>().SimpleDeleteAsync(this);
        }

        public Task<int> DeleteAsync(CancellationToken cancellationToken)
        {
            return new QueryBuilder<T>()
                .SimpleDeleteAsync(
                    this,
                    cancellationToken
                );
        }

        public static void Truncate()
        {
            new QueryBuilder<T>().Truncate();
        }

        public static Task<int> TruncateAsync()
        {
            return new QueryBuilder<T>()
                .TruncateAsync();
        }

        public static Task<int> TruncateAsync(
            CancellationToken cancellationToken)
        {
            return new QueryBuilder<T>()
                .TruncateAsync(
                    cancellationToken
                );
        }

        #endregion

        #region Retrieving
        public static T First()
        {
            var Builder = new QueryBuilder<T>();
            return Builder.First();
        }

        public static T? FirstOrDefault()
        {
            var Builder = new QueryBuilder<T>();
            return Builder.FirstOrDefault();
        }

        public static Task<T> FirstAsync()
        {
            var builder =
                new QueryBuilder<T>();

            return builder.FirstAsync();
        }

        public static Task<T> FirstAsync(CancellationToken cancellationToken)
        {
            var builder =
                new QueryBuilder<T>();

            return builder.FirstAsync(cancellationToken);
        }

        public static Task<T?> FirstOrDefaultAsync()
        {
            var builder =
                new QueryBuilder<T>();

            return builder.FirstOrDefaultAsync();
        }

        public static Task<T?> FirstOrDefaultAsync(
            CancellationToken cancellationToken)
        {
            var builder =
                new QueryBuilder<T>();

            return builder.FirstOrDefaultAsync(
                cancellationToken
            );
        }

        /*
         * ============================================================
         * SINGLE
         * ============================================================
         */

        public static T Single()
        {
            return new QueryBuilder<T>()
                .Single();
        }


        public static Task<T> SingleAsync()
        {
            return new QueryBuilder<T>()
                .SingleAsync();
        }


        public static Task<T> SingleAsync(
            CancellationToken cancellationToken)
        {
            return new QueryBuilder<T>()
                .SingleAsync(
                    cancellationToken
                );
        }


        /*
         * ============================================================
         * SINGLE OR DEFAULT
         * ============================================================
         */

        public static T? SingleOrDefault()
        {
            return new QueryBuilder<T>()
                .SingleOrDefault();
        }


        public static Task<T?> SingleOrDefaultAsync()
        {
            return new QueryBuilder<T>()
                .SingleOrDefaultAsync();
        }


        public static Task<T?> SingleOrDefaultAsync(
            CancellationToken cancellationToken)
        {
            return new QueryBuilder<T>()
                .SingleOrDefaultAsync(
                    cancellationToken
                );
        }



        /*
         * ============================================================
         * FIND
         * ============================================================
         */

        // Compatibilidad existente: int
        public static T Find(int Id)
        {
            return FindByKey(Id);
        }

        // Compatibilidad existente: string
        public static T Find(string Id)
        {
            return FindByKey(Id);
        }

        // Nuevo: cualquier tipo de PK
        public static T Find<TKey>(TKey Id)
        {
            return FindByKey(Id);
        }


        /*
         * ============================================================
         * FIND ASYNC
         * ============================================================
         */

        // Compatibilidad existente: int
        public static Task<T?> FindAsync(int Id)
        {
            return FindByKeyAsync(
                Id,
                CancellationToken.None
            );
        }

        public static Task<T?> FindAsync(
            int Id,
            CancellationToken cancellationToken)
        {
            return FindByKeyAsync(
                Id,
                cancellationToken
            );
        }

        // Compatibilidad existente: string
        public static Task<T?> FindAsync(string Id)
        {
            return FindByKeyAsync(
                Id,
                CancellationToken.None
            );
        }

        public static Task<T?> FindAsync(
            string Id,
            CancellationToken cancellationToken)
        {
            return FindByKeyAsync(
                Id,
                cancellationToken
            );
        }

        // Nuevo: cualquier PK
        public static Task<T?> FindAsync<TKey>(
            TKey Id)
        {
            return FindByKeyAsync(
                Id,
                CancellationToken.None
            );
        }

        public static Task<T?> FindAsync<TKey>(
            TKey Id,
            CancellationToken cancellationToken)
        {
            return FindByKeyAsync(
                Id,
                cancellationToken
            );
        }


        /*
         * ============================================================
         * FIND OR DEFAULT
         * ============================================================
         */

        // Compatibilidad existente: int
        public static T? FindOrDefault(int Id)
        {
            return FindOrDefaultByKey(Id);
        }

        // Compatibilidad existente: string
        public static T? FindOrDefault(string Id)
        {
            return FindOrDefaultByKey(Id);
        }

        // Nuevo: cualquier PK
        public static T? FindOrDefault<TKey>(
            TKey Id)
        {
            return FindOrDefaultByKey(Id);
        }


        /*
         * ============================================================
         * FIND OR DEFAULT ASYNC
         * ============================================================
         */

        // Compatibilidad existente: int
        public static Task<T?> FindOrDefaultAsync(
            int Id)
        {
            return FindOrDefaultByKeyAsync(
                Id,
                CancellationToken.None
            );
        }

        public static Task<T?> FindOrDefaultAsync(
            int Id,
            CancellationToken cancellationToken)
        {
            return FindOrDefaultByKeyAsync(
                Id,
                cancellationToken
            );
        }

        // Compatibilidad existente: string
        public static Task<T?> FindOrDefaultAsync(
            string Id)
        {
            return FindOrDefaultByKeyAsync(
                Id,
                CancellationToken.None
            );
        }

        public static Task<T?> FindOrDefaultAsync(
            string Id,
            CancellationToken cancellationToken)
        {
            return FindOrDefaultByKeyAsync(
                Id,
                cancellationToken
            );
        }

        // Nuevo: cualquier PK
        public static Task<T?> FindOrDefaultAsync<TKey>(
            TKey Id)
        {
            return FindOrDefaultByKeyAsync(
                Id,
                CancellationToken.None
            );
        }

        public static Task<T?> FindOrDefaultAsync<TKey>(
            TKey Id,
            CancellationToken cancellationToken)
        {
            return FindOrDefaultByKeyAsync(
                Id,
                cancellationToken
            );
        }



        public static List<T> ToList()
        {
            var Builder = new QueryBuilder<T>().SimpleQuery();

            return Builder.ToList();
        }

        public static Task<List<T>> ToListAsync()
        {
            var Builder = new QueryBuilder<T>().SimpleQuery();

            return Builder.ToListAsync();
        }


        public static Task<List<T>> ToListAsync(CancellationToken cancellationToken)
        {
            var builder =
                new QueryBuilder<T>()
                    .SimpleQuery();

            return builder.ToListAsync(
                cancellationToken
            );
        }

        public static int Count()
        {
            var Builder = new QueryBuilder<T>().SimpleQuery();
            return Builder.Count();
        }

        public static Task<int> CountAsync()
        {
            return CountAsync(
                CancellationToken.None
            );
        }

        public static Task<int> CountAsync(
            CancellationToken cancellationToken)
        {
            var builder =
                new QueryBuilder<T>()
                    .SimpleQuery();

            return builder.CountAsync(
                cancellationToken
            );
        }

        public static string Value(string Column)
        {
            var Builder = new QueryBuilder<T>();
            return Builder.Value(Column);
        }

        public static TValue Value<TValue>(string Column)
        {
            var Builder =
                new QueryBuilder<T>();

            return Builder.Value<TValue>(
                Column
            );
        }

        public static Task<string> ValueAsync(string Column)
        {
            return new QueryBuilder<T>()
                .ValueAsync(
                    Column
                );
        }

        public static Task<string> ValueAsync(
            string Column,
            CancellationToken cancellationToken)
        {
            return new QueryBuilder<T>()
                .ValueAsync(
                    Column,
                    cancellationToken
                );
        }


        public static Task<TValue> ValueAsync<TValue>(string Column)
        {
            return new QueryBuilder<T>()
                .ValueAsync<TValue>(
                    Column
                );
        }

        public static Task<TValue> ValueAsync<TValue>(string Column, CancellationToken cancellationToken)
        {
            return new QueryBuilder<T>()
                .ValueAsync<TValue>(
                    Column,
                    cancellationToken
                );
        }

        public static List<TValue> Pluck<TValue>(string Column)
        {
            var Builder =
                new QueryBuilder<T>();

            return Builder.Pluck<TValue>(
                Column
            );
        }

        public static Task<List<TValue>> PluckAsync<TValue>(string Column)
        {
            return new QueryBuilder<T>()
                .PluckAsync<TValue>(
                    Column
                );
        }

        public static Task<List<TValue>> PluckAsync<TValue>(
            string Column,
            CancellationToken cancellationToken)
        {
            return new QueryBuilder<T>()
                .PluckAsync<TValue>(
                    Column,
                    cancellationToken
                );
        }

        public static double Max(string Column)
        {
            var Builder = new QueryBuilder<T>();
            return Builder.Max(Column);
        }

        public static Task<double> MaxAsync(string Column)
        {
            return new QueryBuilder<T>()
                .MaxAsync(
                    Column
                );
        }

        public static Task<double> MaxAsync(string Column, CancellationToken cancellationToken)
        {
            return new QueryBuilder<T>()
                .MaxAsync(
                    Column,
                    cancellationToken
                );
        }

        public static double Min(string Column)
        {
            var Builder = new QueryBuilder<T>();
            return Builder.Min(Column);
        }

        public static Task<double> MinAsync(string Column)
        {
            return new QueryBuilder<T>()
                .MinAsync(
                    Column
                );
        }

        public static Task<double> MinAsync(string Column, CancellationToken cancellationToken)
        {
            return new QueryBuilder<T>()
                .MinAsync(
                    Column,
                    cancellationToken
                );
        }

        public static double Avg(string Column)
        {
            var Builder = new QueryBuilder<T>();
            return Builder.Avg(Column);
        }

        public static Task<double> AvgAsync(string Column)
        {
            return new QueryBuilder<T>()
                .AvgAsync(
                    Column
                );
        }

        public static Task<double> AvgAsync(string Column, CancellationToken cancellationToken)
        {
            return new QueryBuilder<T>()
                .AvgAsync(
                    Column,
                    cancellationToken
                );
        }

        public static double Sum(string Column)
        {
            var Builder = new QueryBuilder<T>();
            return Builder.Sum(Column);
        }

        public static Task<double> SumAsync(string Column)
        {
            return new QueryBuilder<T>()
                .SumAsync(
                    Column
                );
        }

        public static Task<double> SumAsync(string Column, CancellationToken cancellationToken)
        {
            return new QueryBuilder<T>()
                .SumAsync(
                    Column,
                    cancellationToken
                );
        }

        public static string GetTableName()
        {
            return QueryBuilder<T>.GetTableName();
        }

        public static string GetConnectionString()
        {
            return QueryBuilder<T>.GetConnectionString();
        }

        #endregion

        public static QueryBuilder<T> Query()
        {
            var Builder = new QueryBuilder<T>().SimpleQuery();

            return Builder;
        }

        public static QueryBuilder<T> Timeout(int seconds)
        {
            return new QueryBuilder<T>()
                .SimpleQuery()
                .Timeout(seconds);
        }

        public static QueryBuilder<T> Select(params string[] Columns)
        {
            var Builder = new QueryBuilder<T>().SimpleQuery();

            Builder.Select(Columns);

            return Builder;
        }

        public static QueryBuilder<T> Skip(int Rows)
        {
            var Builder = new QueryBuilder<T>().SimpleQuery();

            Builder.Skip(Rows);

            return Builder;

        }

        public static QueryBuilder<T> Take(int Rows)
        {
            var Builder = new QueryBuilder<T>().SimpleQuery();

            Builder.Take(Rows);

            return Builder;

        }

        public static QueryBuilder<T> Raw(string Query)
        {
            QueryBuilder<T> Builder = new(Query);
            return Builder;
        }

        public static QueryBuilder<T> Where(string Column, object? Value)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().Where(Column, Value);

            return Builder;
        }

        public static QueryBuilder<T> Where(string Column, string ComparisonOperator, object? Value)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().Where(Column, ComparisonOperator, Value);

            return Builder;
        }

        public static QueryBuilder<T> Where(Func<SubQuery<T>, SubQuery<T>> Builder)
        {
            QueryBuilder<T> Builder_ = new QueryBuilder<T>();

            Builder_.Where(Builder);

            return Builder_;
        }

        public static QueryBuilder<T> WhereLike(string Column, string Pattern)
        {
            return new QueryBuilder<T>()
                .WhereLike(
                    Column,
                    Pattern
                );
        }

        public static QueryBuilder<T> WhereContains(string Column, string Value)
        {
            return new QueryBuilder<T>()
                .WhereContains(
                    Column,
                    Value
                );
        }

        public static QueryBuilder<T> WhereNot(Func<SubQuery<T>, SubQuery<T>> Builder)
        {
            QueryBuilder<T> Builder_ = new QueryBuilder<T>();

            Builder_.WhereNot(Builder);

            return Builder_;
        }

        public static QueryBuilder<T> WhereIn<TValue>(string column, IEnumerable<TValue> values)
        {
            return new QueryBuilder<T>().WhereIn(column, values);
        }

        public static QueryBuilder<T> WhereNotIn<TValue>(string column, IEnumerable<TValue> values)
        {
            return new QueryBuilder<T>().WhereNotIn(column, values);
        }

        public static QueryBuilder<T> WhereNull(string Column)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereNull(Column);

            return Builder;
        }

        public static QueryBuilder<T> WhereNotNull(string Column)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereNotNull(Column);

            return Builder;
        }

        public static QueryBuilder<T> WhereDate(string Column, string Date)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereDate(Column, Date);
            return Builder;
        }

        public static QueryBuilder<T> WhereYear(string Column, string Year)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereYear(Column, Year);
            return Builder;
        }

        public static QueryBuilder<T> WhereMonth(string Column, string Month)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereMonth(Column, Month);
            return Builder;
        }

        public static QueryBuilder<T> WhereDay(string Column, string Day)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereDay(Column, Day);
            return Builder;
        }

        /// <summary>
        ///     
        /// </summary>
        /// <param name="Invert">Reverses the order in the query of the Column and Date parameters</param>
        /// <param name="ComparisonType">The comparison types are Year, Month, Day, Minute</param>
        public static QueryBuilder<T> WhereDateDiff(string Column, string Date, int Difference, DateDiff ComparisonType, bool Invert = false)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereDateDiff(Column, Date, Difference, ComparisonType, Invert);
            return Builder;
        }

        /// <summary>
        ///     
        /// </summary>
        /// <param name="Invert">Reverses the order in the query of the Column and Date parameters</param>
        /// <param name="ComparisonType">The comparison types are Year, Month, Day, Minute</param>
        public static QueryBuilder<T> WhereDateDiff(string Column, string Date, string ComparisonOperator, int Difference, DateDiff ComparisonType, bool Invert = false)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereDateDiff(Column, Date, ComparisonOperator, Difference, ComparisonType, Invert);
            return Builder;
        }

        public static QueryBuilder<T> WhereColumn(string FirstColumn, string SecondColumn)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereColumn(FirstColumn, SecondColumn);
            return Builder;
        }

        public static QueryBuilder<T> WhereColumn(string FirstColumn, string ComparisonOperator, string SecondColumn)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereColumn(FirstColumn, ComparisonOperator, SecondColumn);

            return Builder;
        }

        public static QueryBuilder<T> WhereBetween(string Column, Between Value)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereBetween(Column, Value);
            return Builder;
        }

        public static QueryBuilder<T> WhereNotBetween(string Column, Between Value)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereNotBetween(Column, Value);
            return Builder;
        }

        public static QueryBuilder<T> WhereDateBetween(string Column, DateBetween Value)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereDateBetween(Column, Value);
            return Builder;
        }

        public static QueryBuilder<T> WhereHas<TRelationship>(string Relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? Builder = null)
        {
            QueryBuilder<T> Builder_ = new QueryBuilder<T>("");

            Builder_.WhereHas(Relationship, Builder);

            return Builder_;
        }

        public static QueryBuilder<T> WhereHas<TRelationship>(string Relationship, string ComparisonOperator, int Value)
        {
            QueryBuilder<T> Builder_ = new QueryBuilder<T>("");

            Builder_.WhereHas<TRelationship>(Relationship, ComparisonOperator, Value);

            return Builder_;
        }

        public static QueryBuilder<T> WhereHas<TRelationship>(string Relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>> Builder, string ComparisonOperator, int Value)
        {
            QueryBuilder<T> Builder_ = new QueryBuilder<T>("");

            Builder_.WhereHas(Relationship, Builder, ComparisonOperator, Value);

            return Builder_;
        }

        public static QueryBuilder<T> WhereDoesntHave<TRelationship>(string Relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? Builder = null)
        {
            QueryBuilder<T> Builder_ = new QueryBuilder<T>("");

            Builder_.WhereDoesntHave(Relationship, Builder);

            return Builder_;
        }

        public static QueryBuilder<T> When(bool Condition, Func<SubQuery<T>, SubQuery<T>>? Builder = null)
        {

            QueryBuilder<T> Builder_ = new QueryBuilder<T>().SimpleQuery();

            Builder_.When(Condition, Builder);

            return Builder_;
        }

        public static QueryBuilder<T> Distinct()
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().Distinct();

            return Builder;
        }

        public static QueryBuilder<T> Distinct(string Columns)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().Distinct(Columns);

            return Builder;
        }

        public static QueryBuilder<T> WithCount(string Relationship)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().WithCount(Relationship);

            return Builder;
        }

        public static QueryBuilder<T> OrderBy(string Column, string Direction = "ASC")
        {
            QueryBuilder<T> Builder = new();
            Builder.OrderBy(Column, Direction);
            return Builder;
        }

        public static QueryBuilder<T> InRandomOrder()
        {
            QueryBuilder<T> Builder = new();
            Builder.InRandomOrder();
            return Builder;
        }

        public Relationship<TRelationship> HasRelationship<TRelationship>(Relationship<TRelationship> relationship)
        {
            PropertyInfo? localProperty = this.GetType().GetProperty(relationship.LocalKey);

            if (localProperty == null)
            {
                throw new RelationshipException(
                    $"Relationship configuration error on model '{GetType().Name}'. " +
                    $"Local key property '{relationship.LocalKey}' was not found."
                );
            }

            var localValue = localProperty.GetValue(this);

            relationship.Bind(localValue);

            return relationship;
        }



        // Helpers

        private static string GetRequiredPrimaryKeyName()
        {
            string? primaryKey = QueryBuilder<T>.GetPrimaryKey();

            if (primaryKey == null)
            {
                throw new ModelConfigurationException(
                    $"Primary key is not defined for model " +
                    $"'{typeof(T).Name}'. " +
                    $"Add the [PrimaryKey] attribute to the appropriate property."
                );
            }

            return primaryKey;
        }

        private static T? FindOrDefaultByKey<TKey>(TKey id)
        {
            string primaryKey =
                GetRequiredPrimaryKeyName();

            var builder =
                new QueryBuilder<T>()
                    .Where(
                        primaryKey,
                        id
                    );

            return builder
                .QueryFirstOrDefault<T>(
                    builder.ToParameterizedSql()
                );
        }

        private static T FindByKey<TKey>(
            TKey id)
        {
            T? item =
                FindOrDefaultByKey(id);

            if (item == null)
            {
                throw new ModelNotFoundException(
                    typeof(T),
                    id!
                );
            }

            return item;
        }

        private static Task<T?> FindOrDefaultByKeyAsync<TKey>(TKey id, CancellationToken cancellationToken)
        {
            string primaryKey =
                GetRequiredPrimaryKeyName();

            var builder =
                new QueryBuilder<T>()
                    .Where(
                        primaryKey,
                        id
                    );

            return builder
                .QueryFirstOrDefaultAsync<T>(
                    builder.ToParameterizedSql(),
                    cancellationToken:
                        cancellationToken
                );
        }

        private static async Task<T?> FindByKeyAsync<TKey>(TKey id, CancellationToken cancellationToken)
        {
            T? item =
                await FindOrDefaultByKeyAsync(
                    id,
                    cancellationToken
                )
                .ConfigureAwait(false);

            if (item == null)
            {
                throw new ModelNotFoundException(
                    typeof(T),
                    id!
                );
            }

            return item;
        }

        private static PropertyInfo GetRequiredPrimaryKeyProperty()
        {
            PropertyInfo? primaryKey =
                QueryBuilder<T>.GetPropertyInfoByAttribute(
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

            return primaryKey;
        }

        private static void AssignGeneratedPrimaryKey(object model, object? value)
        {
            PropertyInfo primaryKey =
                GetRequiredPrimaryKeyProperty();

            if (!primaryKey.CanWrite)
            {
                throw new ModelConfigurationException(
                    $"Primary key property '{primaryKey.Name}' " +
                    $"on model '{typeof(T).Name}' is read-only."
                );
            }

            if (value == null ||
                value == DBNull.Value)
            {
                throw new ModelConfigurationException(
                    $"The database did not return a generated value " +
                    $"for primary key '{primaryKey.Name}' " +
                    $"on model '{typeof(T).Name}'."
                );
            }

            Type destinationType =
                Nullable.GetUnderlyingType(
                    primaryKey.PropertyType
                )
                ?? primaryKey.PropertyType;

            object convertedValue;

            try
            {
                if (destinationType.IsInstanceOfType(value))
                {
                    convertedValue =
                        value;
                }
                else if (destinationType == typeof(Guid))
                {
                    convertedValue =
                        value is Guid guid
                            ? guid
                            : Guid.Parse(
                                value.ToString()!
                            );
                }
                else if (destinationType.IsEnum)
                {
                    convertedValue =
                        value is string enumName
                            ? Enum.Parse(
                                destinationType,
                                enumName,
                                true
                            )
                            : Enum.ToObject(
                                destinationType,
                                value
                            );
                }
                else
                {
                    convertedValue =
                        Convert.ChangeType(
                            value,
                            destinationType,
                            System.Globalization
                                .CultureInfo
                                .InvariantCulture
                        );
                }
            }
            catch (Exception ex)
            {
                throw new ModelConfigurationException(
                    $"Unable to convert generated primary key " +
                    $"value '{value}' from type " +
                    $"'{value.GetType().Name}' to " +
                    $"'{primaryKey.PropertyType.Name}' " +
                    $"for model '{typeof(T).Name}'.",
                    ex
                );
            }

            primaryKey.SetValue(
                model,
                convertedValue
            );
        }


        private (QueryBuilder<T> Builder, T Item, PropertyInfo[] Properties) PrepareDynamicUpdate(object args)
        {
            PropertyInfo? primaryAttribute =
                QueryBuilder<T>
                    .GetPropertyInfoByAttribute(
                        typeof(PrimaryKey)
                    );

            if (primaryAttribute == null)
            {
                /*
                 * Se conserva ArgumentException por compatibilidad
                 * con el comportamiento público anterior.
                 */
                throw new ArgumentException(
                    "Column Primary Key not found"
                );
            }

            object? primaryKeyValue =
                primaryAttribute.GetValue(
                    this,
                    null
                );

            string json =
                JsonConvert.SerializeObject(
                    args
                );

            T item =
                JsonConvert
                    .DeserializeObject<T>(
                        json
                    )!;

            primaryAttribute.SetValue(
                item,
                primaryKeyValue
            );

            var builder =
                new QueryBuilder<T>();

            builder.UpdateDynamicQuery<T>(
                args
            );

            PropertyInfo[] properties =
                args.GetType()
                    .GetProperties();

            return (
                builder,
                item,
                properties
            );
        }

        private void ApplyDynamicUpdateValues(object args, IEnumerable<PropertyInfo> properties)
        {
            foreach (var property in properties)
            {
                PropertyInfo? ownProperty =
                    GetType()
                        .GetProperty(
                            property.Name
                        );

                if (ownProperty != null &&
                    ownProperty.CanWrite)
                {
                    ownProperty.SetValue(
                        this,
                        property.GetValue(
                            args,
                            null
                        )
                    );
                }
            }
        }

    }

}
