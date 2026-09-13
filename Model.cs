using Dapper;
using DapperGlib.Exceptions;
using DapperGlib.Interfaces;
using DapperGlib.Util;
using System.Reflection;
using DapperGlib.Internal;
using Microsoft.Data.SqlClient;
using System.Threading;
using DapperGlib.Relationships;
using System.Linq.Expressions;

namespace DapperGlib
{

    public abstract class Model<T> : IModel, IRelationshipLoadState where T : Model<T>, new()
    {


        internal static readonly GlipContext _context = new();

        internal static readonly DatabaseCommandExecutor _executor = new(_context);

        public virtual bool Incrementing { get; } = true;
        public virtual string? Table { get; }
        public virtual string? Schema { get; }
        public virtual string Connection { get; set; } = "SqlConnection";


        protected static readonly object Instance = Activator.CreateInstance(typeof(T)) ?? (new());

        private readonly HashSet<string> _loadedRelationships = new(StringComparer.Ordinal);

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

        public async Task InsertAsync(CancellationToken cancellationToken)
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

        public static T Create(object args)
        {
            var operation = PrepareDynamicCreate(args);

            if (QueryBuilder<T>.IsIncrementing())
            {
                object? primaryKeyValue = operation.Builder.ExecuteScalar<object?>(operation.Builder.ToParameterizedSql(), operation.Parameters);

                AssignGeneratedPrimaryKey(operation.Item, primaryKeyValue);
            }
            else
            {
                operation.Builder.ExecuteCommand(operation.Builder.ToParameterizedSql(), operation.Parameters);
            }

            return operation.Item;
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

        public static Task<T> CreateAsync(object args)
        {
            return CreateDynamicAsyncCore(args, CancellationToken.None);
        }

        public static Task<T> CreateAsync(object args, CancellationToken cancellationToken)
        {
            return CreateDynamicAsyncCore(args, cancellationToken);
        }


        public static List<T> CreateMany(IEnumerable<T> items)
        {
            List<T> itemsList = PrepareManyItems(items, nameof(CreateMany));

            if (itemsList.Count == 0)
            {
                return new List<T>();
            }

            ExecuteCreateMany(itemsList);

            return itemsList;
        }

        public static Task<List<T>> CreateManyAsync(IEnumerable<T> items)
        {
            return CreateManyAsync(items, CancellationToken.None);
        }

        public static async Task<List<T>> CreateManyAsync(IEnumerable<T> items, CancellationToken cancellationToken)
        {
            List<T> itemsList = PrepareManyItems(items, nameof(CreateManyAsync));

            if (itemsList.Count == 0)
            {
                return new List<T>();
            }

            await ExecuteCreateManyAsync(itemsList, cancellationToken).ConfigureAwait(false);

            return itemsList;
        }

        public static void InsertMany(IEnumerable<T> items)
        {
            List<T> itemsList = PrepareManyItems(items, nameof(InsertMany));

            if (itemsList.Count == 0)
            {
                return;
            }

            ExecuteInsertMany(itemsList);
        }

        public static Task InsertManyAsync(IEnumerable<T> items)
        {
            return InsertManyAsync(items, CancellationToken.None);
        }

        public static Task InsertManyAsync(IEnumerable<T> items, CancellationToken cancellationToken)
        {
            List<T> itemsList = PrepareManyItems(items, nameof(InsertManyAsync));

            if (itemsList.Count == 0)
            {
                return Task.CompletedTask;
            }

            return ExecuteInsertManyAsync(itemsList, cancellationToken);
        }

        public static void BulkInsert(IEnumerable<T> items)
        {
            IReadOnlyList<T> itemsList = PrepareBulkItems(items, nameof(BulkInsert));

            if (itemsList.Count == 0)
            {
                return;
            }

            ExecuteBulkInsert(itemsList);
        }

        public static Task BulkInsertAsync(IEnumerable<T> items)
        {
            return BulkInsertAsync(items, CancellationToken.None);
        }

        public static Task BulkInsertAsync(IEnumerable<T> items, CancellationToken cancellationToken)
        {
            IReadOnlyList<T> itemsList = PrepareBulkItems(items, nameof(BulkInsertAsync));

            if (itemsList.Count == 0)
            {
                return Task.CompletedTask;
            }

            return ExecuteBulkInsertAsync(itemsList, cancellationToken);
        }


        private static async Task<T> CreateDynamicAsyncCore(object args, CancellationToken cancellationToken)
        {
            var operation = PrepareDynamicCreate(args);

            if (QueryBuilder<T>.IsIncrementing())
            {
                object? primaryKeyValue = await operation.Builder.ExecuteScalarAsync<object?>(operation.Builder.ToParameterizedSql(), operation.Parameters, cancellationToken).ConfigureAwait(false);

                AssignGeneratedPrimaryKey(operation.Item, primaryKeyValue);
            }
            else
            {
                await operation.Builder.ExecuteCommandAsync(operation.Builder.ToParameterizedSql(), operation.Parameters, cancellationToken).ConfigureAwait(false);
            }

            return operation.Item;
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
            var operation = PrepareDynamicUpdate((object)args);

            operation.Builder.ExecuteCommand(operation.Builder.ToParameterizedSql(), operation.Parameters);

            ApplyDynamicUpdateValues((object)args, operation.Properties);
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
            var operation = PrepareDynamicUpdate(args);

            await operation.Builder.ExecuteCommandAsync(operation.Builder.ToParameterizedSql(), operation.Parameters, cancellationToken).ConfigureAwait(false);

            ApplyDynamicUpdateValues(args, operation.Properties);
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


        public static QueryBuilder<T> With(string relationship)
        {
            return new QueryBuilder<T>().With(relationship);
        }

        public static QueryBuilder<T> With<TRelated>(Expression<Func<T, IEnumerable<TRelated>>> relationship) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().With(relationship);
        }

        public static QueryBuilder<T> With<TRelated>(Expression<Func<T, TRelated?>> relationship) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().With(relationship);
        }

        public static QueryBuilder<T> With(params string[] relationships)
        {
            return new QueryBuilder<T>().With(relationships);
        }

        public static QueryBuilder<T> With(params Expression<Func<T, object?>>[] relationships)
        {
            return new QueryBuilder<T>().With(relationships);
        }

        public static QueryBuilder<T> With<TRelated>(string relationship, Action<EagerLoadBuilder<TRelated>> constraint) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().With(relationship, constraint);
        }

        public static QueryBuilder<T> With<TRelated>(Expression<Func<T, IEnumerable<TRelated>>> relationship, Action<EagerLoadBuilder<TRelated>> constraint) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().With(relationship, constraint);
        }

        public static QueryBuilder<T> With<TRelated>(Expression<Func<T, TRelated?>> relationship, Action<EagerLoadBuilder<TRelated>> constraint) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().With(relationship, constraint);
        }

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

        public static QueryBuilder<T> Where<TValue>(Expression<Func<T, TValue>> column, TValue value)
        {
            return new QueryBuilder<T>().Where(column, value);
        }

        public static QueryBuilder<T> Where<TValue>(Expression<Func<T, TValue>> column, string comparisonOperator, TValue value)
        {
            return new QueryBuilder<T>().Where(column, comparisonOperator, value);
        }

        public static QueryBuilder<T> WhereLike(string Column, string Pattern)
        {
            return new QueryBuilder<T>()
                .WhereLike(
                    Column,
                    Pattern
                );
        }

        public static QueryBuilder<T> WhereLike(Expression<Func<T, string?>> column, string pattern)
        {
            return new QueryBuilder<T>().WhereLike(column, pattern);
        }


        public static QueryBuilder<T> WhereContains(string Column, string Value)
        {
            return new QueryBuilder<T>()
                .WhereContains(
                    Column,
                    Value
                );
        }

        public static QueryBuilder<T> WhereContains(Expression<Func<T, string?>> column, string value)
        {
            return new QueryBuilder<T>().WhereContains(column, value);
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

        public static QueryBuilder<T> WhereIn<TValue>(Expression<Func<T, TValue>> column, IEnumerable<TValue> values)
        {
            return new QueryBuilder<T>().WhereIn(column, values);
        }


        public static QueryBuilder<T> WhereNotIn<TValue>(string column, IEnumerable<TValue> values)
        {
            return new QueryBuilder<T>().WhereNotIn(column, values);
        }

        public static QueryBuilder<T> WhereNotIn<TValue>(Expression<Func<T, TValue>> column, IEnumerable<TValue> values)
        {
            return new QueryBuilder<T>().WhereNotIn(column, values);
        }

        public static QueryBuilder<T> WhereNull(string Column)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereNull(Column);

            return Builder;
        }

        public static QueryBuilder<T> WhereNull<TValue>(Expression<Func<T, TValue>> column)
        {
            return new QueryBuilder<T>().WhereNull(column);
        }

        public static QueryBuilder<T> WhereNotNull(string Column)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereNotNull(Column);

            return Builder;
        }

        public static QueryBuilder<T> WhereNotNull<TValue>(Expression<Func<T, TValue>> column)
        {
            return new QueryBuilder<T>().WhereNotNull(column);
        }

        public static QueryBuilder<T> WhereDate(string Column, string Date)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereDate(Column, Date);
            return Builder;
        }

        public static QueryBuilder<T> WhereDate<TValue>(Expression<Func<T, TValue>> column, string date)
        {
            return new QueryBuilder<T>().WhereDate(column, date);
        }

        public static QueryBuilder<T> WhereYear(string Column, string Year)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereYear(Column, Year);
            return Builder;
        }
        public static QueryBuilder<T> WhereYear<TValue>(Expression<Func<T, TValue>> column, string year)
        {
            return new QueryBuilder<T>().WhereYear(column, year);
        }

        public static QueryBuilder<T> WhereMonth(string Column, string Month)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereMonth(Column, Month);
            return Builder;
        }
        public static QueryBuilder<T> WhereMonth<TValue>(Expression<Func<T, TValue>> column, string month)
        {
            return new QueryBuilder<T>().WhereMonth(column, month);
        }

        public static QueryBuilder<T> WhereDay(string Column, string Day)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereDay(Column, Day);
            return Builder;
        }

        public static QueryBuilder<T> WhereDay<TValue>(Expression<Func<T, TValue>> column, string day)
        {
            return new QueryBuilder<T>().WhereDay(column, day);
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

        public static QueryBuilder<T> WhereDateDiff<TValue>(Expression<Func<T, TValue>> column, string date, int difference, DateDiff comparisonType, bool invert = false)
        {
            return new QueryBuilder<T>().WhereDateDiff(column, date, difference, comparisonType, invert);
        }

        public static QueryBuilder<T> WhereDateDiff<TValue>(Expression<Func<T, TValue>> column, string date, string comparisonOperator, int difference, DateDiff comparisonType, bool invert = false)
        {
            return new QueryBuilder<T>().WhereDateDiff(column, date, comparisonOperator, difference, comparisonType, invert);
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

        public static QueryBuilder<T> WhereColumn<TFirst, TSecond>(Expression<Func<T, TFirst>> firstColumn, Expression<Func<T, TSecond>> secondColumn)
        {
            return new QueryBuilder<T>().WhereColumn(firstColumn, secondColumn);
        }

        public static QueryBuilder<T> WhereColumn<TFirst, TSecond>(Expression<Func<T, TFirst>> firstColumn, string comparisonOperator, Expression<Func<T, TSecond>> secondColumn)
        {
            return new QueryBuilder<T>().WhereColumn(firstColumn, comparisonOperator, secondColumn);
        }

        public static QueryBuilder<T> WhereBetween(string Column, Between Value)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereBetween(Column, Value);
            return Builder;
        }

        public static QueryBuilder<T> WhereBetween<TValue>(Expression<Func<T, TValue>> column, Between value)
        {
            return new QueryBuilder<T>().WhereBetween(column, value);
        }

        public static QueryBuilder<T> WhereNotBetween(string Column, Between Value)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereNotBetween(Column, Value);
            return Builder;
        }

        public static QueryBuilder<T> WhereNotBetween<TValue>(Expression<Func<T, TValue>> column, Between value)
        {
            return new QueryBuilder<T>().WhereNotBetween(column, value);
        }

        public static QueryBuilder<T> WhereDateBetween(string Column, DateBetween Value)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereDateBetween(Column, Value);
            return Builder;
        }

        public static QueryBuilder<T> WhereDateBetween<TValue>(Expression<Func<T, TValue>> column, DateBetween value)
        {
            return new QueryBuilder<T>().WhereDateBetween(column, value);
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

        public static QueryBuilder<T> WhereHas<TRelationship>(Expression<Func<T, IEnumerable<TRelationship>>> relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? builder = null) where TRelationship : Model<TRelationship>, new()
        {
            return new QueryBuilder<T>("").WhereHas(relationship, builder);
        }

        public static QueryBuilder<T> WhereHas<TRelationship>(Expression<Func<T, TRelationship?>> relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? builder = null) where TRelationship : Model<TRelationship>, new()
        {
            return new QueryBuilder<T>("").WhereHas(relationship, builder);
        }

        public static QueryBuilder<T> WhereHas<TRelationship>(Expression<Func<T, IEnumerable<TRelationship>>> relationship, string comparisonOperator, int value) where TRelationship : Model<TRelationship>, new()
        {
            return new QueryBuilder<T>("").WhereHas(relationship, comparisonOperator, value);
        }

        public static QueryBuilder<T> WhereHas<TRelationship>(Expression<Func<T, TRelationship?>> relationship, string comparisonOperator, int value) where TRelationship : Model<TRelationship>, new()
        {
            return new QueryBuilder<T>("").WhereHas(relationship, comparisonOperator, value);
        }

        public static QueryBuilder<T> WhereHas<TRelationship>(Expression<Func<T, IEnumerable<TRelationship>>> relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>> builder, string comparisonOperator, int value) where TRelationship : Model<TRelationship>, new()
        {
            return new QueryBuilder<T>("").WhereHas(relationship, builder, comparisonOperator, value);
        }

        public static QueryBuilder<T> WhereHas<TRelationship>(Expression<Func<T, TRelationship?>> relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>> builder, string comparisonOperator, int value) where TRelationship : Model<TRelationship>, new()
        {
            return new QueryBuilder<T>("").WhereHas(relationship, builder, comparisonOperator, value);
        }

        public static QueryBuilder<T> WhereDoesntHave<TRelationship>(Expression<Func<T, IEnumerable<TRelationship>>> relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? builder = null) where TRelationship : Model<TRelationship>, new()
        {
            return new QueryBuilder<T>("").WhereDoesntHave(relationship, builder);
        }

        public static QueryBuilder<T> WhereDoesntHave<TRelationship>(Expression<Func<T, TRelationship?>> relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? builder = null) where TRelationship : Model<TRelationship>, new()
        {
            return new QueryBuilder<T>("").WhereDoesntHave(relationship, builder);
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

        public static QueryBuilder<T> WithCount(string relationship, string? alias = null)
        {
            return new QueryBuilder<T>().WithCount(relationship, alias);
        }

        public static QueryBuilder<T> WithCount<TRelated>(string relationship, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithCount(relationship, constraint, alias);
        }

        public static QueryBuilder<T> WithCount<TRelated>(Expression<Func<T, IEnumerable<TRelated>>> relationship, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithCount(relationship, alias);
        }

        public static QueryBuilder<T> WithCount<TRelated>(Expression<Func<T, IEnumerable<TRelated>>> relationship, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithCount(relationship, constraint, alias);
        }

        public static QueryBuilder<T> WithCount<TRelated>(Expression<Func<T, TRelated?>> relationship, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithCount(relationship, alias);
        }

        public static QueryBuilder<T> WithCount<TRelated>(Expression<Func<T, TRelated?>> relationship, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithCount(relationship, constraint, alias);
        }


        public static QueryBuilder<T> WithExists(string relationship, string? alias = null)
        {
            return new QueryBuilder<T>().WithExists(relationship, alias);
        }

        public static QueryBuilder<T> WithExists<TRelated>(string relationship, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithExists(relationship, constraint, alias);
        }

        public static QueryBuilder<T> WithExists<TRelated>(Expression<Func<T, IEnumerable<TRelated>>> relationship, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithExists(relationship, alias);
        }

        public static QueryBuilder<T> WithExists<TRelated>(Expression<Func<T, IEnumerable<TRelated>>> relationship, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithExists(relationship, constraint, alias);
        }

        public static QueryBuilder<T> WithExists<TRelated>(Expression<Func<T, TRelated?>> relationship, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithExists(relationship, alias);
        }

        public static QueryBuilder<T> WithExists<TRelated>(Expression<Func<T, TRelated?>> relationship, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithExists(relationship, constraint, alias);
        }

        public static QueryBuilder<T> WithSum(string relationship, string column, string? alias = null)
        {
            return new QueryBuilder<T>().WithSum(relationship, column, alias);
        }

        public static QueryBuilder<T> WithSum<TRelated>(string relationship, string column, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithSum(relationship, column, constraint, alias);
        }

        public static QueryBuilder<T> WithSum<TRelated>(Expression<Func<T, IEnumerable<TRelated>>> relationship, string column, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithSum(relationship, column, alias);
        }

        public static QueryBuilder<T> WithSum<TRelated>(Expression<Func<T, IEnumerable<TRelated>>> relationship, string column, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithSum(relationship, column, constraint, alias);
        }

        public static QueryBuilder<T> WithSum<TRelated>(Expression<Func<T, TRelated?>> relationship, string column, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithSum(relationship, column, alias);
        }

        public static QueryBuilder<T> WithSum<TRelated>(Expression<Func<T, TRelated?>> relationship, string column, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithSum(relationship, column, constraint, alias);
        }

        public static QueryBuilder<T> WithAvg(string relationship, string column, string? alias = null)
        {
            return new QueryBuilder<T>().WithAvg(relationship, column, alias);
        }

        public static QueryBuilder<T> WithAvg<TRelated>(string relationship, string column, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithAvg(relationship, column, constraint, alias);
        }

        public static QueryBuilder<T> WithAvg<TRelated>(Expression<Func<T, IEnumerable<TRelated>>> relationship, string column, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithAvg(relationship, column, alias);
        }

        public static QueryBuilder<T> WithAvg<TRelated>(Expression<Func<T, IEnumerable<TRelated>>> relationship, string column, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithAvg(relationship, column, constraint, alias);
        }

        public static QueryBuilder<T> WithAvg<TRelated>(Expression<Func<T, TRelated?>> relationship, string column, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithAvg(relationship, column, alias);
        }

        public static QueryBuilder<T> WithAvg<TRelated>(Expression<Func<T, TRelated?>> relationship, string column, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithAvg(relationship, column, constraint, alias);
        }

        public static QueryBuilder<T> WithMin(string relationship, string column, string? alias = null)
        {
            return new QueryBuilder<T>().WithMin(relationship, column, alias);
        }

        public static QueryBuilder<T> WithMin<TRelated>(string relationship, string column, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithMin(relationship, column, constraint, alias);
        }

        public static QueryBuilder<T> WithMin<TRelated>(Expression<Func<T, IEnumerable<TRelated>>> relationship, string column, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithMin(relationship, column, alias);
        }

        public static QueryBuilder<T> WithMin<TRelated>(Expression<Func<T, IEnumerable<TRelated>>> relationship, string column, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithMin(relationship, column, constraint, alias);
        }

        public static QueryBuilder<T> WithMin<TRelated>(Expression<Func<T, TRelated?>> relationship, string column, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithMin(relationship, column, alias);
        }

        public static QueryBuilder<T> WithMin<TRelated>(Expression<Func<T, TRelated?>> relationship, string column, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithMin(relationship, column, constraint, alias);
        }

        public static QueryBuilder<T> WithMax(string relationship, string column, string? alias = null)
        {
            return new QueryBuilder<T>().WithMax(relationship, column, alias);
        }

        public static QueryBuilder<T> WithMax<TRelated>(string relationship, string column, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithMax(relationship, column, constraint, alias);
        }

        public static QueryBuilder<T> WithMax<TRelated>(Expression<Func<T, IEnumerable<TRelated>>> relationship, string column, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithMax(relationship, column, alias);
        }

        public static QueryBuilder<T> WithMax<TRelated>(Expression<Func<T, IEnumerable<TRelated>>> relationship, string column, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithMax(relationship, column, constraint, alias);
        }

        public static QueryBuilder<T> WithMax<TRelated>(Expression<Func<T, TRelated?>> relationship, string column, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithMax(relationship, column, alias);
        }

        public static QueryBuilder<T> WithMax<TRelated>(Expression<Func<T, TRelated?>> relationship, string column, Func<SubQuery<TRelated>, SubQuery<TRelated>> constraint, string? alias = null) where TRelated : Model<TRelated>, new()
        {
            return new QueryBuilder<T>().WithMax(relationship, column, constraint, alias);
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



        public RelationshipQuery<TRelated> Relation<TRelated>(Expression<Func<T, IEnumerable<TRelated>>> relationship) where TRelated : Model<TRelated>, new()
        {
            PropertyInfo property = RelationshipExpression.GetProperty(relationship, typeof(T));
            RelationshipDefinition definition = RelationshipMetadataCache.GetRequired(typeof(T), property.Name);

            if (definition.Kind != RelationshipKind.HasMany)
            {
                throw new RelationshipException($"Relationship '{definition.Name}' on model '{typeof(T).Name}' is configured as '{definition.Kind}' and cannot be accessed as a collection relationship.");
            }

            return new RelationshipQuery<TRelated>(this, definition);
        }

        public RelationshipQuery<TRelated> Relation<TRelated>(Expression<Func<T, TRelated?>> relationship) where TRelated : Model<TRelated>, new()
        {
            PropertyInfo property = RelationshipExpression.GetProperty(relationship, typeof(T));
            RelationshipDefinition definition = RelationshipMetadataCache.GetRequired(typeof(T), property.Name);

            if (definition.Kind == RelationshipKind.HasMany)
            {
                throw new RelationshipException($"Relationship '{definition.Name}' on model '{typeof(T).Name}' is configured as 'HasMany' and must be accessed as a collection relationship.");
            }

            return new RelationshipQuery<TRelated>(this, definition);
        }


        public bool IsRelationLoaded(string relationship)
        {
            RelationshipDefinition definition = RelationshipMetadataCache.GetRequired(typeof(T), relationship);
            return _loadedRelationships.Contains(definition.Name);
        }

        public bool IsRelationLoaded(Expression<Func<T, object?>> relationship)
        {
            PropertyInfo property = RelationshipExpression.GetProperty(relationship, typeof(T));
            return IsRelationLoaded(property.Name);
        }

        public T Load(params string[] relationships)
        {
            EagerLoadPlan plan = BuildEagerLoadPlan(relationships, false, nameof(Load));

            if (plan.HasLoads)
            {
                EagerLoader.Load(new[] { (T)this }, plan);
            }

            return (T)this;
        }

        public T Load(params Expression<Func<T, object?>>[] relationships)
        {
            return Load(GetRelationshipNames(relationships, nameof(Load)));
        }

        public Task<T> LoadAsync(params string[] relationships)
        {
            return LoadAsync(CancellationToken.None, relationships);
        }

        public async Task<T> LoadAsync(CancellationToken cancellationToken, params string[] relationships)
        {
            EagerLoadPlan plan = BuildEagerLoadPlan(relationships, false, nameof(LoadAsync));

            if (plan.HasLoads)
            {
                await EagerLoader.LoadAsync(new[] { (T)this }, plan, cancellationToken: cancellationToken).ConfigureAwait(false);
            }

            return (T)this;
        }

        public Task<T> LoadAsync(params Expression<Func<T, object?>>[] relationships)
        {
            return LoadAsync(CancellationToken.None, relationships);
        }

        public Task<T> LoadAsync(CancellationToken cancellationToken, params Expression<Func<T, object?>>[] relationships)
        {
            return LoadAsync(cancellationToken, GetRelationshipNames(relationships, nameof(LoadAsync)));
        }

        public T LoadMissing(params string[] relationships)
        {
            EagerLoadPlan plan = BuildEagerLoadPlan(relationships, true, nameof(LoadMissing));

            if (plan.HasLoads)
            {
                EagerLoader.Load(new[] { (T)this }, plan);
            }

            return (T)this;
        }

        public T LoadMissing(params Expression<Func<T, object?>>[] relationships)
        {
            return LoadMissing(GetRelationshipNames(relationships, nameof(LoadMissing)));
        }

        public Task<T> LoadMissingAsync(params string[] relationships)
        {
            return LoadMissingAsync(CancellationToken.None, relationships);
        }

        public async Task<T> LoadMissingAsync(CancellationToken cancellationToken, params string[] relationships)
        {
            EagerLoadPlan plan = BuildEagerLoadPlan(relationships, true, nameof(LoadMissingAsync));

            if (plan.HasLoads)
            {
                await EagerLoader.LoadAsync(new[] { (T)this }, plan, cancellationToken: cancellationToken).ConfigureAwait(false);
            }

            return (T)this;
        }

        public Task<T> LoadMissingAsync(params Expression<Func<T, object?>>[] relationships)
        {
            return LoadMissingAsync(CancellationToken.None, relationships);
        }

        public Task<T> LoadMissingAsync(CancellationToken cancellationToken, params Expression<Func<T, object?>>[] relationships)
        {
            return LoadMissingAsync(cancellationToken, GetRelationshipNames(relationships, nameof(LoadMissingAsync)));
        }

        bool IRelationshipLoadState.IsRelationLoadedInternal(string relationshipName)
        {
            return _loadedRelationships.Contains(relationshipName);
        }

        void IRelationshipLoadState.MarkRelationLoadedInternal(string relationshipName)
        {
            if (!string.IsNullOrWhiteSpace(relationshipName))
            {
                _loadedRelationships.Add(relationshipName);
            }
        }

        private EagerLoadPlan BuildEagerLoadPlan(IEnumerable<string> relationships, bool onlyMissing, string methodName)
        {
            if (relationships == null)
            {
                throw new ArgumentNullException(nameof(relationships));
            }

            string[] names = relationships.ToArray();

            if (names.Length == 0)
            {
                throw new ArgumentException($"{methodName} requires at least one relationship.", nameof(relationships));
            }

            var plan = new EagerLoadPlan();

            foreach (string relationship in names)
            {
                if (string.IsNullOrWhiteSpace(relationship))
                {
                    throw new RelationshipException($"{methodName} contains an invalid relationship name on model '{typeof(T).Name}'.");
                }

                RelationshipDefinition definition = RelationshipMetadataCache.GetRequired(typeof(T), relationship.Trim());

                if (!onlyMissing || !_loadedRelationships.Contains(definition.Name))
                {
                    plan.Add(typeof(T), definition.Name);
                }
            }

            return plan;
        }

        private static string[] GetRelationshipNames(IEnumerable<Expression<Func<T, object?>>> relationships, string methodName)
        {
            if (relationships == null)
            {
                throw new ArgumentNullException(nameof(relationships));
            }

            Expression<Func<T, object?>>[] expressions = relationships.ToArray();

            if (expressions.Length == 0)
            {
                throw new ArgumentException($"{methodName} requires at least one relationship.", nameof(relationships));
            }

            return expressions.Select(expression => RelationshipExpression.GetProperty(expression, typeof(T)).Name).ToArray();
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


        private static (QueryBuilder<T> Builder, T Item, DynamicParameters Parameters) PrepareDynamicCreate(object args)
        {
            if (args == null)
            {
                throw new ArgumentNullException(nameof(args));
            }

            PropertyInfo[] properties = args.GetType().GetProperties();

            if (properties.Length == 0)
            {
                throw new QueryBuilderException("Create requires at least one property to insert.");
            }

            PropertyInfo primaryKey = GetRequiredPrimaryKeyProperty();
            bool incrementing = QueryBuilder<T>.IsIncrementing();

            Dictionary<string, PropertyInfo> modelProperties = typeof(T).GetProperties().ToDictionary(property => property.Name, property => property, StringComparer.OrdinalIgnoreCase);

            var parameters = new DynamicParameters();
            var names = new List<string>();
            T item = new();

            foreach (PropertyInfo sourceProperty in properties)
            {
                if (!modelProperties.TryGetValue(sourceProperty.Name, out PropertyInfo? modelProperty))
                {
                    throw new QueryBuilderException($"Property '{sourceProperty.Name}' does not exist on model '{typeof(T).Name}'.");
                }

                bool isPrimaryKey = string.Equals(modelProperty.Name, primaryKey.Name, StringComparison.OrdinalIgnoreCase);
                bool isFillable = Attribute.IsDefined(modelProperty, typeof(Fillable));

                if (isPrimaryKey && incrementing)
                {
                    throw new QueryBuilderException($"Primary key '{primaryKey.Name}' cannot be provided because model '{typeof(T).Name}' uses an incrementing primary key.");
                }

                if (!isPrimaryKey && !isFillable)
                {
                    throw new QueryBuilderException($"Property '{modelProperty.Name}' is not marked with [Fillable] on model '{typeof(T).Name}'.");
                }

                object? value = sourceProperty.GetValue(args, null);

                parameters.Add(modelProperty.Name, value);
                names.Add(modelProperty.Name);

                AssignDynamicPropertyValue(item, modelProperty, value);
            }

            if (!incrementing && !names.Any(name => string.Equals(name, primaryKey.Name, StringComparison.OrdinalIgnoreCase)))
            {
                throw new QueryBuilderException($"Primary key '{primaryKey.Name}' must be provided because model '{typeof(T).Name}' does not use an incrementing primary key.");
            }

            var builder = new QueryBuilder<T>().InsertDynamicQuery(names);

            return (builder, item, parameters);
        }

        private (QueryBuilder<T> Builder, DynamicParameters Parameters, PropertyInfo[] Properties) PrepareDynamicUpdate(object args)
        {
            if (args == null)
            {
                throw new ArgumentNullException(nameof(args));
            }


            PropertyInfo? primaryAttribute = QueryBuilder<T>.GetPropertyInfoByAttribute(typeof(PrimaryKey));

            if (primaryAttribute == null)
            {
                throw new ArgumentException("Column Primary Key not found");
            }

            PropertyInfo[] properties = args.GetType().GetProperties();

            if (properties.Length == 0)
            {
                throw new ArgumentException("Update requires at least one property to update.", nameof(args));
            }


            /*
             * ============================================================
             * VALIDAR QUE TODAS LAS COLUMNAS PROVENGAN DEL MODELO
             * ============================================================
             */

            Dictionary<string, PropertyInfo> modelProperties = typeof(T).GetProperties().ToDictionary(property => property.Name, property => property, StringComparer.OrdinalIgnoreCase);

            foreach (PropertyInfo property in properties)
            {
                if (!modelProperties.TryGetValue(property.Name, out PropertyInfo? modelProperty))
                {
                    throw new QueryBuilderException(
                        $"Property '{property.Name}' does not exist " +
                        $"on model '{typeof(T).Name}'."
                    );
                }


                if (string.Equals(modelProperty.Name, primaryAttribute.Name, StringComparison.OrdinalIgnoreCase))
                {
                    throw new ArgumentException(
                        $"Primary key '{primaryAttribute.Name}' " +
                        $"cannot be included in a partial update.",
                        nameof(args)
                    );
                }


                if (!Attribute.IsDefined(modelProperty, typeof(Fillable)))
                {
                    throw new QueryBuilderException(
                        $"Property '{modelProperty.Name}' is not marked " +
                        $"with [Fillable] on model '{typeof(T).Name}'."
                    );
                }
            }


            object? primaryKeyValue = primaryAttribute.GetValue(this, null);

            var parameters = new DynamicParameters();

            foreach (PropertyInfo property in properties)
            {
                parameters.Add(property.Name, property.GetValue(args, null));
            }

            parameters.Add(primaryAttribute.Name, primaryKeyValue);

            var builder = new QueryBuilder<T>();

            builder.UpdateDynamicQuery<T>(args);

            return (builder, parameters, properties);
        }

        private static List<T> PrepareManyItems(IEnumerable<T> items, string methodName)
        {
            if (items == null)
            {
                throw new ArgumentNullException(nameof(items));
            }

            List<T> itemsList = items.ToList();

            if (itemsList.Any(item => item == null))
            {
                throw new ArgumentException($"{methodName}<{typeof(T).Name}> cannot contain null items.", nameof(items));
            }

            return itemsList;
        }

        private static IReadOnlyList<T> PrepareBulkItems(IEnumerable<T> items, string methodName)
        {
            if (items == null)
            {
                throw new ArgumentNullException(nameof(items));
            }

            IReadOnlyList<T> itemsList = items as IReadOnlyList<T> ?? items.ToList();

            if (itemsList.Any(item => item == null))
            {
                throw new ArgumentException($"{methodName}<{typeof(T).Name}> cannot contain null items.", nameof(items));
            }

            return itemsList;
        }

        private static void ExecuteCreateMany(List<T> items)
        {
            int chunkSize = QueryBuilder<T>.GetInsertManyChunkSize();
            string connectionKey = QueryBuilder<T>.GetConnectionString();

            _executor.ExecuteBatch(connectionKey, () =>
            {
                for (int offset = 0; offset < items.Count; offset += chunkSize)
                {
                    int count = Math.Min(chunkSize, items.Count - offset);
                    List<T> chunk = items.GetRange(offset, count);

                    BulkInsertCommand command = QueryBuilder<T>.BuildInsertManyCommand(chunk, true);
                    var builder = new QueryBuilder<T>();

                    if (command.ReturnsGeneratedKeys)
                    {
                        List<BulkInsertKeyResult> generatedKeys = builder.QueryList<BulkInsertKeyResult>(command.Sql, command.Parameters);
                        AssignBulkGeneratedPrimaryKeys(chunk, generatedKeys);
                    }
                    else
                    {
                        builder.ExecuteCommand(command.Sql, command.Parameters);
                    }
                }

                return true;
            });
        }

        private static async Task ExecuteCreateManyAsync(List<T> items, CancellationToken cancellationToken)
        {
            int chunkSize = QueryBuilder<T>.GetInsertManyChunkSize();
            string connectionKey = QueryBuilder<T>.GetConnectionString();

            await _executor.ExecuteBatchAsync(connectionKey, async () =>
            {
                for (int offset = 0; offset < items.Count; offset += chunkSize)
                {
                    cancellationToken.ThrowIfCancellationRequested();

                    int count = Math.Min(chunkSize, items.Count - offset);
                    List<T> chunk = items.GetRange(offset, count);

                    BulkInsertCommand command = QueryBuilder<T>.BuildInsertManyCommand(chunk, true);
                    var builder = new QueryBuilder<T>();

                    if (command.ReturnsGeneratedKeys)
                    {
                        List<BulkInsertKeyResult> generatedKeys = await builder.QueryListAsync<BulkInsertKeyResult>(command.Sql, command.Parameters, cancellationToken).ConfigureAwait(false);
                        AssignBulkGeneratedPrimaryKeys(chunk, generatedKeys);
                    }
                    else
                    {
                        await builder.ExecuteCommandAsync(command.Sql, command.Parameters, cancellationToken).ConfigureAwait(false);
                    }
                }

                return true;
            }, cancellationToken).ConfigureAwait(false);
        }

        private static void ExecuteInsertMany(List<T> items)
        {
            int chunkSize = QueryBuilder<T>.GetInsertManyChunkSize();
            string connectionKey = QueryBuilder<T>.GetConnectionString();

            _executor.ExecuteBatch(connectionKey, () =>
            {
                var builder = new QueryBuilder<T>();

                for (int offset = 0; offset < items.Count; offset += chunkSize)
                {
                    int count = Math.Min(chunkSize, items.Count - offset);
                    List<T> chunk = items.GetRange(offset, count);

                    BulkInsertCommand command = QueryBuilder<T>.BuildInsertManyCommand(chunk, false);
                    builder.ExecuteCommand(command.Sql, command.Parameters);
                }

                return true;
            });
        }

        private static async Task ExecuteInsertManyAsync(List<T> items, CancellationToken cancellationToken)
        {
            int chunkSize = QueryBuilder<T>.GetInsertManyChunkSize();
            string connectionKey = QueryBuilder<T>.GetConnectionString();

            await _executor.ExecuteBatchAsync(connectionKey, async () =>
            {
                var builder = new QueryBuilder<T>();

                for (int offset = 0; offset < items.Count; offset += chunkSize)
                {
                    cancellationToken.ThrowIfCancellationRequested();

                    int count = Math.Min(chunkSize, items.Count - offset);
                    List<T> chunk = items.GetRange(offset, count);

                    BulkInsertCommand command = QueryBuilder<T>.BuildInsertManyCommand(chunk, false);
                    await builder.ExecuteCommandAsync(command.Sql, command.Parameters, cancellationToken).ConfigureAwait(false);
                }

                return true;
            }, cancellationToken).ConfigureAwait(false);
        }

        private static void ExecuteBulkInsert(IReadOnlyList<T> items)
        {
            string connectionKey = QueryBuilder<T>.GetConnectionString();
            string table = QueryBuilder<T>.GetTableName();
            List<PropertyInfo> properties = GetBulkInsertProperties();

            _executor.Execute(connectionKey, context =>
            {
                using var reader = new BulkInsertDataReader<T>(items, properties);
                using var bulkCopy = new SqlBulkCopy(context.Connection, SqlBulkCopyOptions.KeepNulls | SqlBulkCopyOptions.CheckConstraints, context.Transaction);

                bulkCopy.DestinationTableName = table;
                bulkCopy.EnableStreaming = true;
                bulkCopy.BatchSize = 5000;

                foreach (PropertyInfo property in properties)
                {
                    bulkCopy.ColumnMappings.Add(property.Name, property.Name);
                }

                int? commandTimeout = new QueryBuilder<T>().GetCommandTimeout();

                if (commandTimeout.HasValue)
                {
                    bulkCopy.BulkCopyTimeout = commandTimeout.Value;
                }

                bulkCopy.WriteToServer(reader);

                return true;
            });
        }

        private static async Task ExecuteBulkInsertAsync(IReadOnlyList<T> items, CancellationToken cancellationToken)
        {
            string connectionKey = QueryBuilder<T>.GetConnectionString();
            string table = QueryBuilder<T>.GetTableName();
            List<PropertyInfo> properties = GetBulkInsertProperties();

            await _executor.ExecuteAsync(connectionKey, async context =>
            {
                using var reader = new BulkInsertDataReader<T>(items, properties);
                using var bulkCopy = new SqlBulkCopy(context.Connection, SqlBulkCopyOptions.KeepNulls | SqlBulkCopyOptions.CheckConstraints, context.Transaction);

                bulkCopy.DestinationTableName = table;
                bulkCopy.EnableStreaming = true;
                bulkCopy.BatchSize = 5000;

                foreach (PropertyInfo property in properties)
                {
                    bulkCopy.ColumnMappings.Add(property.Name, property.Name);
                }

                int? commandTimeout = new QueryBuilder<T>().GetCommandTimeout();

                if (commandTimeout.HasValue)
                {
                    bulkCopy.BulkCopyTimeout = commandTimeout.Value;
                }

                await bulkCopy.WriteToServerAsync(reader, cancellationToken).ConfigureAwait(false);

                return true;
            }, cancellationToken).ConfigureAwait(false);
        }

        private static List<PropertyInfo> GetBulkInsertProperties()
        {
            PropertyInfo? primaryKey = QueryBuilder<T>.GetPropertyInfoByAttribute(typeof(PrimaryKey));

            if (primaryKey == null)
            {
                throw new ModelConfigurationException($"Primary key is not defined for model '{typeof(T).Name}'. Add the [PrimaryKey] attribute to the appropriate property.");
            }

            bool incrementing = QueryBuilder<T>.IsIncrementing();

            List<PropertyInfo> properties = QueryBuilder<T>.GetInsertManyProperties(primaryKey, incrementing);

            if (properties.Count == 0)
            {
                throw new ModelConfigurationException($"Model '{typeof(T).Name}' does not contain properties that can be bulk inserted.");
            }

            return properties;
        }

        private static void AssignBulkGeneratedPrimaryKeys(IReadOnlyList<T> items, IReadOnlyList<BulkInsertKeyResult> generatedKeys)
        {
            if (generatedKeys.Count != items.Count)
            {
                throw new ModelConfigurationException($"Bulk insert for model '{typeof(T).Name}' inserted {items.Count} records but returned {generatedKeys.Count} generated primary keys.");
            }

            var assignedIndexes = new bool[items.Count];

            foreach (BulkInsertKeyResult generatedKey in generatedKeys)
            {
                if (generatedKey.Index < 0 || generatedKey.Index >= items.Count)
                {
                    throw new ModelConfigurationException($"Bulk insert for model '{typeof(T).Name}' returned an invalid row index '{generatedKey.Index}'.");
                }

                if (assignedIndexes[generatedKey.Index])
                {
                    throw new ModelConfigurationException($"Bulk insert for model '{typeof(T).Name}' returned the row index '{generatedKey.Index}' more than once.");
                }

                AssignGeneratedPrimaryKey(items[generatedKey.Index], generatedKey.Value);
                assignedIndexes[generatedKey.Index] = true;
            }

            if (assignedIndexes.Any(assigned => !assigned))
            {
                throw new ModelConfigurationException($"Bulk insert for model '{typeof(T).Name}' did not return a generated primary key for every inserted record.");
            }
        }

        private static void AssignDynamicPropertyValue(T item, PropertyInfo property, object? value)
        {
            if (!property.CanWrite)
            {
                throw new ModelConfigurationException($"Property '{property.Name}' on model '{typeof(T).Name}' is read-only.");
            }

            if (value == null)
            {
                if (property.PropertyType.IsValueType && Nullable.GetUnderlyingType(property.PropertyType) == null)
                {
                    throw new ModelConfigurationException($"Property '{property.Name}' on model '{typeof(T).Name}' cannot receive null because its type '{property.PropertyType.Name}' is not nullable.");
                }

                property.SetValue(item, null);

                return;
            }

            Type destinationType = Nullable.GetUnderlyingType(property.PropertyType) ?? property.PropertyType;

            object convertedValue;

            try
            {
                if (destinationType.IsInstanceOfType(value))
                {
                    convertedValue = value;
                }
                else if (destinationType == typeof(Guid))
                {
                    convertedValue = value is Guid guid ? guid : Guid.Parse(value.ToString()!);
                }
                else if (destinationType.IsEnum)
                {
                    convertedValue = value is string enumName ? Enum.Parse(destinationType, enumName, true) : Enum.ToObject(destinationType, value);
                }
                else
                {
                    convertedValue = Convert.ChangeType(value, destinationType, System.Globalization.CultureInfo.InvariantCulture);
                }
            }
            catch (Exception ex)
            {
                throw new ModelConfigurationException($"Unable to convert value '{value}' from type '{value.GetType().Name}' to '{property.PropertyType.Name}' for property '{property.Name}' on model '{typeof(T).Name}'.", ex);
            }

            property.SetValue(item, convertedValue);
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
