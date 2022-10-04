using Dapper;
using DapperGlib.Util;
using Newtonsoft.Json;
using System.Reflection;

namespace DapperGlib
{

    public abstract class Model<T> where T : Model<T>, new()
    {

        internal static readonly GlipContext _context = new();

        public virtual bool Incrementing { get; } = true;
        public virtual string? Table { get; }
        public virtual string? Schema { get; }

        protected static readonly object Instance = Activator.CreateInstance(typeof(T)) ?? (new());

        public Model()
        {

        }

        #region CRUD
        public void Insert()
        {

            var Builder = new QueryBuilder<T>().InsertQuery(this);

            using var conection = _context.CreateConnection();
            conection.Execute(Builder.ToSql(), this);

            string? query = QueryBuilder<T>.LastIdQuery();

            if (query != null)
            {
                int LastId = conection.Query<int>(query).First();

                PropertyInfo? primaryAttribute = QueryBuilder<T>.GetPropertyInfoByAttribute(typeof(PrimaryKey));

                if (primaryAttribute != null)
                {
                    primaryAttribute.SetValue(this, LastId);
                }
            }
          
        }

        public static T Create(T Item)
        {

            var Builder = new QueryBuilder<T>().InsertQuery(Item);

            using var conection = _context.CreateConnection();
            conection.Execute(Builder.ToSql(), Item);

            string? query = QueryBuilder<T>.LastIdQuery();

            if (query != null)
            {
                int LastId = conection.Query<int>(query).First();

                PropertyInfo? primaryAttribute = QueryBuilder<T>.GetPropertyInfoByAttribute(typeof(PrimaryKey));

                if (primaryAttribute != null)
                {
                    primaryAttribute.SetValue(Item, LastId);
                }
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

        public void Update()
        {
            var Builder = new QueryBuilder<T>().UpdateQuery(this);

            using var conection = _context.CreateConnection();
            conection.Execute(Builder.ToSql(), this);
        }

        public void Update(dynamic args)
        {

            PropertyInfo? primaryAttribute = QueryBuilder<T>.GetPropertyInfoByAttribute(typeof(PrimaryKey));

            if (primaryAttribute != null)
            {
                var val = primaryAttribute.GetValue(this, null);

                var json = JsonConvert.SerializeObject(args);
                var item = (T)JsonConvert.DeserializeObject<T>(json);
                primaryAttribute.SetValue(item, val);

                var Builder = new QueryBuilder<T>();
                Builder.UpdateDynamicQuery<T>(args);

                using var conection = _context.CreateConnection();
                conection.Execute(Builder.ToSql(), item);

                var Properties = args.GetType().GetProperties();

                foreach (var property in Properties)
                {
                    var PropertyName = property.Name;

                    var OwnProperty = this.GetType().GetProperties().Where(x => x.Name == PropertyName).FirstOrDefault();

                    if (OwnProperty != null)
                    {
                        OwnProperty.SetValue(this, property.GetValue(args, null));
                    }

                }

            }
            else
            {
                throw new ArgumentException("Column Primary Key not found");
            }


        }

        public void Delete()
        {
            new QueryBuilder<T>().SimpleDelete(this);
        }

        public Task<int> DeleteAsync()
        {
            return new QueryBuilder<T>().SimpleDeleteAsync(this);
        }

        public static void Truncate()
        {
            new QueryBuilder<T>().Truncate();
        }

        public static Task<int> TruncateAsync()
        {
            return new QueryBuilder<T>().TruncateAsync();
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
            var Builder = new QueryBuilder<T>();
            return Builder.FirstAsync();
        }

        public static Task<T?> FirstOrDefaultAsync()
        {
            var Builder = new QueryBuilder<T>();
            return Builder.FirstOrDefaultAsync();
        }

        public static T Find(int Id)
        {
            string? primaryKey = QueryBuilder<T>.GetPrimaryKey();

            if (primaryKey == null)
            {
                throw new ApplicationException("Primary Key Column is not defined");
            }

            var Builder = new QueryBuilder<T>().Where(primaryKey, Id);

            using var conection = _context.CreateConnection();
            var item = conection.QueryFirst<T>(Builder.ToSql(), new { Id });

            return item;
        }

        public static Task<T> FindAsync(int Id)
        {
            string? primaryKey = QueryBuilder<T>.GetPrimaryKey();

            if (primaryKey == null)
            {
                throw new ApplicationException("Primary Key Column is not defined");
            }

            var Builder = new QueryBuilder<T>().Where(primaryKey, Id);

            using var conection = _context.CreateConnection();
            var item = conection.QueryFirstAsync<T>(Builder.ToSql(), new { Id });

            return Task.FromResult(item.Result);
        }

        public static T? FindOrDefault(int Id)
        {
            string? primaryKey = QueryBuilder<T>.GetPrimaryKey();

            if (primaryKey == null)
            {
                throw new ApplicationException("Primary Key Column is not defined");
            }

            var Builder = new QueryBuilder<T>().Where(primaryKey, Id);

            using var conection = _context.CreateConnection();
            var item = conection.QueryFirstOrDefault<T>(Builder.ToSql(), new { Id });

            return item;
        }

        public static Task<T?> FindOrDefaultAsync(int Id)
        {
            string? primaryKey = QueryBuilder<T>.GetPrimaryKey();

            if (primaryKey == null)
            {
                throw new ApplicationException("Primary Key Column is not defined");
            }

            var Builder = new QueryBuilder<T>().Where(primaryKey, Id);

            using var conection = _context.CreateConnection();
            var item = conection.QueryFirstOrDefaultAsync<T?>(Builder.ToSql(), new { Id });

            return Task.FromResult(item.Result);
        }

        public static T Find(string Id)
        {
            string? primaryKey = QueryBuilder<T>.GetPrimaryKey();

            if (primaryKey == null)
            {
                throw new ApplicationException("Primary Key Column is not defined");
            }

            var Builder = new QueryBuilder<T>().Where(primaryKey, Id);

            using var conection = _context.CreateConnection();
            var item = conection.QueryFirst<T>(Builder.ToSql(), new { Id });

            return item;
        }

        public static Task<T> FindAsync(string Id)
        {
            string? primaryKey = QueryBuilder<T>.GetPrimaryKey();

            if (primaryKey == null)
            {
                throw new ApplicationException("Primary Key Column is not defined");
            }

            var Builder = new QueryBuilder<T>().Where(primaryKey, Id);

            using var conection = _context.CreateConnection();
            var item = conection.QueryFirstAsync<T>(Builder.ToSql(), new { Id });

            return Task.FromResult(item.Result);
        }

        public static T? FindOrDefault(string Id)
        {
            string? primaryKey = QueryBuilder<T>.GetPrimaryKey();

            if (primaryKey == null)
            {
                throw new ApplicationException("Primary Key Column is not defined");
            }

            var Builder = new QueryBuilder<T>().Where(primaryKey, Id);

            using var conection = _context.CreateConnection();
            var item = conection.QueryFirstOrDefault<T>(Builder.ToSql(), new { Id });

            return item;
        }

        public static Task<T?> FindOrDefaultAsync(string Id)
        {
            string? primaryKey = QueryBuilder<T>.GetPrimaryKey();

            if (primaryKey == null)
            {
                throw new ApplicationException("Primary Key Column is not defined");
            }

            var Builder = new QueryBuilder<T>().Where(primaryKey, Id);

            using var conection = _context.CreateConnection();
            var item = conection.QueryFirstOrDefaultAsync<T?>(Builder.ToSql(), new { Id });

            return Task.FromResult(item.Result);
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

        public static int Count()
        {
            var Builder = new QueryBuilder<T>().SimpleQuery();
            return Builder.Count();
        }

        public static string Value(string Column)
        {
            var Builder = new QueryBuilder<T>();
            return Builder.Value(Column);
        }

        public static double Max(string Column)
        {
            var Builder = new QueryBuilder<T>();
            return Builder.Max(Column);
        }

        public static double Min(string Column)
        {
            var Builder = new QueryBuilder<T>();
            return Builder.Min(Column);
        }

        public static double Avg(string Column)
        {
            var Builder = new QueryBuilder<T>();
            return Builder.Avg(Column);
        }

        public static double Sum(string Column)
        {
            var Builder = new QueryBuilder<T>();
            return Builder.Sum(Column);
        }

        public static string GetTableName()
        {
            return QueryBuilder<T>.GetTableName();
        }

        #endregion

        public static QueryBuilder<T> Query()
        {
            var Builder = new QueryBuilder<T>().SimpleQuery();

            return Builder;
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

        public static QueryBuilder<T> WhereNot(Func<SubQuery<T>, SubQuery<T>> Builder)
        {
            QueryBuilder<T> Builder_ = new QueryBuilder<T>();

            Builder_.WhereNot(Builder);

            return Builder_;
        }

        public static QueryBuilder<T> WhereIn(string Column, object[] Values)
        {

            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereIn(Column, Values);
            return Builder;

        }

        public static QueryBuilder<T> WhereNotIn(string Column, object[] Values)
        {

            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereNotIn(Column, Values);
            return Builder;

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
      
        public static QueryBuilder<T> WhereDate(string Column, string Value)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereDate(Column, Value);
            return Builder;
        }

        public static QueryBuilder<T> WhereYear(string Column, string Value)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereYear(Column, Value);
            return Builder;
        }

        public static QueryBuilder<T> WhereMonth(string Column, string Value)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereMonth(Column, Value);
            return Builder;
        }

        public static QueryBuilder<T> WhereDay(string Column, string Value)
        {
            QueryBuilder<T> Builder = new QueryBuilder<T>().WhereDay(Column, Value);
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

        public Relationship<TRelationship> HasRelationship<TRelationship>(Relationship<TRelationship> Relationship)
        {

            PropertyInfo? Local = Instance.GetType().GetProperty(Relationship.LocalKey);

            if (Local != null)
            {
                var localValue = Local.GetValue(this, null);

                Relationship.Where(Relationship.ForeignKey, "=", localValue);

                return Relationship;
            }

            throw new ArgumentException($"Column {Relationship.LocalKey} not found");

        }

        //public Relationship<TRelationship> HasOne<TRelationship>(Relationship<TRelationship> Relationship)
        //{

        //    PropertyInfo? Local = Instance.GetType().GetProperty(Relationship.LocalKey);

        //    if (Local != null)
        //    {
        //        var localValue = Local.GetValue(this, null);

        //        Relationship.Where(Relationship.ForeignHey, "=", localValue);

        //        return Relationship;
        //    }

        //    throw new ArgumentException($"Column {Relationship.LocalKey} not found");

        //}

        //public Relationship<TRelationship> HasMany<TRelationship>(Relationship<TRelationship> Relationship)
        //{

        //    PropertyInfo? Local = Instance.GetType().GetProperty(Relationship.LocalKey);

        //    if (Local != null)
        //    {
        //        var localValue = Local.GetValue(this, null);

        //        Relationship.Where(Relationship.ForeignHey, "=", localValue);

        //        return Relationship;
        //    }

        //    throw new ArgumentException($"Column {Relationship.LocalKey} not found");

        //}

    }

}
