using Dapper;
using DapperGlib.Util;
using Newtonsoft.Json;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;

namespace DapperGlib
{
    public class QueryBuilder<TModel>: Builder<TModel>
    {
        public QueryBuilder()
        {

        }

        public QueryBuilder(string query)
        {
            Query = new StringBuilder(query);
        }

        public TModel First()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            string query = ToSql();

            using var conection = _context.CreateConnection();

            var item = conection.QueryFirst<TModel>(query);
            return item;
        }

        public TModel? FirstOrDefault()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            string query = ToSql();

            using var conection = _context.CreateConnection();
            var item = conection.QueryFirstOrDefault<TModel>(query);
            return item;
        }

        public Task<TModel> FirstAsync()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            string query = ToSql();

            using var conection = _context.CreateConnection();

            var item = conection.QueryFirstAsync<TModel>(query)!;

            return Task.FromResult(item.Result);
        }

        public Task<TModel?> FirstOrDefaultAsync()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            string query = ToSql();

            using var conection = _context.CreateConnection();

            var item = conection.QueryFirstOrDefaultAsync<TModel?>(query)!;

            return Task.FromResult(item.Result);
        }

        public List<TModel> ToList()
        {

            string query = ToSql();

            using var conection = _context.CreateConnection();
            var lista = conection.Query<TModel>(query);
            return lista.ToList();
        }

        public Task<List<TModel>> ToListAsync()
        {
            string query = ToSql();

            using var conection = _context.CreateConnection();

            var lista = conection.QueryAsync<TModel>(query);

            return Task.FromResult(lista.Result.ToList());
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

            using var conection = _context.CreateConnection();
            conection.Execute(ToSql(), item);
        }

        public Task<int> UpdateAsync(dynamic args)
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

            using var conection = _context.CreateConnection();
            var result = conection.ExecuteAsync(ToSql(), item);
            return Task.FromResult(result.Result);
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

            using var conection = _context.CreateConnection();
            conection.Execute(ToSql());

        }

        public Task<int> DeleteAsync()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            var regex = new Regex(Regex.Escape("FROM"));
            var match = regex.Match(Query.ToString());

            string replaced = string.Concat(" DELETE ", Query.ToString().AsSpan(match.Index));

            Query = new(replaced);

            using var conection = _context.CreateConnection();
            var result = conection.ExecuteAsync(ToSql());

            return Task.FromResult(result.Result);
        }

        public int Count()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            var regex = new Regex(Regex.Escape("_selector_all"));
            var newQuery = regex.Replace(Query.ToString(), "_selector_count", 1);

            Query = new(newQuery);

            string query = ToSql();

            using var conection = _context.CreateConnection();

            var result = conection.Query<int>(query);

            return result.FirstOrDefault();
        }

        internal void SimpleDelete<T>(T Item)
        {
            string table = GetTableName();
            string primaryKey = GetPrimaryKey();

            Query = new StringBuilder($"{Clauses.DELETE} FROM {table} WHERE {primaryKey} = @{primaryKey}");

            using var conection = _context.CreateConnection();
            conection.Execute(ToSql(), Item);

        }

        internal Task<int> SimpleDeleteAsync<T>(T Item)
        {
            string table = GetTableName();
            string primaryKey = GetPrimaryKey();

            Query = new StringBuilder($"{Clauses.DELETE} FROM {table} WHERE {primaryKey} = @{primaryKey}");

            using var conection = _context.CreateConnection();
            var result = conection.ExecuteAsync(ToSql(), Item);

            return Task.FromResult(result.Result);
        }

        public QueryBuilder<TModel> Take(int Limit, int Offset = 0)
        {
           
            if (!HasOrderClause())
            {
                AddOrderClause(" (SELECT NULL) ");
            }

            if (!Query.ToString().Contains(Clauses.OFFSET.ToString()))
            {
                Query.Append($" {Clauses.OFFSET} {Offset} ROWS FETCH NEXT {Limit} ROWS ONLY ");

            }

            return this;
        }

        internal QueryBuilder<TModel> SimpleQuery()
        {
            string table = GetTableName();
            Query = new StringBuilder($"SELECT _selector_all FROM {table} ");

            return this;
        }

        internal QueryBuilder<TModel> InsertQuery<T>(T Item)
        {

            List<PropertyInfo> Properties = GetFillableProperties();

            string[] Names = new string[Properties.Count];
            object[] Values = new object[Properties.Count];

            foreach (var (item, index) in Properties.Select((item, index) => (item, index)))
            {

                string PropertyName = item.Name;
                object PropertyValue = item.GetValue(Item, null) ?? (new());

                Names[index] = PropertyName;
                Values[index] = $"@{PropertyName}";

            }

            string table = GetTableName();
            Query = new StringBuilder($"INSERT INTO {table} ({String.Join(",", Names)}) VALUES ({String.Join(",", Values)}) ");

            return this;
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
            string primaryKey = GetPrimaryKey();

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
            string primaryKey = GetPrimaryKey();

            Query = new StringBuilder($"UPDATE {table} SET {String.Join(",", Values)} WHERE {primaryKey} = @{primaryKey}");

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

        public QueryBuilder<TModel> WhereIn(string Column, object[] Values)
        {

            string stringValues = $"({string.Join(",", Values)})";
            InitWhere(Column, stringValues, null, LogicalOperators.IN);
            return this;
        }

        public QueryBuilder<TModel> WhereNotIn(string Column, object[] Values)
        {

            string stringValues = $"({string.Join(",", Values)})";
            InitWhere(Column, stringValues, null, LogicalOperators.NOT_IN);
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

        public QueryBuilder<TModel> WhereDate(string Column, string Value)
        {
            InitWhere(Column, Value, null, LogicalOperators.DATE);
            return this;
        }

        public QueryBuilder<TModel> WhereYear(string Column, string Value)
        {
            InitWhere(Column, Value, null, LogicalOperators.YEAR);
            return this;
        }

        public QueryBuilder<TModel> WhereMonth(string Column, string Value)
        {
            InitWhere(Column, Value, null, LogicalOperators.MONTH);
            return this;
        }

        public QueryBuilder<TModel> WhereDay(string Column, string Value)
        {
            InitWhere(Column, Value, null, LogicalOperators.DAY);
            return this;
        }

        public QueryBuilder<TModel> WhereColumn(string FirstColumn, string SecondColumn)
        {
            InitWhere(FirstColumn, SecondColumn, null, LogicalOperators.COLUMN);
            return this;
        }

        public QueryBuilder<TModel> WhereBetween(string Column, Between Value)
        {
            InitWhere(Column, Value, null, LogicalOperators.BETWEEN);
            return this;
        }

        public QueryBuilder<TModel> WhereDateBetween(string Column, DateBetween Value)
        {
            InitWhere(Column, Value, null, LogicalOperators.DATEBETWEEN);
            return this;
        }

        public QueryBuilder<TModel> WhereHas<TRelationship>(Relationship<TRelationship> Relationship, string ComparisonOperator, int Value)
        {
            WhereHasBuilder(Clauses.EXISTS, Relationship, null, ComparisonOperator, Value);
            return this;
        }

        public QueryBuilder<TModel> WhereHas<TRelationship>(Relationship<TRelationship> Relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? Builder = null, string? ComparisonOperator = null, int? Value = null)
        {
            WhereHasBuilder(Clauses.EXISTS, Relationship, Builder, ComparisonOperator, Value);
            return this;
        }

        public QueryBuilder<TModel> WhereDoesntHave<TRelationship>(Relationship<TRelationship> Relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? Builder = null)
        {
            WhereHasBuilder(Clauses.NOT_EXISTS, Relationship, Builder);
            return this;
        }

        public QueryBuilder<TModel> When(bool Condition, Func<SubQuery<TModel>, SubQuery<TModel>>? Builder = null)
        {
            InitWhen(Condition, Builder);
            return this;
        }
       
        public QueryBuilder<TModel> WithCount(string Relationship)
        {

            try
            {
                if (!CheckQueryInit())
                {
                    SimpleQuery();
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

                var parts = result.GetQuery().Split("=");

                string OwnTable = GetTableName();

                string countQuery = string.Concat(parts[0], $" = {OwnTable}.{result.LocalKey}").Replace("_selector_all", "count(*)");

                var regex = new Regex(Regex.Escape("_selector_all"));
                var match = regex.Match(Query.ToString());

                int IndexSelector = match.Index + "_selector_all".Length;

                string replaced = Query.ToString().Insert(IndexSelector, $", ({countQuery}) as {Relationship}_Count ");

                Query = new(replaced);
            }
            catch (NullReferenceException)
            {
                throw new NullReferenceException("Relationship not method found on Model");
            }


            return this;
        }

        public QueryBuilder<TModel> OrderBy(string Column, string Direction = "ASC")
        {
            AddOrderClause(Column, Direction);

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

            string ClauseName = string.Join(" ", Clauses.ORDER_BY.ToString().Split("_"));

            if (!Query.ToString().Contains(ClauseName))
            {
                Query.Append($" {ClauseName} {Column} ");

                if (Direction != null)
                {
                    Query.Append($" {Direction} ");
                }

            }
        }

        internal static string GetPrimaryKey()
        {
            PropertyInfo? primaryAttribute = Instance.GetType().GetProperties().Where(prop => Attribute.IsDefined(prop, typeof(PrimaryKey))).FirstOrDefault();
            return (primaryAttribute != null) ? primaryAttribute.Name : "Id";
        }

        internal static string GetPrimaryKey(object Instance)
        {
            PropertyInfo? primaryAttribute = Instance.GetType().GetProperties().Where(prop => Attribute.IsDefined(prop, typeof(PrimaryKey))).FirstOrDefault();
            return (primaryAttribute != null) ? primaryAttribute.Name : "Id";
        }

        internal static string LastIdQuery()
        {
            string table = GetTableName();
            string PrimaryKey = GetPrimaryKey();

            return $"SELECT TOP 1 {PrimaryKey} FROM {table} ORDER BY {PrimaryKey} DESC";
        }

        protected static List<PropertyInfo> GetFillableProperties()
        {
            List<PropertyInfo> Properties = Instance.GetType().GetProperties().Where(prop => Attribute.IsDefined(prop, typeof(Fillable))).ToList();

            return Properties;
        }

    }
}
