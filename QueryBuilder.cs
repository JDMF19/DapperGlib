using Dapper;
using DapperGlib.Util;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using System;
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

        public QueryBuilder(QueryBuilder<TModel> Clone)
        {
            var deserializeSettings = new JsonSerializerSettings { ObjectCreationHandling = ObjectCreationHandling.Replace };

            Query = new(Clone.Query.ToString());
            SubQueries = JsonConvert.DeserializeObject<List<object>>(JsonConvert.SerializeObject(Clone.SubQueries), deserializeSettings) ?? new();
            CountsRelationship = JsonConvert.DeserializeObject<List<string>>(JsonConvert.SerializeObject(Clone.CountsRelationship), deserializeSettings) ?? new();
            OrderList = JsonConvert.DeserializeObject<List<string>>(JsonConvert.SerializeObject(Clone.OrderList), deserializeSettings) ?? new();
            SelectList = JsonConvert.DeserializeObject<string[]>(JsonConvert.SerializeObject(Clone.SelectList), deserializeSettings) ?? Array.Empty<string>();
            SkipString = Clone.SkipString?.Trim();
            TakeString = Clone.TakeString?.Trim();
            ConditionsAdded = Clone.ConditionsAdded;
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

            List<PropertyInfo> Properties = GetFillableProperties();

            var primaryKey = GetPropertyInfoByAttribute(Item!, typeof(PrimaryKey));

            int length = !IsIncrementing() && primaryKey != null ? Properties.Count + 1 : Properties.Count;

            string[] Names = new string[length];
            object[] Values = new object[length];


            foreach (var (item, index) in Properties.Select((item, index) => (item, index)))
            {

                string PropertyName = item.Name;

                Names[index] = PropertyName;
                Values[index] = $"@{PropertyName}";

            }

            if (!IsIncrementing() && primaryKey != null)
            {
                Names[length - 1] = primaryKey.Name;
                Values[length - 1] = $"@{primaryKey.Name}";
            }

            string table = GetTableName();
            Query = new StringBuilder($" INSERT INTO {table} ({String.Join(",", Names)}) VALUES ({String.Join(",", Values)})  ");

            if (IsIncrementing() && primaryKey != null)
            {
                Query.Append($"SELECT {primaryKey.Name} FROM {table} WHERE {primaryKey.Name} = @@IDENTITY");
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

            using var conection = _context.CreateConnection(GetConnectionString());
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

            using var conection = _context.CreateConnection(GetConnectionString());
            var result = conection.ExecuteAsync(ToSql(), item);
            return Task.FromResult(result.Result);
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
                throw new ApplicationException("Primary Key Column is not defined");
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
                throw new ApplicationException("Primary Key Column is not defined");
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
                throw new ApplicationException("Primary Key Column is not defined");
            }

            Query = new StringBuilder($"{Clauses.DELETE} FROM {table} WHERE {primaryKey} = @{primaryKey}");

            using var conection = _context.CreateConnection(GetConnectionString());
            conection.Execute(ToSql(), Item);

        }

        internal Task<int> SimpleDeleteAsync<T>(T Item)
        {
            string table = GetTableName();
            string? primaryKey = GetPrimaryKey();

            if (primaryKey == null)
            {
                throw new ApplicationException("Primary Key Column is not defined");
            }

            Query = new StringBuilder($"{Clauses.DELETE} FROM {table} WHERE {primaryKey} = @{primaryKey}");

            using var conection = _context.CreateConnection(GetConnectionString());
            var result = conection.ExecuteAsync(ToSql(), Item);

            return Task.FromResult(result.Result);
        }

        internal void Truncate()
        {
            string table = GetTableName();
          
            Query = new StringBuilder($"{Clauses.TRUNCATE} TABLE {table}");

            using var conection = _context.CreateConnection(GetConnectionString());
            conection.Execute(ToSql());
        }

        internal Task<int> TruncateAsync()
        {
            string table = GetTableName();
       
            Query = new StringBuilder($"{Clauses.TRUNCATE} TABLE {table}");

            using var conection = _context.CreateConnection(GetConnectionString());
            var result = conection.ExecuteAsync(ToSql());

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

            using var conection = _context.CreateConnection(GetConnectionString());
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

            using var conection = _context.CreateConnection(GetConnectionString());
            var result = conection.ExecuteAsync(ToSql());

            return Task.FromResult(result.Result);
        }
        
        #endregion

        #region Retrieving

        public TModel First()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            string query = ToSql();

            using var conection = _context.CreateConnection(GetConnectionString());

            var item = conection.QueryFirst<TModel>(query);
            return item;
        }

        public T First<T>()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            string query = ToSql();

            using var conection = _context.CreateConnection(GetConnectionString());

            var item = conection.QueryFirst<T>(query);
            return item;
        }

        public TModel? FirstOrDefault()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            string query = ToSql();

            using var conection = _context.CreateConnection(GetConnectionString());
            var item = conection.QueryFirstOrDefault<TModel>(query);
            return item;
        }

        public T? FirstOrDefault<T>()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            string query = ToSql();

            using var conection = _context.CreateConnection(GetConnectionString());
            var item = conection.QueryFirstOrDefault<T>(query);
            return item;
        }

        public Task<TModel> FirstAsync()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            string query = ToSql();

            using var conection = _context.CreateConnection(GetConnectionString());

            var item = conection.QueryFirstAsync<TModel>(query)!;

            return Task.FromResult(item.Result);
        }

        public Task<T> FirstAsync<T>()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            string query = ToSql();

            using var conection = _context.CreateConnection(GetConnectionString());

            var item = conection.QueryFirstAsync<T>(query)!;

            return Task.FromResult(item.Result);
        }

        public Task<TModel?> FirstOrDefaultAsync()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            string query = ToSql();

            using var conection = _context.CreateConnection(GetConnectionString());

            var item = conection.QueryFirstOrDefaultAsync<TModel?>(query)!;

            return Task.FromResult(item.Result);
        }

        public Task<T?> FirstOrDefaultAsync<T>()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            string query = ToSql();

            using var conection = _context.CreateConnection(GetConnectionString());

            var item = conection.QueryFirstOrDefaultAsync<T?>(query)!;

            return Task.FromResult(item.Result);
        }

        public List<TModel> ToList()
        {

            string query = ToSql();

            using var conection = _context.CreateConnection(GetConnectionString());
            var lista = conection.Query<TModel>(query);
            return lista.ToList();
        }

        public List<T> ToList<T>()
        {

            string query = ToSql();

            using var conection = _context.CreateConnection(GetConnectionString());
            var lista = conection.Query<T>(query);
            return lista.ToList();
        }

        public Task<List<TModel>> ToListAsync()
        {
            string query = ToSql();

            using var conection = _context.CreateConnection(GetConnectionString());

            var lista = conection.QueryAsync<TModel>(query);

            return Task.FromResult(lista.Result.ToList());
        }

        public Task<List<T>> ToListAsync<T>()
        {
            string query = ToSql();

            using var conection = _context.CreateConnection(GetConnectionString());

            var lista = conection.QueryAsync<T>(query);

            return Task.FromResult(lista.Result.ToList());
        }

        public bool Exists()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            string sql = ToSql();

            using var conection = _context.CreateConnection(GetConnectionString());

            var result = conection.Query<bool>($"SELECT 1 {Clauses.WHERE} {Clauses.EXISTS} ({sql})");

            return result.FirstOrDefault();
        }

        public bool DoesntExist()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            string sql = ToSql();

            using var conection = _context.CreateConnection(GetConnectionString());

            var result = conection.Query<bool>($"SELECT 1 {Clauses.WHERE} {LogicalOperators.NOT} {Clauses.EXISTS} ({sql})");

            return result.FirstOrDefault();
        }

        public int Count()
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            using var conection = _context.CreateConnection(GetConnectionString());

            var result = conection.Query<int>(SqlAggregate($"count(*) as CountColumn"));

            return result.FirstOrDefault();
        }

        public string Value(string Column)
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            using var conection = _context.CreateConnection(GetConnectionString());

            var item = conection.QueryFirst<string>(SqlAggregate($"{Column} as ValueColumn"));

            return item;

            //var property = Instance.GetType().GetProperty(Column);

            //if (property == null)
            //{
            //    return item;
            //}

            //var PropType = property.PropertyType;

            //var val = Convert.ChangeType(item, PropType);

            //return val;
        }

        public double Max(string Column)
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }
            //SelectList.Concat(new string[] { $"max({Column})" }).ToArray();

            using var conection = _context.CreateConnection(GetConnectionString());

            var item = conection.QueryFirst<double>(SqlAggregate($"max({Column})"));

            return item;
        }

        public double Min(string Column)
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            using var conection = _context.CreateConnection(GetConnectionString());

            var item = conection.QueryFirst<double>(SqlAggregate($"min({Column})"));

            return item;
        }

        public double Avg(string Column)
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            using var conection = _context.CreateConnection(GetConnectionString());

            var item = conection.QueryFirst<double>(SqlAggregate($"avg({Column})"));

            return item;
        }

        public double Sum(string Column)
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            using var conection = _context.CreateConnection(GetConnectionString());

            var item = conection.QueryFirst<double>(SqlAggregate($"sum({Column})"));

            return item;
        }
        #endregion

        public QueryBuilder<TModel> Select(params string[] Columns)
        {
            SelectList = Columns;
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
            string stringValues = ParseWhereInValues(Values);
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
            InitWhere(FirstColumn, SecondColumn, null, LogicalOperators.COLUMN);
            return this;
        }

        public QueryBuilder<TModel> WhereColumn(string FirstColumn, string ComparisonOperator, string SecondColumn)
        {
            InitWhere(FirstColumn, SecondColumn, ComparisonOperator, LogicalOperators.COLUMN);
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
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            if (CountsRelationship.Count > 0)
            {
                throw new ApplicationException("Distinct method is incompatible with the WithCount method");
            }

            Query.Replace("_selector_all", $"{Clauses.DISTINCT} {Columns}");

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
                throw new NullReferenceException("Relationship property not found on Model");
            }


            return this;
        }

        public QueryBuilder<TModel> GroupBy(params string[] Columns)
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            if (!HasGroupByClause())
            {
                string ClauseName = string.Join(" ", Clauses.GROUP_BY.ToString().Split("_"));
                string cols = string.Join(",", Columns);

                Query.Append($" {ClauseName} {cols} ");
            }


            return this;
        }

        public QueryBuilder<TModel> Having(string Column, string ComparisonOperator, object Value)
        {
            if (!CheckQueryInit())
            {
                SimpleQuery();
            }

            if (!Query.ToString().Contains(Clauses.HAVING.ToString()) && HasGroupByClause())
            {
                Query.Append($" {Clauses.HAVING} {Column} {ComparisonOperator} {FormatValue(Value)}");
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

        internal static string? LastIdQuery()
        {
            string table = GetTableName();
            string? PrimaryKey = GetPrimaryKey();

            if (PrimaryKey != null && IsIncrementing())
            {
                return $"SELECT TOP 1 {PrimaryKey} FROM {table} ORDER BY {PrimaryKey} DESC";
            }

            return null;
        }

        protected static List<PropertyInfo> GetFillableProperties()
        {
            List<PropertyInfo> Properties = Instance.GetType().GetProperties().Where(prop => Attribute.IsDefined(prop, typeof(Fillable))).ToList();

            return Properties;
        }

        internal static string ParseWhereInValues(object[] Values)
        {
            var result = new StringBuilder("");

            foreach (var item in Values)
            {
                var value = FormatValue(item);

                if (result.ToString() != "")
                {
                    result.Append($",{value}");
                }
                else
                {
                    result.Append($"{value}");
                }

            }

            string stringValues = $"({result})";

            return stringValues;
        }

        public QueryBuilder<TModel> Clone()
        {
            return new QueryBuilder<TModel>(this);
        }
    }
}
