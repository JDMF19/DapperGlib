using Dapper;
using DapperGlib.Util;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;

namespace DapperGlib
{
    public class Builder<TModel>
    {
        internal static readonly object Instance = Activator.CreateInstance(typeof(TModel)) ?? (new());
        internal static readonly GlipContext _context = new();

        internal StringBuilder Query { get; set; } = new StringBuilder();
        internal List<object> SubQueries { get; set; } = new();
        internal List<string> CountsRelationship { get; set; } = new();
        internal List<string> OrderList { get; set; } = new();
        internal string? SkipString { get; set; }
        internal string? TakeString { get; set; }

        internal int ConditionsAdded = 0;

        public Builder()
        {

        }

        internal string GetQuery()
        {
            return Query.ToString();
        }

        public string ToSql()
        {
            return BuildQuery();
        }

        internal string SqlAggregate(string ReplaceSelector)
        {
            return BuildQuery(ReplaceSelector);
        }

        internal string BuildQuery(string? ReplaceSelectorAggregate = null)
        {
            var QueryCopy = Query.ToString();

            if (ReplaceSelectorAggregate != null)
            {
                var regex = new Regex(Regex.Escape("_selector_all"));
                QueryCopy = regex.Replace(QueryCopy, ReplaceSelectorAggregate, 1);
            }

            int i = 1;
            foreach (dynamic genericBuilder in SubQueries)
            {

                string index = $"SubQuery_{i}";

                string clause = string.Join(" ", genericBuilder.Clause.ToString().Split("_"));

                string squery;

                if (genericBuilder.AsCondition)
                {
                    squery = $" ( {genericBuilder.ToSql()} ) {genericBuilder.ConditionOperator} {FormatValue(genericBuilder.ConditionValue)} ";
                }
                else
                {
                    squery = $" {clause} ( {genericBuilder.ToSql()} ) ";
                }

                QueryCopy = QueryCopy.Replace(index, squery);
                i++;
            }

            int j = 1;
            foreach (string countQuery in CountsRelationship)
            {
                string index = $"count_relationship_{j}";
                if (ReplaceSelectorAggregate == null)
                {
                    QueryCopy = QueryCopy.Replace(index, countQuery);
                }
                else
                {
                    QueryCopy = QueryCopy.Replace(index, "");
                }
                
                j++;
            }

            if (SkipString != null)
            {
                QueryCopy = QueryCopy.Replace("skip_string", SkipString);
            }

            if (TakeString != null)
            {
                QueryCopy = QueryCopy.Replace("take_string", TakeString);
            }

            QueryCopy = QueryCopy.Replace("_selector_all", "*");
            QueryCopy = QueryCopy.Replace("_selector_count", "count(*)");

            int x = 1;
            foreach (var order in OrderList)
            {
                string index = $"order_clause_{x}";

                if (ReplaceSelectorAggregate == null)
                {
                    QueryCopy = QueryCopy.Replace(index, order);
                }
                else
                {
                    QueryCopy = QueryCopy.Replace(index, "");
                }

                x++;
            }


            return Regex.Replace(QueryCopy, @"\s+"," ").Trim();
        }

        internal void GroupCondition(Func<SubQuery<TModel>, SubQuery<TModel>> Builder, LogicalOperators logicalOperator, bool Reverse = false)
        {
            AddWhereClause();

            if (ConditionsAdded != 0)
            {
                Query.Append($" {logicalOperator} ");
            }

            var SBuilder = Builder.Invoke(new SubQuery<TModel>("", Clauses.EXISTS));

            string reverse = "";

            if (Reverse)
            {
                reverse = LogicalOperators.NOT.ToString();
            }


            string ExtraCondition = $" {reverse} ( {SBuilder.GetQuery().Split("WHERE")[1]} ) ";

            foreach (var item in SBuilder.SubQueries)
            {
                SubQueries.Add(item);
            }

            Query.Append($" {ExtraCondition} ");
        }

        internal void InitWhere(string Column, object? Value, string? ComparisonOperator = null, LogicalOperators? logicalOperators = null)
        {

            //PropertyInfo? columnProp = Instance.GetType().GetProperties().Where(prop => Attribute.IsDefined(prop, typeof(Fillable)) && prop.Name == Column).FirstOrDefault();


            LogicalOperators Op = logicalOperators != null ? (LogicalOperators)logicalOperators : LogicalOperators.AND;

            if (ComparisonOperator != null)
            {
                AddCondition(Column, ComparisonOperator, Value, Op);
            }
            else
            {
                AddCondition(Column, "=", Value, Op);
            }

            //try
            //{


            //}
            //catch (NullReferenceException)
            //{
            //    throw new NullReferenceException("Column not found on fillable props");
            //}
            //catch (FormatException)
            //{
            //    throw new FormatException("Column value not match with column type");
            //}
            //catch (InvalidCastException)
            //{
            //    throw new FormatException("Column value not match with column type");
            //}

        }

        internal void WhereHasBuilder<TRelationship>(Clauses Clause, string Relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? Builder = null, string? ComparisonOperator = null, int? Value = null)
        {
            if (!HasOrderClause())
            {

                var ReturnInstance = Activator.CreateInstance(typeof(TRelationship))!;

                var property = Instance.GetType().GetProperty(Relationship);

                if (property == null)
                {
                    throw new NullReferenceException("Relationship property not found on Model");
                }

                var propertyValue = property.GetValue(Instance);

                if (propertyValue == null)
                {
                    throw new NullReferenceException("Relationship property not has a Relationship value");
                }

                Relationship<TRelationship> relationship = (Relationship<TRelationship>)propertyValue;

                AddWhereClause();

                if (ConditionsAdded != 0)
                {
                    Query.Append($" {LogicalOperators.AND} ");
                }

                string Table = GetTableName(ReturnInstance);
                string OwnTable = GetTableName();

                SubQuery<TRelationship> SubQueryRelationship = new($" SELECT _selector_all FROM {Table} WHERE {Table}.{relationship.ForeignKey} = {OwnTable}.{relationship.LocalKey} ", Clause);

                if (ComparisonOperator != null && Value != null)
                {
                    SubQueryRelationship.Query = new StringBuilder(SubQueryRelationship.Query.ToString().Replace("_selector_all", "_selector_count"));

                    SubQueryRelationship.AsCondition = true;
                    SubQueryRelationship.ConditionOperator = ComparisonOperator;
                    SubQueryRelationship.ConditionValue = Value;
                }

                SubQueries.Add(SubQueryRelationship);

                Query.Append($" SubQuery_{SubQueries.Count} ");

                if (Builder != null)
                {

                    var SBuilder = Builder.Invoke(new("", Clause));

                    string ExtraCondition = $"{LogicalOperators.AND} {SBuilder.GetQuery().Split("WHERE")[1]}";

                    foreach (var item in SBuilder.SubQueries)
                    {
                        SubQueryRelationship.SubQueries.Add(item);
                    }

                    SubQueryRelationship.Query.Append($" {ExtraCondition} ");

                }

                ConditionsAdded += 1;
            }

        }

        //internal void WhereHasBuilder<TRelationship>(Clauses Clause, Relationship<TRelationship> Relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? Builder = null, string? ComparisonOperator = null, int? Value = null)
        //{
        //    if (!HasOrderClause())
        //    {
        //        var ReturnInstance = Activator.CreateInstance(typeof(TRelationship))!;

        //        AddWhereClause();

        //        if (ConditionsAdded != 0)
        //        {
        //            Query.Append($" {LogicalOperators.AND} ");
        //        }

        //        string Table = GetTableName(ReturnInstance);
        //        string OwnTable = GetTableName();

        //        SubQuery<TRelationship> SubQueryRelationship = new($" SELECT _selector_all FROM {Table} WHERE {Table}.{Relationship.ForeignKey} = {OwnTable}.{Relationship.LocalKey} ", Clause);

        //        if (ComparisonOperator != null && Value != null)
        //        {
        //            SubQueryRelationship.Query = new(SubQueryRelationship.Query.ToString().Replace("_selector_all", "_selector_count"));

        //            SubQueryRelationship.AsCondition = true;
        //            SubQueryRelationship.ConditionOperator = ComparisonOperator;
        //            SubQueryRelationship.ConditionValue = Value;
        //        }


        //        SubQueries.Add(SubQueryRelationship);

        //        Query.Append($" SubQuery_{SubQueries.Count} ");

        //        if (Builder != null)
        //        {
        //            var SBuilder = Builder.Invoke(new SubQuery<TRelationship>("",Clause));

        //            string ExtraCondition = $"{LogicalOperators.AND} {SBuilder.GetQuery().Split("WHERE")[1]}";

        //            foreach (var item in SBuilder.SubQueries)
        //            {
        //                SubQueryRelationship.SubQueries.Add(item);
        //            }

        //            SubQueryRelationship.Query.Append($" {ExtraCondition} ");

        //        }

        //        ConditionsAdded += 1;
        //    }

        //}

        internal void InitWhen(bool Condition, Func<SubQuery<TModel>, SubQuery<TModel>>? Builder = null)
        {
            if (Condition && !HasOrderClause() && Builder != null)
            {
                var SBuilder = Builder.Invoke(new SubQuery<TModel>("", Clauses.EXISTS));

                AddWhereClause();

                if (ConditionsAdded != 0)
                {
                    Query.Append($" {LogicalOperators.AND} ");
                }

                var ExtraCondition = $" {SBuilder.GetQuery().Split("WHERE")[1]} ";

                foreach (var item in SBuilder.SubQueries)
                {
                    SubQueries.Add(item);
                }

                Query.Append($" {ExtraCondition} ");

                ConditionsAdded += 1;
            }

        }

        internal void AddCondition(string Column, string ComparisonOperator, object? Value, LogicalOperators logicalOperators)
        {
            if (!HasOrderClause())
            {
                AddWhereClause();

                string Table = GetTableName();

                switch (logicalOperators)
                {
                    case LogicalOperators.AND:
                        object? AndValue = (Value == null) ? "IS NULL" : $"{ComparisonOperator} {FormatValue(Value)}";

                        if (ConditionsAdded != 0)
                        {
                            Query.Append($" {logicalOperators.ToString()} ");
                        }

                        Query.Append($" {Table}.{Column} {AndValue} ");

                        break;
                    case LogicalOperators.OR:
                        object? OrValue = (Value == null) ? "IS NULL" : $"{ComparisonOperator} {FormatValue(Value)}";

                        if (ConditionsAdded != 0)
                        {
                            Query.Append($" {logicalOperators.ToString()} ");
                        }

                        Query.Append($" {Table}.{Column} {OrValue} ");
                        break;
                    case LogicalOperators.IN:

                        if (ConditionsAdded != 0)
                        {
                            Query.Append($" {LogicalOperators.AND.ToString()} ");
                        }

                        Query.Append($" {Table}.{Column} {logicalOperators.ToString()} {Value} ");

                        break;
                    case LogicalOperators.NOT_IN:

                        if (ConditionsAdded != 0)
                        {
                            Query.Append($" {LogicalOperators.AND.ToString()} ");
                        }

                        Query.Append($" {Table}.{Column} {LogicalOperators.NOT.ToString()} {LogicalOperators.IN.ToString()} {Value} ");

                        break;
                    case LogicalOperators.LIKE:
                        break;
                    case LogicalOperators.NOT:

                        if (ConditionsAdded != 0)
                        {
                            Query.Append($" {LogicalOperators.AND.ToString()} ");
                        }

                        object? NotValue = (Value == null) ? "IS NULL" : $"{ComparisonOperator} {FormatValue(Value)}";

                        Query.Append($" {logicalOperators.ToString()} {Table}.{Column} {NotValue} ");

                        break;

                    case LogicalOperators.NOT_BETWEEN:
                    case LogicalOperators.BETWEEN:

                        if (Value != null)
                        {
                            if (ConditionsAdded != 0)
                            {
                                Query.Append($" {LogicalOperators.AND.ToString()} ");
                            }

                            Between obj = (Between)Value;

                            var from = FormatValue(obj.From);
                            var to = FormatValue(obj.To);

                            if (logicalOperators == LogicalOperators.NOT_BETWEEN)
                            {
                                Query.Append($" {LogicalOperators.NOT} ({Table}.{Column} {LogicalOperators.BETWEEN.ToString()} {from} {LogicalOperators.AND.ToString()} {to}) ");
                            }
                            else
                            {
                                Query.Append($" {Table}.{Column} {logicalOperators.ToString()} {from} {LogicalOperators.AND.ToString()} {to} ");

                            }

                        }

                        break;
                    case LogicalOperators.DATE:

                        if (ConditionsAdded != 0)
                        {
                            Query.Append($" {LogicalOperators.AND.ToString()} ");
                        }

                        Query.Append($" DATEDIFF(DAY, {Table}.{Column}, {FormatValue(Value)}) = 0  ");

                        break;
                    case LogicalOperators.YEAR:
                    case LogicalOperators.MONTH:
                    case LogicalOperators.DAY:

                        if (ConditionsAdded != 0)
                        {
                            Query.Append($" {LogicalOperators.AND.ToString()} ");
                        }

                        Query.Append($" {logicalOperators}({Table}.{Column}) = {FormatValue(Value)} ");

                        break;
                    case LogicalOperators.DATEBETWEEN:

                        if (Value != null)
                        {
                            if (ConditionsAdded != 0)
                            {
                                Query.Append($" {LogicalOperators.AND.ToString()} ");
                            }

                            DateBetween obj = (DateBetween)Value;

                            var from = FormatValue(obj.From);
                            var to = FormatValue(obj.To);

                            Query.Append($" {Table}.{Column} {LogicalOperators.BETWEEN.ToString()} {from} {LogicalOperators.AND.ToString()} DATEADD(s,-1,DATEADD(d,1,{to})) ");

                        }

                        break;
                    case LogicalOperators.COLUMN:

                        if (ConditionsAdded != 0)
                        {
                            Query.Append($" {LogicalOperators.AND.ToString()} ");
                        }

                        Query.Append($" {Table}.{Column} {ComparisonOperator} {Table}.{Value} ");

                        break;
                    default:
                        break;
                }

                ConditionsAdded += 1;
            }

        }

        internal void AddWhereClause()
        {
            if (!CheckQueryInit())
            {
                string table = GetTableName();

                Query = new StringBuilder($"SELECT _selector_all FROM {table} ");
            }

            if (!Query.ToString().Contains(Clauses.WHERE.ToString()))
            {
                Query.Append($" {Clauses.WHERE.ToString()} ");
            }
        }

        internal bool HasOrderClause()
        {
            if (OrderList.Count > 0)
            {
                return true;
            }
            return false;
        }

        internal bool CheckQueryInit()
        {
            return Query != null && Query.ToString() != "";
        }

        internal static object? FormatValue(object? Value)
        {
            if (Value != null)
            {

                object? value;

                switch (Value.GetType().Name)
                {
                    case "String":
                        value = $"'{Value}'";
                        break;
                    case "Object":
                        value = "''";
                        break;
                    case "DateTime":
                        DateTime date = (DateTime)Value;
                        value = $"'{date:yyyy-mm-dd hh:mm:ss}'";
                        break;
                    default:
                        value = Value;
                        break;
                }


                return value;
            }

            return Value;
        }

        internal static string GetTableName()
        {
            PropertyInfo? schema = GetPropertyInfoByAttribute(typeof(Schema));
            PropertyInfo? tableAttribute = GetPropertyInfoByAttribute(typeof(TableName));

            var schemaValue = schema?.GetValue(Instance, null);
            var tableValue = tableAttribute?.GetValue(Instance, null);

            string? tableName = (tableValue != null) ? (string)tableValue : Instance.GetType().Name;
            string table = (schemaValue != null) ? $"{(string)schemaValue}.{tableName}" : tableName;

            return table;
        }

        internal static string GetTableName(object Instance)
        {
            PropertyInfo? schema = GetPropertyInfoByAttribute(Instance, typeof(Schema));
            PropertyInfo? tableAttribute = GetPropertyInfoByAttribute(Instance, typeof(TableName));

            var schemaValue = schema?.GetValue(Instance, null);
            var tableValue = tableAttribute?.GetValue(Instance, null);

            string? tableName = (tableValue != null) ? (string)tableValue : Instance.GetType().Name;
            string table = (schemaValue != null) ? $"{(string)schemaValue}.{tableName}" : tableName;

            return table;
        }

        internal static PropertyInfo? GetPropertyInfoByAttribute(Type type)
        {
            PropertyInfo? proterty = Instance.GetType().GetProperties().Where(prop => Attribute.IsDefined(prop, type)).FirstOrDefault();
            return proterty;
        }

        internal static PropertyInfo? GetPropertyInfoByAttribute(object Instance, Type type)
        {
            PropertyInfo? proterty = Instance.GetType().GetProperties().Where(prop => Attribute.IsDefined(prop, type)).FirstOrDefault();
            return proterty;
        }

    }
}
