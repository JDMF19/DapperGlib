using Dapper;
using DapperGlib.Util;
using Newtonsoft.Json.Linq;
using System;
using System.Data.Common;
using System.Linq;
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
        internal string[] SelectList { get; set; } = Array.Empty<string>();
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
                string replace = ReplaceSelectorAggregate == null ? countQuery : "";
                QueryCopy = QueryCopy.Replace(index, replace);
                j++;
            }

            if (SkipString != null)
            {
                string replace = ReplaceSelectorAggregate == null ? SkipString : "";
                QueryCopy = QueryCopy.Replace("skip_string", replace);
            }

            if (TakeString != null)
            {
                string replace = ReplaceSelectorAggregate == null ? TakeString : "";
                QueryCopy = QueryCopy.Replace("take_string", replace);
            }

            if (SelectList.Length > 0)
            {
                string selectString = string.Join(",", SelectList);
                QueryCopy = QueryCopy.Replace("_selector_all", selectString);
            }

            QueryCopy = QueryCopy.Replace("_selector_all", "*");
            QueryCopy = QueryCopy.Replace("_selector_count", "count(*)");

            int x = 1;
            foreach (var order in OrderList)
            {
                string index = $"order_clause_{x}";
                string replace = ReplaceSelectorAggregate == null ? order : "";
                QueryCopy = QueryCopy.Replace(index, replace);
                x++;
            }
           

            return Regex.Replace(QueryCopy, @"\s+"," ").Trim();
        }

        internal void GroupCondition(Func<SubQuery<TModel>, SubQuery<TModel>> Builder, LogicalOperators logicalOperator, bool Reverse = false)
        {

            var SBuilder = Builder.Invoke(new SubQuery<TModel>("", Clauses.EXISTS));
            var parts = SBuilder.GetQuery().Split("WHERE");

            if (parts.Length == 2 && CanAddCondition())
            {
                AddWhereClause();

                string reverse = "";

                if (Reverse)
                {
                    reverse = LogicalOperators.NOT.ToString();
                }

                if (ConditionsAdded != 0)
                {
                    Query.Append($" {logicalOperator} ");
                }

                string ExtraCondition = $" {reverse} ( {parts[1]} ) ";
                int index = 0;

                foreach (var item in SBuilder.SubQueries)
                {
                    index++;

                    SubQueries.Add(item);

                    string validSubqueryIndex = $" SubQuery_{SubQueries.Count} ";

                    ExtraCondition = ReplaceLastOccurrence(ExtraCondition, $"SubQuery_{index}", validSubqueryIndex).Trim();
                }

                Query.Append($" {ExtraCondition} ");

                ConditionsAdded++;
            }
          
        }

        internal void InitWhere(string Column, object? Value, string? ComparisonOperator = null, LogicalOperators? logicalOperators = null, object? ExtraValue = null)
        {

            //PropertyInfo? columnProp = Instance.GetType().GetProperties().Where(prop => Attribute.IsDefined(prop, typeof(Fillable)) && prop.Name == Column).FirstOrDefault();

            LogicalOperators Op = logicalOperators != null ? (LogicalOperators)logicalOperators : LogicalOperators.AND;

            if (ComparisonOperator != null)
            {
                AddCondition(Column, ComparisonOperator, Value, Op, ExtraValue);
            }
            else
            {
                AddCondition(Column, "=", Value, Op, ExtraValue);
            }


        }

        internal void WhereHasBuilder<TRelationship>(Clauses Clause, LogicalOperators Operator, string Relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? Builder = null, string? ComparisonOperator = null, int? Value = null)
        {
            if (CanAddCondition())
            {

                var ReturnInstance = Activator.CreateInstance(typeof(TRelationship))!;

                var property = Instance.GetType().GetProperty(Relationship);

                if (property == null)
                {
                    throw new NullReferenceException($"Relationship property '{Relationship}' not found on Model '{Instance.GetType().Name}'");
                }

                if (property.PropertyType != typeof(Relationship<TRelationship>))
                {
                    throw new NullReferenceException($"Relationship property '{Relationship}' must be of type  'Relationship<{ReturnInstance.GetType().Name}>'");
                }

                var propertyValue = property.GetValue(Instance);
              
                if (propertyValue == null)
                {
                    throw new NullReferenceException($"Relationship property '{Relationship}' not initialized");
                }

                Relationship<TRelationship> relationship = (Relationship<TRelationship>)propertyValue;

                AddWhereClause();

                if (ConditionsAdded != 0)
                {
                    Query.Append($" {Operator} ");
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

                    var parts = SBuilder.GetQuery().Split("WHERE");

                    if (parts.Length == 2)
                    {
                        string ExtraCondition = $"{LogicalOperators.AND} {parts[1]}";

                        foreach (var item in SBuilder.SubQueries)
                        {
                            SubQueryRelationship.SubQueries.Add(item);
                        }

                        SubQueryRelationship.Query.Append($" {ExtraCondition} ");
                    }

                }

                ConditionsAdded += 1;
            }

        }

        internal void InitWhen(bool Condition, Func<SubQuery<TModel>, SubQuery<TModel>>? Builder = null)
        {
            if (Condition && CanAddCondition() && Builder != null)
            {
                var SBuilder = Builder.Invoke(new SubQuery<TModel>("", Clauses.EXISTS));
                var parts = SBuilder.GetQuery().Split("WHERE");

                if (parts.Length == 2)
                {

                    AddWhereClause();

                    if (ConditionsAdded != 0)
                    {
                        Query.Append($" {LogicalOperators.AND} ");
                    }

                    string ExtraCondition = $"{parts[1]}";

                    int index = 0;
                    foreach (var item in SBuilder.SubQueries)
                    {
                        index++;

                        SubQueries.Add(item);

                        string validSubqueryIndex = $" SubQuery_{SubQueries.Count} ";

                        ExtraCondition = ReplaceLastOccurrence(ExtraCondition, $"SubQuery_{index}", validSubqueryIndex).Trim();

                    }

                    Query.Append($" {ExtraCondition} ");

                    ConditionsAdded += 1;
                }

              
            }

        }

        internal void AddRaw(string QueryRaw, LogicalOperators logicalOperators)
        {
            if (CanAddCondition())
            {

                AddWhereClause();

                if (ConditionsAdded != 0)
                {
                    Query.Append($" {logicalOperators.ToString()} ");
                }

                Query.Append($" {QueryRaw} ");

                ConditionsAdded += 1;

            }
        }

        internal void AddCondition(string Column, string ComparisonOperator, object? Value, LogicalOperators logicalOperators, object? ExtraValue = null)
        {
            if (CanAddCondition())
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
                    case LogicalOperators.DATEDIFFYEAR:

                        if (ConditionsAdded != 0)
                        {
                            Query.Append($" {LogicalOperators.AND.ToString()} ");
                        }

                        Query.Append($" DATEDIFF(YEAR, {FormatValue(Value)}, {Table}.{Column}) {ComparisonOperator} {FormatValue(ExtraValue)} ");

                        break;
                    case LogicalOperators.DATEDIFFMONTH:

                        if (ConditionsAdded != 0)
                        {
                            Query.Append($" {LogicalOperators.AND.ToString()} ");
                        }

                        Query.Append($" DATEDIFF(MONTH, {FormatValue(Value)}, {Table}.{Column}) {ComparisonOperator} {FormatValue(ExtraValue)} ");

                        break;
                    case LogicalOperators.DATEDIFFDAY:

                        if (ConditionsAdded != 0)
                        {
                            Query.Append($" {LogicalOperators.AND.ToString()} ");
                        }

                        Query.Append($" DATEDIFF(DAY, {FormatValue(Value)}, {Table}.{Column}) {ComparisonOperator} {FormatValue(ExtraValue)} ");

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

        internal bool CanAddCondition()
        {
            if (OrderList.Count > 0)
            {
                return false;
            }

            if (HasGroupByClause())
            {
                return false;
            }

            if (Query.ToString().Contains(Clauses.HAVING.ToString()))
            {
                return false;
            }

            return true;
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

        internal bool HasGroupByClause()
        {
            return Query.ToString().Contains(string.Join(" ", Clauses.GROUP_BY.ToString().Split("_")));
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

        internal static string ReplaceLastOccurrence(string Source, string Find, string Replace)
        {
            int place = Source.LastIndexOf(Find);

            if (place == -1)
                return Source;

            string result = Source.Remove(place, Find.Length).Insert(place, Replace);
            return result;
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

        public static string GetTableName()
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
