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

        internal string BuildQuery()
        {

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

                Query.Replace(index, squery);
                i++;
            }

            Query.Replace("_selector_all", "*");
            Query.Replace("_selector_count", "count(*)");

            return Regex.Replace(Query.ToString(),@"\s+"," ").Trim();
        }

        internal void GroupCondition(Func<SubQuery<TModel>, SubQuery<TModel>> Builder, LogicalOperators logicalOperator)
        {
            AddWhereClause();

            if (ConditionsAdded != 0)
            {
                Query.Append($" {logicalOperator} ");
            }

            var SBuilder = Builder.Invoke(new SubQuery<TModel>("", Clauses.EXISTS));

            string ExtraCondition = $" ( {SBuilder.GetQuery().Split("WHERE")[1]} ) ";

            foreach (var item in SBuilder.SubQueries)
            {
                SubQueries.Add(item);
            }

            Query.Append($" {ExtraCondition} ");
        }

        internal void InitWhere(string Column, object? Value, string? ComparisonOperator = null, LogicalOperators? logicalOperators = null)
        {

            PropertyInfo? columnProp = Instance.GetType().GetProperties().Where(prop => Attribute.IsDefined(prop, typeof(Fillable)) && prop.Name == Column).FirstOrDefault();

            try
            {
                // var val = (Value != null && !IgnoreCast) ? Convert.ChangeType(Value, columnProp.PropertyType) : Value;

                LogicalOperators Op = logicalOperators != null ? (LogicalOperators)logicalOperators : LogicalOperators.AND;

                if (ComparisonOperator != null)
                {
                    AddCondition(Column, ComparisonOperator, Value, Op);
                }
                else
                {
                    AddCondition(Column, "=", Value, Op);
                }

            }
            catch (NullReferenceException)
            {
                throw new NullReferenceException("Column not found on fillable props");
            }
            catch (FormatException)
            {
                throw new FormatException("Column value not match with column type");
            }
            catch (InvalidCastException)
            {
                throw new FormatException("Column value not match with column type");
            }

        }

        internal void WhereHasBuilder<TRelationship>(Clauses Clause, Relationship<TRelationship> Relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? Builder = null, string? ComparisonOperator = null, int? Value = null)
        {
            if (!HasOrderClause())
            {
                var ReturnInstance = Activator.CreateInstance(typeof(TRelationship))!;

                AddWhereClause();

                if (ConditionsAdded != 0)
                {
                    Query.Append($" {LogicalOperators.AND} ");
                }

                string Table = GetTableName(ReturnInstance);
                string OwnTable = GetTableName();

                SubQuery<TRelationship> SubQueryRelationship = new($" SELECT _selector_all FROM {Table} WHERE {Table}.{Relationship.ForeignKey} = {OwnTable}.{Relationship.LocalKey} ", Clause);

                if (ComparisonOperator != null && Value != null)
                {
                    SubQueryRelationship.Query = new(SubQueryRelationship.Query.ToString().Replace("_selector_all", "_selector_count"));

                    SubQueryRelationship.AsCondition = true;
                    SubQueryRelationship.ConditionOperator = ComparisonOperator;
                    SubQueryRelationship.ConditionValue = Value;
                }


                SubQueries.Add(SubQueryRelationship);

                Query.Append($" SubQuery_{SubQueries.Count} ");

                if (Builder != null)
                {
                    var SBuilder = Builder.Invoke(new SubQuery<TRelationship>("",Clause));

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

                            Query.Append($" {Table}.{Column} {logicalOperators.ToString()} {from} {LogicalOperators.AND.ToString()} {to} ");

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

                        Query.Append($" {Table}.{Column} = {Table}.{Value} ");

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
            string ClauseName = string.Join(" ", Clauses.ORDER_BY.ToString().Split("_"));

            if (Query != null && Query.ToString().Contains(ClauseName))
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
