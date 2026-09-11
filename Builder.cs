using Dapper;
using DapperGlib.Exceptions;
using DapperGlib.Internal;
using DapperGlib.Util;
using System.Data;
using System.Globalization;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;

namespace DapperGlib
{
    public class Builder<TModel>
    {
        internal static readonly object Instance = Activator.CreateInstance(typeof(TModel)) ?? (new());
        internal static readonly GlipContext _context = new();
        internal static readonly DatabaseCommandExecutor _executor = new(_context);

        internal StringBuilder Query { get; set; } = new StringBuilder();
        internal List<object> SubQueries { get; set; } = new();
        internal List<string> CountsRelationship { get; set; } = new();
        internal QueryParameterContext ParameterContext { get; set; } = new();

        internal List<string> OrderList { get; set; } = new();
        internal string[] SelectList { get; set; } = Array.Empty<string>();
        internal string? SkipString { get; set; }
        internal string? TakeString { get; set; }
        internal int ConditionsAdded = 0;
        internal bool UnderRelationship { get; set; } = false;

        internal int? QueryCommandTimeout
        {
            get;
            set;
        }

        private bool ParenthesisAdded { get; set; } = false;

        public Builder()
        {

        }

        internal string GetQuery()
        {
            return Query.ToString();
        }

        public string ToSql()
        {
            string sql = BuildQuery();

            return ExpandParameters(sql);
        }

        public string ToParameterizedSql()
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
                    squery = $" ( {genericBuilder.ToParameterizedSql()} ) {genericBuilder.ConditionOperator} {FormatValue(genericBuilder.ConditionValue)} ";
                }
                else
                {
                    squery = $" {clause} ( {genericBuilder.ToParameterizedSql()} ) ";
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

            if (UnderRelationship && ParenthesisAdded)
            {
                QueryCopy = QueryCopy.Replace("__parenthesis__", " ( ");
                var index = QueryCopy.IndexOf("ORDER BY");
                if (index >= 0)
                {
                    QueryCopy = QueryCopy.Insert(index, " ) ");
                }
                else
                {
                    QueryCopy += " ) ";
                }
            }
            else
            {
                QueryCopy = QueryCopy.Replace("__parenthesis__", "");
            }

            return Regex.Replace(QueryCopy, @"\s+", " ").Trim();
        }

        internal void GroupCondition(Func<SubQuery<TModel>, SubQuery<TModel>> Builder, LogicalOperators logicalOperator, bool Reverse = false)
        {

            var SBuilder = Builder.Invoke(new SubQuery<TModel>("", Clauses.EXISTS, ParameterContext));

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

        internal void InitWhere(string Column, object? Value, string? ComparisonOperator = null, LogicalOperators? logicalOperators = null, object? ExtraValue = null, bool Invert = false)
        {
            Column =
                ValidateColumn(
                    Column,
                    "Where"
                );

            LogicalOperators Op =
                logicalOperators != null
                    ? (LogicalOperators)logicalOperators
                    : LogicalOperators.AND;

            if (ComparisonOperator != null)
            {
                AddCondition(
                    Column,
                    ComparisonOperator,
                    Value,
                    Op,
                    ExtraValue,
                    Invert
                );
            }
            else
            {
                AddCondition(
                    Column,
                    "=",
                    Value,
                    Op,
                    ExtraValue,
                    Invert
                );
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
                    throw new RelationshipException(
                        $"Relationship '{Relationship}' was not found " +
                        $"on model '{Instance.GetType().Name}'."
                    );
                }

                if (property.PropertyType != typeof(Relationship<TRelationship>))
                {
                    throw new RelationshipException(
                        $"Relationship '{Relationship}' on model " +
                        $"'{Instance.GetType().Name}' must be of type " +
                        $"'Relationship<{typeof(TRelationship).Name}>'."
                    );
                }

                var propertyValue = property.GetValue(Instance);

                if (propertyValue == null)
                {
                    throw new RelationshipException(
                        $"Relationship '{Relationship}' on model " +
                        $"'{Instance.GetType().Name}' is not initialized."
                    );
                }

                Relationship<TRelationship> relationship = (Relationship<TRelationship>)propertyValue;

                AddWhereClause();

                if (ConditionsAdded != 0)
                {
                    Query.Append($" {Operator} ");
                }

                AddParenthesisGroupRelationship();

                string Table = GetTableName(ReturnInstance);
                string OwnTable = GetTableName();

                SubQuery<TRelationship> SubQueryRelationship = new($" SELECT _selector_all FROM {Table} WHERE {Table}.{relationship.ForeignKey} = {OwnTable}.{relationship.LocalKey} ", Clause, ParameterContext);

                if (ComparisonOperator != null && Value != null)
                {
                    SubQueryRelationship.Query = new StringBuilder(SubQueryRelationship.Query.ToString().Replace("_selector_all", "_selector_count"));

                    SubQueryRelationship.AsCondition = true;
                    SubQueryRelationship.ConditionOperator = ComparisonOperator;
                    SubQueryRelationship.ConditionValue = Value;
                    SubQueryRelationship.ConditionParameter = AddParameter(Value);
                }

                SubQueries.Add(SubQueryRelationship);

                Query.Append($" SubQuery_{SubQueries.Count} ");

                if (Builder != null)
                {

                    var SBuilder = Builder.Invoke(new("", Clause, ParameterContext));

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
                var SBuilder = Builder.Invoke(new SubQuery<TModel>("", Clauses.EXISTS, ParameterContext));

                var parts = SBuilder.GetQuery().Split("WHERE");

                if (parts.Length == 2)
                {

                    AddWhereClause();

                    if (ConditionsAdded != 0)
                    {
                        Query.Append($" {LogicalOperators.AND} ");
                    }

                    AddParenthesisGroupRelationship();

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

                AddParenthesisGroupRelationship();

                Query.Append($" {QueryRaw} ");

                ConditionsAdded += 1;

            }
        }

        internal void AddCondition(string Column, string ComparisonOperator, object? Value, LogicalOperators logicalOperators, object? ExtraValue = null, bool Invert = false)
        {
            if (CanAddCondition())
            {
                AddWhereClause();


                string Table = GetTableName();

                switch (logicalOperators)
                {
                    case LogicalOperators.AND:
                        string AndValue = Value == null ? "IS NULL" : $"{ComparisonOperator} {AddParameter(Value)}";

                        if (ConditionsAdded != 0)
                        {
                            Query.Append($" {logicalOperators} ");
                        }

                        AddParenthesisGroupRelationship();

                        Query.Append(
                            $" {Table}.{Column} {AndValue} "
                        );

                        break;
                    case LogicalOperators.OR:
                        string OrValue = Value == null ? "IS NULL" : $"{ComparisonOperator} {AddParameter(Value)}";

                        if (ConditionsAdded != 0)
                        {
                            Query.Append($" {logicalOperators} ");
                        }

                        AddParenthesisGroupRelationship();

                        Query.Append(
                            $" {Table}.{Column} {OrValue} "
                        );
                        break;
                    case LogicalOperators.IN:

                        if (ConditionsAdded != 0)
                        {
                            Query.Append($" {LogicalOperators.AND} ");
                        }

                        AddParenthesisGroupRelationship();

                        string inParameter = AddParameter(Value);

                        Query.Append(
                            $" {Table}.{Column} IN {inParameter} "
                        );

                        break;
                    case LogicalOperators.NOT_IN:

                        if (ConditionsAdded != 0)
                        {
                            Query.Append($" {LogicalOperators.AND} ");
                        }

                        AddParenthesisGroupRelationship();

                        string notInParameter = AddParameter(Value);

                        Query.Append(
                            $" {Table}.{Column} NOT IN {notInParameter} "
                        );

                        break;
                    case LogicalOperators.LIKE:
                        break;
                    case LogicalOperators.NOT:

                        if (ConditionsAdded != 0)
                        {
                            Query.Append($" {LogicalOperators.AND.ToString()} ");
                        }
                        AddParenthesisGroupRelationship();

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
                            AddParenthesisGroupRelationship();


                            Between obj = (Between)Value;

                            var from = AddParameter(obj.From);
                            var to = AddParameter(obj.To);

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
                        AddParenthesisGroupRelationship();

                        string dateParameter = AddParameter(Value);

                        Query.Append($" DATEDIFF(DAY, {Table}.{Column}, {dateParameter}) = 0  ");

                        break;
                    case LogicalOperators.YEAR:
                    case LogicalOperators.MONTH:
                    case LogicalOperators.DAY:
                    case LogicalOperators.MINUTE:

                        if (ConditionsAdded != 0)
                        {
                            Query.Append($" {LogicalOperators.AND.ToString()} ");
                        }
                        AddParenthesisGroupRelationship();

                        var valueParameter = AddParameter(Value);
                        var differenceParameter = AddParameter(ExtraValue);

                        if (Invert)
                        {
                            Query.Append($" DATEDIFF({logicalOperators}, {Table}.{Column}, {valueParameter}) {ComparisonOperator} {differenceParameter} ");
                        }
                        else
                        {
                            Query.Append($" DATEDIFF({logicalOperators}, {valueParameter}, {Table}.{Column}) {ComparisonOperator} {differenceParameter} ");
                        }

                        break;

                    case LogicalOperators.WHEREYEAR:

                        if (ConditionsAdded != 0)
                        {
                            Query.Append($" {LogicalOperators.AND.ToString()} ");
                        }
                        AddParenthesisGroupRelationship();

                        Query.Append($" YEAR({Table}.{Column}) = {AddParameter(Value)} ");

                        break;
                    case LogicalOperators.WHEREMONTH:

                        if (ConditionsAdded != 0)
                        {
                            Query.Append($" {LogicalOperators.AND.ToString()} ");
                        }
                        AddParenthesisGroupRelationship();

                        Query.Append($" MONTH({Table}.{Column}) = {AddParameter(Value)} ");

                        break;
                    case LogicalOperators.WHEREDAY:

                        if (ConditionsAdded != 0)
                        {
                            Query.Append($" {LogicalOperators.AND.ToString()} ");
                        }
                        AddParenthesisGroupRelationship();

                        Query.Append($" DAY({Table}.{Column}) = {AddParameter(Value)} ");

                        break;
                    case LogicalOperators.ORWHEREYEAR:

                        if (ConditionsAdded != 0)
                        {
                            Query.Append($" {LogicalOperators.OR.ToString()} ");
                        }
                        AddParenthesisGroupRelationship();

                        Query.Append($" YEAR({Table}.{Column}) = {AddParameter(Value)} ");

                        break;
                    case LogicalOperators.ORWHEREMONTH:

                        if (ConditionsAdded != 0)
                        {
                            Query.Append($" {LogicalOperators.OR.ToString()} ");
                        }
                        AddParenthesisGroupRelationship();

                        Query.Append($" MONTH({Table}.{Column}) = {AddParameter(Value)} ");

                        break;
                    case LogicalOperators.ORWHEREDAY:

                        if (ConditionsAdded != 0)
                        {
                            Query.Append($" {LogicalOperators.OR.ToString()} ");
                        }
                        AddParenthesisGroupRelationship();

                        Query.Append($" DAY({Table}.{Column}) = {AddParameter(Value)} ");

                        break;
                    case LogicalOperators.DATEBETWEEN:

                        if (Value != null)
                        {
                            if (ConditionsAdded != 0)
                            {
                                Query.Append($" {LogicalOperators.AND.ToString()} ");
                            }
                            AddParenthesisGroupRelationship();

                            DateBetween obj = (DateBetween)Value;

                            var from = AddParameter(obj.From);
                            var to = AddParameter(obj.To);

                            Query.Append($" {Table}.{Column} {LogicalOperators.BETWEEN.ToString()} {from} {LogicalOperators.AND.ToString()} DATEADD(s,-1,DATEADD(d,1,{to})) ");

                        }

                        break;
                    case LogicalOperators.COLUMN:

                        if (ConditionsAdded != 0)
                        {
                            Query.Append($" {LogicalOperators.AND.ToString()} ");
                        }
                        AddParenthesisGroupRelationship();

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

        private void AddParenthesisGroupRelationship()
        {

            if (UnderRelationship && !ParenthesisAdded)
            {
                Query.Append(" __parenthesis__ ");
                ParenthesisAdded = true;
            }

        }

        internal static string ReplaceLastOccurrence(string Source, string Find, string Replace)
        {
            int place = Source.LastIndexOf(Find);

            if (place == -1)
                return Source;

            string result = Source.Remove(place, Find.Length).Insert(place, Replace);
            return result;
        }

        internal static string FormatValue(object? value)
        {
            if (value == null)
            {
                return "NULL";
            }

            if (value is Enum enumValue)
            {
                return Convert
                    .ToInt64(enumValue)
                    .ToString(CultureInfo.InvariantCulture);
            }

            return value switch
            {
                string text =>
                    $"'{text.Replace("'", "''")}'",

                char character =>
                    $"'{character.ToString().Replace("'", "''")}'",

                DateTime date =>
                    $"'{date:yyyy-MM-dd HH:mm:ss.fff}'",

                DateTimeOffset date =>
                    $"'{date:yyyy-MM-dd HH:mm:ss.fff zzz}'",

                bool boolean =>
                    boolean ? "1" : "0",

                Guid guid =>
                    $"'{guid}'",

                byte[] bytes =>
                    $"0x{Convert.ToHexString(bytes)}",

                IFormattable formattable =>
                    formattable.ToString(
                        null,
                        CultureInfo.InvariantCulture
                    ) ?? "NULL",

                _ =>
                    $"'{value.ToString()?.Replace("'", "''")}'"
            };
        }

        protected static string EscapeLikePattern(string value)
        {
            return value
                .Replace("[", "[[]")
                .Replace("%", "[%]")
                .Replace("_", "[_]");
        }

        public IReadOnlyDictionary<string, object?> GetBindings()
        {
            return ParameterContext.Values
                .ToDictionary(
                    item => $"@{item.Key}",
                    item => item.Value
                );
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

        public static string GetConnectionString()
        {
            PropertyInfo? connectionProperty = GetPropertyInfoByName("Connection");

            var connectionPropertyValue = connectionProperty?.GetValue(Instance, null);

            string? value = (connectionPropertyValue != null) ? (string)connectionPropertyValue : "SqlConnection";

            return value;
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

        internal static PropertyInfo? GetPropertyInfoByName(string Name)
        {
            PropertyInfo? proterty = Instance.GetType().GetProperties().Where(prop => prop.Name == Name).FirstOrDefault();
            return proterty;
        }

        internal static PropertyInfo? GetPropertyInfoByAttribute(object Instance, Type type)
        {
            PropertyInfo? proterty = Instance.GetType().GetProperties().Where(prop => Attribute.IsDefined(prop, type)).FirstOrDefault();
            return proterty;
        }

        internal static List<TValue> PrepareWhereInValues<TValue>(string column, IEnumerable<TValue>? values, string methodName, bool rejectNullValues)
        {
            if (values == null)
            {
                throw new ArgumentNullException(
                    nameof(values),
                    $"{methodName}('{column}') cannot receive a null collection."
                );
            }

            var list = values.ToList();

            if (list.Count == 0)
            {
                throw new ArgumentException(
                    $"{methodName}('{column}') cannot receive an empty collection.",
                    nameof(values)
                );
            }

            if (rejectNullValues &&
                list.Any(value => value is null))
            {
                throw new ArgumentException(
                    $"{methodName}('{column}') cannot contain null values. " +
                    $"Use WhereNull() explicitly when querying NULL values.",
                    nameof(values)
                );
            }

            return list;
        }

        internal void InitWhereIn<TValue>(string column, IEnumerable<TValue>? values, LogicalOperators logicalOperator, string methodName, bool rejectNullValues)
        {
            column =
                ValidateColumn(
                    column,
                    methodName
                );

            var list =
                PrepareWhereInValues(
                    column,
                    values,
                    methodName,
                    rejectNullValues
                );

            InitWhere(
                column,
                list,
                null,
                logicalOperator
            );
        }

        internal static string ValidateColumn(string? column, string methodName)
        {
            if (string.IsNullOrWhiteSpace(column))
            {
                throw new QueryBuilderException(
                    $"{methodName} requires a valid column."
                );
            }

            return column.Trim();
        }

        internal static string[] ValidateColumns(IEnumerable<string>? columns, string methodName)
        {
            if (columns == null)
            {
                throw new ArgumentNullException(
                    nameof(columns),
                    $"{methodName} cannot receive a null column collection."
                );
            }

            var columnList =
                columns.ToArray();

            if (columnList.Length == 0)
            {
                throw new QueryBuilderException(
                    $"{methodName} requires at least one column."
                );
            }

            var result =
                new string[columnList.Length];

            for (int i = 0; i < columnList.Length; i++)
            {
                if (string.IsNullOrWhiteSpace(columnList[i]))
                {
                    throw new QueryBuilderException(
                        $"{methodName} contains an invalid column " +
                        $"at index {i}."
                    );
                }

                result[i] =
                    columnList[i].Trim();
            }

            return result;
        }


        internal string AddParameter(object? value)
        {
            return ParameterContext.Add(value);
        }

        internal DynamicParameters GetExecutionParameters(object? extraParameters = null)
        {
            var parameters = new DynamicParameters();

            foreach (var parameter in ParameterContext.Values)
            {
                parameters.Add(
                    parameter.Key,
                    parameter.Value
                );
            }

            if (extraParameters != null)
            {
                parameters.AddDynamicParams(extraParameters);
            }

            return parameters;
        }

        internal string ExpandParameters(string sql)
        {
            var parameters = ParameterContext.Values
                .OrderByDescending(x => x.Key.Length);

            foreach (var parameter in parameters)
            {
                sql = sql.Replace(
                    $"@{parameter.Key}",
                    FormatParameterForSql(parameter.Value)
                );
            }

            return sql;
        }


        internal CommandDefinition CreateCommand(string sql, object? extraParameters = null, CancellationToken cancellationToken = default, IDbTransaction? transaction = null)
        {
            return CommandDefinitionFactory.Create(
                commandText: sql,
                parameters: GetExecutionParameters(extraParameters),
                commandTimeout: GetCommandTimeout(),
                transaction: transaction,
                cancellationToken: cancellationToken
            );
        }

        internal CommandDefinition CreateCommand(string commandText, DynamicParameters parameters, CommandType commandType, CancellationToken cancellationToken = default, IDbTransaction? transaction = null)
        {
            return CommandDefinitionFactory.Create(
                commandText: commandText,
                parameters: parameters,
                commandTimeout: GetCommandTimeout(),
                commandType: commandType,
                transaction: transaction,
                cancellationToken: cancellationToken
            );
        }

        internal int ExecuteCommand(string sql, object? extraParameters = null)
        {
            return _executor.Execute(GetConnectionString(), context =>
            {
                CommandDefinition command = CreateCommand(sql, extraParameters, transaction: context.Transaction);
                return context.Connection.Execute(command);
            });
        }

        internal async Task<int> ExecuteCommandAsync(string sql, object? extraParameters = null, CancellationToken cancellationToken = default)
        {
            return await _executor.ExecuteAsync(GetConnectionString(), context =>
            {
                CommandDefinition command = CreateCommand(sql, extraParameters, cancellationToken, context.Transaction);
                return context.Connection.ExecuteAsync(command);
            }, cancellationToken).ConfigureAwait(false);
        }

        internal TResult ExecuteScalar<TResult>(string sql, object? extraParameters = null)
        {
            return _executor.Execute(GetConnectionString(), context =>
            {
                CommandDefinition command = CreateCommand(sql, extraParameters, transaction: context.Transaction);
                return context.Connection.ExecuteScalar<TResult>(command)!;
            });
        }

        internal async Task<TResult> ExecuteScalarAsync<TResult>(string sql, object? extraParameters = null, CancellationToken cancellationToken = default)
        {
            return (await _executor.ExecuteAsync(GetConnectionString(), context =>
            {
                CommandDefinition command = CreateCommand(sql, extraParameters, cancellationToken, context.Transaction);
                return context.Connection.ExecuteScalarAsync<TResult>(command);
            }, cancellationToken).ConfigureAwait(false))!;
        }

        internal TResult QueryFirst<TResult>(string sql, object? extraParameters = null)
        {
            return _executor.Execute(GetConnectionString(), context =>
            {
                CommandDefinition command = CreateCommand(sql, extraParameters, transaction: context.Transaction);
                return context.Connection.QueryFirst<TResult>(command);
            });
        }

        internal async Task<TResult> QueryFirstAsync<TResult>(string sql, object? extraParameters = null, CancellationToken cancellationToken = default)
        {
            return await _executor.ExecuteAsync(GetConnectionString(), context =>
            {
                CommandDefinition command = CreateCommand(sql, extraParameters, cancellationToken, context.Transaction);
                return context.Connection.QueryFirstAsync<TResult>(command);
            }, cancellationToken).ConfigureAwait(false);
        }

        internal TResult? QueryFirstOrDefault<TResult>(string sql, object? extraParameters = null)
        {
            return _executor.Execute(GetConnectionString(), context =>
            {
                CommandDefinition command = CreateCommand(sql, extraParameters, transaction: context.Transaction);
                return context.Connection.QueryFirstOrDefault<TResult>(command);
            });
        }

        internal async Task<TResult?> QueryFirstOrDefaultAsync<TResult>(string sql, object? extraParameters = null, CancellationToken cancellationToken = default)
        {
            return await _executor.ExecuteAsync(GetConnectionString(), context =>
            {
                CommandDefinition command = CreateCommand(sql, extraParameters, cancellationToken, context.Transaction);
                return context.Connection.QueryFirstOrDefaultAsync<TResult>(command);
            }, cancellationToken).ConfigureAwait(false);
        }

        internal List<TResult> QueryList<TResult>(string sql, object? extraParameters = null)
        {
            return _executor.Execute(GetConnectionString(), context =>
            {
                CommandDefinition command = CreateCommand(sql, extraParameters, transaction: context.Transaction);
                return context.Connection.Query<TResult>(command).AsList();
            });
        }

        internal async Task<List<TResult>> QueryListAsync<TResult>(string sql, object? extraParameters = null, CancellationToken cancellationToken = default)
        {
            IEnumerable<TResult> result = await _executor.ExecuteAsync(GetConnectionString(), context =>
            {
                CommandDefinition command = CreateCommand(sql, extraParameters, cancellationToken, context.Transaction);
                return context.Connection.QueryAsync<TResult>(command);
            }, cancellationToken).ConfigureAwait(false);

            return result.AsList();
        }

        internal TResult QuerySingle<TResult>(string sql, object? extraParameters = null)
        {
            return _executor.Execute(GetConnectionString(), context =>
            {
                CommandDefinition command = CreateCommand(sql, extraParameters, transaction: context.Transaction);
                return context.Connection.QuerySingle<TResult>(command);
            });
        }

        internal async Task<TResult> QuerySingleAsync<TResult>(string sql, object? extraParameters = null, CancellationToken cancellationToken = default)
        {
            return await _executor.ExecuteAsync(GetConnectionString(), context =>
            {
                CommandDefinition command = CreateCommand(sql, extraParameters, cancellationToken, context.Transaction);
                return context.Connection.QuerySingleAsync<TResult>(command);
            }, cancellationToken).ConfigureAwait(false);
        }

        internal TResult? QuerySingleOrDefault<TResult>(string sql, object? extraParameters = null)
        {
            return _executor.Execute(GetConnectionString(), context =>
            {
                CommandDefinition command = CreateCommand(sql, extraParameters, transaction: context.Transaction);
                return context.Connection.QuerySingleOrDefault<TResult>(command);
            });
        }

        internal async Task<TResult?> QuerySingleOrDefaultAsync<TResult>(string sql, object? extraParameters = null, CancellationToken cancellationToken = default)
        {
            return await _executor.ExecuteAsync(GetConnectionString(), context =>
            {
                CommandDefinition command = CreateCommand(sql, extraParameters, cancellationToken, context.Transaction);
                return context.Connection.QuerySingleOrDefaultAsync<TResult>(command);
            }, cancellationToken).ConfigureAwait(false);
        }


        internal static string FormatParameterForSql(object? value)
        {
            if (value == null)
            {
                return "NULL";
            }

            if (value is System.Collections.IEnumerable enumerable &&
                value is not string &&
                value is not byte[])
            {
                var values = new List<string>();

                foreach (var item in enumerable)
                {
                    values.Add(FormatValue(item));
                }

                return $"({string.Join(",", values)})";
            }

            return FormatValue(value);
        }

        internal int? GetCommandTimeout()
        {
            return QueryCommandTimeout ?? _context.CommandTimeout;
        }

    }
}
