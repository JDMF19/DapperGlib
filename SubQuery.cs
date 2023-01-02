using DapperGlib.Util;
using System.Text;

namespace DapperGlib
{
    public class SubQuery<TModel> : Builder<TModel>
    {
        internal Clauses Clause { get; set; }

        internal bool AsCondition { get; set; } = false;
        internal string? ConditionOperator { get; set; }
        internal int? ConditionValue { get; set; }

        public SubQuery(string query, Clauses clause)
        {
            Clause = clause;
            Query = new StringBuilder(query);
        }


        public SubQuery<TModel> WhereRaw(string Query)
        {
            AddRaw(Query, LogicalOperators.AND);
            return this;
        }

        public SubQuery<TModel> OrWhereRaw(string Query)
        {
            AddRaw(Query, LogicalOperators.OR);
            return this;
        }

        public SubQuery<TModel> Where(string Column, object? Value)
        {
            InitWhere(Column, Value);
            return this;
        }

        public SubQuery<TModel> Where(string Column, string ComparisonOperator, object? Value)
        {
            InitWhere(Column, Value, ComparisonOperator);
            return this;
        }

        public SubQuery<TModel> Where(Func<SubQuery<TModel>, SubQuery<TModel>> Builder)
        {
            GroupCondition(Builder, LogicalOperators.AND);
            return this;
        }

        public SubQuery<TModel> OrWhere(Func<SubQuery<TModel>, SubQuery<TModel>> Builder)
        {
            GroupCondition(Builder, LogicalOperators.OR);
            return this;
        }

        public SubQuery<TModel> OrWhere(string Column, object? Value)
        {
            InitWhere(Column, Value, null, LogicalOperators.OR);
            return this;
        }

        public SubQuery<TModel> OrWhere(string Column, string ComparisonOperator, object? Value)
        {
            InitWhere(Column, Value, ComparisonOperator, LogicalOperators.OR);
            return this;
        }

        public SubQuery<TModel> WhereNot(Func<SubQuery<TModel>, SubQuery<TModel>> Builder)
        {
            GroupCondition(Builder, LogicalOperators.AND, true);
            return this;
        }

        public SubQuery<TModel> OrWhereNot(Func<SubQuery<TModel>, SubQuery<TModel>> Builder)
        {
            GroupCondition(Builder, LogicalOperators.OR, true);
            return this;
        }

        public SubQuery<TModel> WhereIn(string Column, object[] Values)
        {

            string stringValues = ParseWhereInValues(Values);
            InitWhere(Column, stringValues, null, LogicalOperators.IN);
            return this;
        }

        public SubQuery<TModel> WhereNotIn(string Column, object[] Values)
        {

            string stringValues = $"({string.Join(",", Values)})";
            InitWhere(Column, stringValues, null, LogicalOperators.NOT_IN);
            return this;
        }

        public SubQuery<TModel> WhereNull(string Column)
        {
            InitWhere(Column, null);
            return this;
        }

        public SubQuery<TModel> WhereNotNull(string Column)
        {
            InitWhere(Column, null, null, LogicalOperators.NOT);
            return this;
        }

        public SubQuery<TModel> WhereDate(string Column, string Date)
        {
            InitWhere(Column, Date, null, LogicalOperators.DATE);
            return this;
        }

        public SubQuery<TModel> WhereYear(string Column, string Year)
        {
            InitWhere(Column, Year, null, LogicalOperators.WHEREYEAR);
            return this;
        }

        public SubQuery<TModel> WhereMonth(string Column, string Month)
        {
            InitWhere(Column, Month, null, LogicalOperators.WHEREMONTH);
            return this;
        }

        public SubQuery<TModel> WhereDay(string Column, string Day)
        {
            InitWhere(Column, Day, null, LogicalOperators.WHEREDAY);
            return this;
        }

        public SubQuery<TModel> OrWhereYear(string Column, string Year)
        {
            InitWhere(Column, Year, null, LogicalOperators.ORWHEREYEAR);
            return this;
        }

        public SubQuery<TModel> OrWhereMonth(string Column, string Month)
        {
            InitWhere(Column, Month, null, LogicalOperators.ORWHEREMONTH);
            return this;
        }

        public SubQuery<TModel> OrWhereDay(string Column, string Day)
        {
            InitWhere(Column, Day, null, LogicalOperators.ORWHEREDAY);
            return this;
        }


        /// <summary>
        ///    
        /// </summary>
        /// <param name="Invert">Reverses the order in the query of the Column and Date parameters</param>
        /// <param name="ComparisonType">The comparison types are Year, Month, Day, Minute</param>
        public SubQuery<TModel> WhereDateDiff(string Column, string Date, int Difference, DateDiff ComparisonType, bool Invert = false)
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
        public SubQuery<TModel> WhereDateDiff(string Column, string Date, string ComparisonOperator, int Difference, DateDiff ComparisonType, bool Invert = false)
        {
            LogicalOperators logicalOperator = Enum.TryParse(ComparisonType.ToString(), out LogicalOperators outValue) ? outValue : LogicalOperators.YEAR;
            InitWhere(Column, Date, ComparisonOperator, logicalOperator, Difference, Invert);
            return this;
        }


        public SubQuery<TModel> WhereColumn(string FirstColumn, string SecondColumn)
        {
            InitWhere(FirstColumn, SecondColumn, null, LogicalOperators.COLUMN);
            return this;
        }

        public SubQuery<TModel> WhereColumn(string FirstColumn, string ComparisonOperator, string SecondColumn)
        {
            InitWhere(FirstColumn, SecondColumn, ComparisonOperator, LogicalOperators.COLUMN);
            return this;
        }

        public SubQuery<TModel> WhereBetween(string Column, Between Value)
        {
            InitWhere(Column, Value, null, LogicalOperators.BETWEEN);
            return this;
        }

        public SubQuery<TModel> WhereNotBetween(string Column, Between Value)
        {
            InitWhere(Column, Value, null, LogicalOperators.NOT_BETWEEN);
            return this;
        }

        public SubQuery<TModel> WhereDateBetween(string Column, DateBetween Value)
        {
            InitWhere(Column, Value, null, LogicalOperators.DATEBETWEEN);
            return this;
        }

        public SubQuery<TModel> WhereHas<TRelationship>(string Relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? Builder = null)
        {
            WhereHasBuilder(Clauses.EXISTS, LogicalOperators.AND, Relationship, Builder);
            return this;
        }

        public SubQuery<TModel> WhereHas<TRelationship>(string Relationship, string ComparisonOperator, int Value)
        {
            WhereHasBuilder<TRelationship>(Clauses.EXISTS, LogicalOperators.AND, Relationship, null, ComparisonOperator, Value);
            return this;
        }

        public SubQuery<TModel> WhereHas<TRelationship>(string Relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>> Builder, string ComparisonOperator, int Value)
        {
            WhereHasBuilder(Clauses.EXISTS, LogicalOperators.AND, Relationship, Builder, ComparisonOperator, Value);
            return this;
        }

        public SubQuery<TModel> OrWhereHas<TRelationship>(string Relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? Builder = null)
        {
            WhereHasBuilder(Clauses.EXISTS, LogicalOperators.OR, Relationship, Builder);
            return this;
        }

        public SubQuery<TModel> OrWhereHas<TRelationship>(string Relationship, string ComparisonOperator, int Value)
        {
            WhereHasBuilder<TRelationship>(Clauses.EXISTS, LogicalOperators.OR, Relationship, null, ComparisonOperator, Value);
            return this;
        }

        public SubQuery<TModel> OrWhereHas<TRelationship>(string Relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>> Builder, string ComparisonOperator, int Value)
        {
            WhereHasBuilder(Clauses.EXISTS, LogicalOperators.OR, Relationship, Builder, ComparisonOperator, Value);
            return this;
        }

        public SubQuery<TModel> WhereDoesntHave<TRelationship>(string Relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? Builder = null)
        {
            WhereHasBuilder(Clauses.NOT_EXISTS, LogicalOperators.AND, Relationship, Builder);
            return this;
        }

        public SubQuery<TModel> OrWhereDoesntHave<TRelationship>(string Relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? Builder = null)
        {
            WhereHasBuilder(Clauses.NOT_EXISTS, LogicalOperators.OR, Relationship, Builder);
            return this;
        }


        public SubQuery<TModel> When(bool Condition, Func<SubQuery<TModel>, SubQuery<TModel>>? Builder = null)
        {
            InitWhen(Condition, Builder);
            return this;
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

    }
}
