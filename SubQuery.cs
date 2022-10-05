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

            string stringValues = $"({string.Join(",", Values)})";
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

        public SubQuery<TModel> WhereDate(string Column, string Value)
        {
            InitWhere(Column, Value, null, LogicalOperators.DATE);
            return this;
        }

        public SubQuery<TModel> WhereYear(string Column, string Value)
        {
            InitWhere(Column, Value, null, LogicalOperators.YEAR);
            return this;
        }

        public SubQuery<TModel> WhereMonth(string Column, string Value)
        {
            InitWhere(Column, Value, null, LogicalOperators.MONTH);
            return this;
        }

        public SubQuery<TModel> WhereDay(string Column, string Value)
        {
            InitWhere(Column, Value, null, LogicalOperators.DAY);
            return this;
        }

        public SubQuery<TModel> WhereDateDiffYear(string Column, string Date, int Difference)
        {
            InitWhere(Column, Date, null, LogicalOperators.DATEDIFFYEAR, Difference);
            return this;
        }

        public SubQuery<TModel> WhereDateDiffYear(string Column, string Date, string ComparisonOperator, int Difference)
        {
            InitWhere(Column, Date, ComparisonOperator, LogicalOperators.DATEDIFFYEAR, Difference);
            return this;
        }

        public SubQuery<TModel> WhereDateDiffMonth(string Column, string Date, int Difference)
        {
            InitWhere(Column, Date, null, LogicalOperators.DATEDIFFMONTH, Difference);
            return this;
        }

        public SubQuery<TModel> WhereDateDiffMonth(string Column, string Date, string ComparisonOperator, int Difference)
        {
            InitWhere(Column, Date, ComparisonOperator, LogicalOperators.DATEDIFFMONTH, Difference);
            return this;
        }

        public SubQuery<TModel> WhereDateDiffDay(string Column, string Date, int Difference)
        {
            InitWhere(Column, Date, null, LogicalOperators.DATEDIFFDAY, Difference);
            return this;
        }

        public SubQuery<TModel> WhereDateDiffDay(string Column, string Date, string ComparisonOperator, int Difference)
        {
            InitWhere(Column, Date, ComparisonOperator, LogicalOperators.DATEDIFFDAY, Difference);
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

    }
}
