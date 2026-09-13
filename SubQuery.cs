using DapperGlib.Exceptions;
using DapperGlib.Internal;
using DapperGlib.Util;
using System.Text;
using System.Linq.Expressions;

namespace DapperGlib
{
    public class SubQuery<TModel> : Builder<TModel>
    {
        internal Clauses Clause { get; set; }

        internal bool AsCondition { get; set; } = false;
        internal string? ConditionOperator { get; set; }
        internal int? ConditionValue { get; set; }
        internal string? ConditionParameter { get; set; }

        public SubQuery(string query, Clauses clause)
        {
            Clause = clause;
            Query = new StringBuilder(query);
        }

        internal SubQuery(string query, Clauses clause, QueryParameterContext parameterContext)
        {
            Clause = clause;
            Query = new StringBuilder(query);

            ParameterContext = parameterContext;
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

        public SubQuery<TModel> Where<TValue>(Expression<Func<TModel, TValue>> column, TValue value)
        {
            return Where(ModelPropertyExpression.GetName(column, nameof(Where)), value);
        }

        public SubQuery<TModel> Where<TValue>(Expression<Func<TModel, TValue>> column, string comparisonOperator, TValue value)
        {
            return Where(ModelPropertyExpression.GetName(column, nameof(Where)), comparisonOperator, value);
        }


        public SubQuery<TModel> WhereLike(string Column, string Pattern)
        {
            if (string.IsNullOrWhiteSpace(Column))
            {
                throw new QueryBuilderException(
                    "WhereLike requires a valid column name."
                );
            }

            if (Pattern == null)
            {
                throw new QueryBuilderException(
                    $"WhereLike('{Column}') cannot receive a null pattern."
                );
            }

            InitWhere(
                Column,
                Pattern,
                "LIKE"
            );

            return this;
        }

        public SubQuery<TModel> WhereLike(Expression<Func<TModel, string?>> column, string pattern)
        {
            return WhereLike(ModelPropertyExpression.GetName(column, nameof(WhereLike)), pattern);
        }


        public SubQuery<TModel> WhereContains(string Column, string Value)
        {
            if (string.IsNullOrWhiteSpace(Column))
            {
                throw new QueryBuilderException(
                    "WhereContains requires a valid column name."
                );
            }

            if (Value == null)
            {
                throw new QueryBuilderException(
                    $"WhereContains('{Column}') cannot receive a null value."
                );
            }

            string pattern =
                $"%{EscapeLikePattern(Value)}%";

            return WhereLike(
                Column,
                pattern
            );
        }

        public SubQuery<TModel> WhereContains(Expression<Func<TModel, string?>> column, string value)
        {
            return WhereContains(ModelPropertyExpression.GetName(column, nameof(WhereContains)), value);
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

        public SubQuery<TModel> OrWhere<TValue>(Expression<Func<TModel, TValue>> column, TValue value)
        {
            return OrWhere(ModelPropertyExpression.GetName(column, nameof(OrWhere)), value);
        }

        public SubQuery<TModel> OrWhere<TValue>(Expression<Func<TModel, TValue>> column, string comparisonOperator, TValue value)
        {
            return OrWhere(ModelPropertyExpression.GetName(column, nameof(OrWhere)), comparisonOperator, value);
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


        public SubQuery<TModel> WhereIn<TValue>(string Column, IEnumerable<TValue> Values)
        {
            InitWhereIn(
                Column,
                Values,
                LogicalOperators.IN,
                nameof(WhereIn),
                rejectNullValues: true
            );

            return this;
        }

        public SubQuery<TModel> WhereIn<TValue>(Expression<Func<TModel, TValue>> column, IEnumerable<TValue> values)
        {
            return WhereIn(ModelPropertyExpression.GetName(column, nameof(WhereIn)), values);
        }


        public SubQuery<TModel> WhereNotIn<TValue>(string Column, IEnumerable<TValue> Values)
        {
            InitWhereIn(
                Column,
                Values,
                LogicalOperators.NOT_IN,
                nameof(WhereNotIn),
                rejectNullValues: true
            );

            return this;
        }

        public SubQuery<TModel> WhereNotIn<TValue>(Expression<Func<TModel, TValue>> column, IEnumerable<TValue> values)
        {
            return WhereNotIn(ModelPropertyExpression.GetName(column, nameof(WhereNotIn)), values);
        }


        public SubQuery<TModel> WhereNull(string Column)
        {
            InitWhere(Column, null);
            return this;
        }

        public SubQuery<TModel> WhereNull<TValue>(Expression<Func<TModel, TValue>> column)
        {
            return WhereNull(ModelPropertyExpression.GetName(column, nameof(WhereNull)));
        }


        public SubQuery<TModel> WhereNotNull(string Column)
        {
            InitWhere(Column, null, null, LogicalOperators.NOT);
            return this;
        }

        public SubQuery<TModel> WhereNotNull<TValue>(Expression<Func<TModel, TValue>> column)
        {
            return WhereNotNull(ModelPropertyExpression.GetName(column, nameof(WhereNotNull)));
        }

        public SubQuery<TModel> WhereDate(string Column, string Date)
        {
            InitWhere(Column, Date, null, LogicalOperators.DATE);
            return this;
        }

        public SubQuery<TModel> WhereDate<TValue>(Expression<Func<TModel, TValue>> column, string date)
        {
            return WhereDate(ModelPropertyExpression.GetName(column, nameof(WhereDate)), date);
        }


        public SubQuery<TModel> WhereYear(string Column, string Year)
        {
            InitWhere(Column, Year, null, LogicalOperators.WHEREYEAR);
            return this;
        }

        public SubQuery<TModel> WhereYear<TValue>(Expression<Func<TModel, TValue>> column, string year)
        {
            return WhereYear(ModelPropertyExpression.GetName(column, nameof(WhereYear)), year);
        }

        public SubQuery<TModel> WhereMonth(string Column, string Month)
        {
            InitWhere(Column, Month, null, LogicalOperators.WHEREMONTH);
            return this;
        }

        public SubQuery<TModel> WhereMonth<TValue>(Expression<Func<TModel, TValue>> column, string month)
        {
            return WhereMonth(ModelPropertyExpression.GetName(column, nameof(WhereMonth)), month);
        }

        public SubQuery<TModel> WhereDay(string Column, string Day)
        {
            InitWhere(Column, Day, null, LogicalOperators.WHEREDAY);
            return this;
        }

        public SubQuery<TModel> WhereDay<TValue>(Expression<Func<TModel, TValue>> column, string day)
        {
            return WhereDay(ModelPropertyExpression.GetName(column, nameof(WhereDay)), day);
        }

        public SubQuery<TModel> OrWhereYear(string Column, string Year)
        {
            InitWhere(Column, Year, null, LogicalOperators.ORWHEREYEAR);
            return this;
        }

        public SubQuery<TModel> OrWhereYear<TValue>(Expression<Func<TModel, TValue>> column, string year)
        {
            return OrWhereYear(ModelPropertyExpression.GetName(column, nameof(OrWhereYear)), year);
        }

        public SubQuery<TModel> OrWhereMonth(string Column, string Month)
        {
            InitWhere(Column, Month, null, LogicalOperators.ORWHEREMONTH);
            return this;
        }

        public SubQuery<TModel> OrWhereMonth<TValue>(Expression<Func<TModel, TValue>> column, string month)
        {
            return OrWhereMonth(ModelPropertyExpression.GetName(column, nameof(OrWhereMonth)), month);
        }

        public SubQuery<TModel> OrWhereDay(string Column, string Day)
        {
            InitWhere(Column, Day, null, LogicalOperators.ORWHEREDAY);
            return this;
        }

        public SubQuery<TModel> OrWhereDay<TValue>(Expression<Func<TModel, TValue>> column, string day)
        {
            return OrWhereDay(ModelPropertyExpression.GetName(column, nameof(OrWhereDay)), day);
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


        public SubQuery<TModel> WhereDateDiff<TValue>(Expression<Func<TModel, TValue>> column, string date, int difference, DateDiff comparisonType, bool invert = false)
        {
            return WhereDateDiff(ModelPropertyExpression.GetName(column, nameof(WhereDateDiff)), date, difference, comparisonType, invert);
        }

        public SubQuery<TModel> WhereDateDiff<TValue>(Expression<Func<TModel, TValue>> column, string date, string comparisonOperator, int difference, DateDiff comparisonType, bool invert = false)
        {
            return WhereDateDiff(ModelPropertyExpression.GetName(column, nameof(WhereDateDiff)), date, comparisonOperator, difference, comparisonType, invert);
        }

        public SubQuery<TModel> WhereColumn(string FirstColumn, string SecondColumn)
        {
            FirstColumn =
                ValidateColumn(
                    FirstColumn,
                    nameof(WhereColumn)
                );

            SecondColumn =
                ValidateColumn(
                    SecondColumn,
                    nameof(WhereColumn)
                );

            InitWhere(
                FirstColumn,
                SecondColumn,
                null,
                LogicalOperators.COLUMN
            );

            return this;
        }

        public SubQuery<TModel> WhereColumn(string FirstColumn, string ComparisonOperator, string SecondColumn)
        {
            FirstColumn =
                ValidateColumn(
                    FirstColumn,
                    nameof(WhereColumn)
                );

            SecondColumn =
                ValidateColumn(
                    SecondColumn,
                    nameof(WhereColumn)
                );

            InitWhere(
                FirstColumn,
                SecondColumn,
                ComparisonOperator,
                LogicalOperators.COLUMN
            );

            return this;
        }

        public SubQuery<TModel> WhereColumn<TFirst, TSecond>(Expression<Func<TModel, TFirst>> firstColumn, Expression<Func<TModel, TSecond>> secondColumn)
        {
            return WhereColumn(ModelPropertyExpression.GetName(firstColumn, nameof(WhereColumn)), ModelPropertyExpression.GetName(secondColumn, nameof(WhereColumn)));
        }

        public SubQuery<TModel> WhereColumn<TFirst, TSecond>(Expression<Func<TModel, TFirst>> firstColumn, string comparisonOperator, Expression<Func<TModel, TSecond>> secondColumn)
        {
            return WhereColumn(ModelPropertyExpression.GetName(firstColumn, nameof(WhereColumn)), comparisonOperator, ModelPropertyExpression.GetName(secondColumn, nameof(WhereColumn)));
        }

        public SubQuery<TModel> WhereBetween(string Column, Between Value)
        {
            InitWhere(Column, Value, null, LogicalOperators.BETWEEN);
            return this;
        }

        public SubQuery<TModel> WhereBetween<TValue>(Expression<Func<TModel, TValue>> column, Between value)
        {
            return WhereBetween(ModelPropertyExpression.GetName(column, nameof(WhereBetween)), value);
        }

        public SubQuery<TModel> WhereNotBetween(string Column, Between Value)
        {
            InitWhere(Column, Value, null, LogicalOperators.NOT_BETWEEN);
            return this;
        }

        public SubQuery<TModel> WhereNotBetween<TValue>(Expression<Func<TModel, TValue>> column, Between value)
        {
            return WhereNotBetween(ModelPropertyExpression.GetName(column, nameof(WhereNotBetween)), value);
        }

        public SubQuery<TModel> WhereDateBetween(string Column, DateBetween Value)
        {
            InitWhere(Column, Value, null, LogicalOperators.DATEBETWEEN);
            return this;
        }

        public SubQuery<TModel> WhereDateBetween<TValue>(Expression<Func<TModel, TValue>> column, DateBetween value)
        {
            return WhereDateBetween(ModelPropertyExpression.GetName(column, nameof(WhereDateBetween)), value);
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
