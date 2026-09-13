using DapperGlib.Exceptions;
using DapperGlib.Util;
using System.Linq.Expressions;
using DapperGlib.Relationships;
using System.Reflection;

namespace DapperGlib
{
    public sealed class EagerLoadBuilder<TModel>
    {
        private readonly QueryBuilder<TModel> _query;
        private readonly bool _chaperoneAllowed;
        private readonly string _relationshipName;
        private readonly EagerLoadNode _node;

        private readonly List<(string Column, string Direction, bool Random)> _orders = new();

        internal QueryBuilder<TModel> Query => _query;
        internal int? SkipRows { get; private set; }
        internal int? TakeRows { get; private set; }
        internal bool ChaperoneRequested { get; private set; }
        internal bool HasPagination => SkipRows.HasValue || TakeRows.HasValue;

        internal EagerLoadBuilder(QueryBuilder<TModel> query, EagerLoadNode node)
        {
            _query = query ?? throw new ArgumentNullException(nameof(query));
            _node = node ?? throw new ArgumentNullException(nameof(node));
            _chaperoneAllowed = node.Definition.Kind == RelationshipKind.HasMany;
            _relationshipName = node.Name;
        }

        public EagerLoadBuilder<TModel> WhereRaw(string query)
        {
            _query.WhereRaw(query);
            return this;
        }

        public EagerLoadBuilder<TModel> OrWhereRaw(string query)
        {
            _query.OrWhereRaw(query);
            return this;
        }

        public EagerLoadBuilder<TModel> Where(string column, object? value)
        {
            _query.Where(column, value);
            return this;
        }

        public EagerLoadBuilder<TModel> Where(string column, string comparisonOperator, object? value)
        {
            _query.Where(column, comparisonOperator, value);
            return this;
        }

        public EagerLoadBuilder<TModel> Where(Func<SubQuery<TModel>, SubQuery<TModel>> builder)
        {
            _query.Where(builder);
            return this;
        }

        public EagerLoadBuilder<TModel> Where<TValue>(Expression<Func<TModel, TValue>> column, TValue value)
        {
            _query.Where(column, value);
            return this;
        }

        public EagerLoadBuilder<TModel> Where<TValue>(Expression<Func<TModel, TValue>> column, string comparisonOperator, TValue value)
        {
            _query.Where(column, comparisonOperator, value);
            return this;
        }


        public EagerLoadBuilder<TModel> WhereLike(string column, string pattern)
        {
            _query.WhereLike(column, pattern);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereLike(Expression<Func<TModel, string?>> column, string pattern)
        {
            _query.WhereLike(column, pattern);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereContains(string column, string value)
        {
            _query.WhereContains(column, value);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereContains(Expression<Func<TModel, string?>> column, string value)
        {
            _query.WhereContains(column, value);
            return this;
        }


        public EagerLoadBuilder<TModel> OrWhere(string column, object? value)
        {
            _query.OrWhere(column, value);
            return this;
        }

        public EagerLoadBuilder<TModel> OrWhere(string column, string comparisonOperator, object? value)
        {
            _query.OrWhere(column, comparisonOperator, value);
            return this;
        }

        public EagerLoadBuilder<TModel> OrWhere(Func<SubQuery<TModel>, SubQuery<TModel>> builder)
        {
            _query.OrWhere(builder);
            return this;
        }

        public EagerLoadBuilder<TModel> OrWhere<TValue>(Expression<Func<TModel, TValue>> column, TValue value)
        {
            _query.OrWhere(column, value);
            return this;
        }

        public EagerLoadBuilder<TModel> OrWhere<TValue>(Expression<Func<TModel, TValue>> column, string comparisonOperator, TValue value)
        {
            _query.OrWhere(column, comparisonOperator, value);
            return this;
        }


        public EagerLoadBuilder<TModel> WhereNot(Func<SubQuery<TModel>, SubQuery<TModel>> builder)
        {
            _query.WhereNot(builder);
            return this;
        }

        public EagerLoadBuilder<TModel> OrWhereNot(Func<SubQuery<TModel>, SubQuery<TModel>> builder)
        {
            _query.OrWhereNot(builder);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereIn(string column, object[] values)
        {
            _query.WhereIn(column, values);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereIn<TValue>(string column, IEnumerable<TValue> values)
        {
            _query.WhereIn(column, values);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereIn<TValue>(Expression<Func<TModel, TValue>> column, IEnumerable<TValue> values)
        {
            _query.WhereIn(column, values);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereNotIn(string column, object[] values)
        {
            _query.WhereNotIn(column, values);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereNotIn<TValue>(string column, IEnumerable<TValue> values)
        {
            _query.WhereNotIn(column, values);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereNotIn<TValue>(Expression<Func<TModel, TValue>> column, IEnumerable<TValue> values)
        {
            _query.WhereNotIn(column, values);
            return this;
        }


        public EagerLoadBuilder<TModel> WhereNull(string column)
        {
            _query.WhereNull(column);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereNull<TValue>(Expression<Func<TModel, TValue>> column)
        {
            _query.WhereNull(column);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereNotNull(string column)
        {
            _query.WhereNotNull(column);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereNotNull<TValue>(Expression<Func<TModel, TValue>> column)
        {
            _query.WhereNotNull(column);
            return this;
        }


        public EagerLoadBuilder<TModel> WhereDate(string column, string date)
        {
            _query.WhereDate(column, date);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereDate<TValue>(Expression<Func<TModel, TValue>> column, string date)
        {
            _query.WhereDate(column, date);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereYear(string column, string year)
        {
            _query.WhereYear(column, year);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereYear<TValue>(Expression<Func<TModel, TValue>> column, string year)
        {
            _query.WhereYear(column, year);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereMonth(string column, string month)
        {
            _query.WhereMonth(column, month);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereMonth<TValue>(Expression<Func<TModel, TValue>> column, string month)
        {
            _query.WhereMonth(column, month);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereDay(string column, string day)
        {
            _query.WhereDay(column, day);
            return this;
        }
        public EagerLoadBuilder<TModel> WhereDay<TValue>(Expression<Func<TModel, TValue>> column, string day)
        {
            _query.WhereDay(column, day);
            return this;
        }

        public EagerLoadBuilder<TModel> OrWhereYear(string column, string year)
        {
            _query.OrWhereYear(column, year);
            return this;
        }

        public EagerLoadBuilder<TModel> OrWhereYear<TValue>(Expression<Func<TModel, TValue>> column, string year)
        {
            _query.OrWhereYear(column, year);
            return this;
        }

        public EagerLoadBuilder<TModel> OrWhereMonth(string column, string month)
        {
            _query.OrWhereMonth(column, month);
            return this;
        }

        public EagerLoadBuilder<TModel> OrWhereMonth<TValue>(Expression<Func<TModel, TValue>> column, string month)
        {
            _query.OrWhereMonth(column, month);
            return this;
        }


        public EagerLoadBuilder<TModel> OrWhereDay(string column, string day)
        {
            _query.OrWhereDay(column, day);
            return this;
        }

        public EagerLoadBuilder<TModel> OrWhereDay<TValue>(Expression<Func<TModel, TValue>> column, string day)
        {
            _query.OrWhereDay(column, day);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereDateDiff(string column, string date, int difference, DateDiff comparisonType, bool invert = false)
        {
            _query.WhereDateDiff(column, date, difference, comparisonType, invert);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereDateDiff(string column, string date, string comparisonOperator, int difference, DateDiff comparisonType, bool invert = false)
        {
            _query.WhereDateDiff(column, date, comparisonOperator, difference, comparisonType, invert);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereDateDiff<TValue>(Expression<Func<TModel, TValue>> column, string date, int difference, DateDiff comparisonType, bool invert = false)
        {
            _query.WhereDateDiff(column, date, difference, comparisonType, invert);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereDateDiff<TValue>(Expression<Func<TModel, TValue>> column, string date, string comparisonOperator, int difference, DateDiff comparisonType, bool invert = false)
        {
            _query.WhereDateDiff(column, date, comparisonOperator, difference, comparisonType, invert);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereColumn(string firstColumn, string secondColumn)
        {
            _query.WhereColumn(firstColumn, secondColumn);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereColumn(string firstColumn, string comparisonOperator, string secondColumn)
        {
            _query.WhereColumn(firstColumn, comparisonOperator, secondColumn);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereColumn<TFirst, TSecond>(Expression<Func<TModel, TFirst>> firstColumn, Expression<Func<TModel, TSecond>> secondColumn)
        {
            _query.WhereColumn(firstColumn, secondColumn);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereColumn<TFirst, TSecond>(Expression<Func<TModel, TFirst>> firstColumn, string comparisonOperator, Expression<Func<TModel, TSecond>> secondColumn)
        {
            _query.WhereColumn(firstColumn, comparisonOperator, secondColumn);
            return this;
        }


        public EagerLoadBuilder<TModel> WhereBetween(string column, Between value)
        {
            _query.WhereBetween(column, value);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereBetween<TValue>(Expression<Func<TModel, TValue>> column, Between value)
        {
            _query.WhereBetween(column, value);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereNotBetween(string column, Between value)
        {
            _query.WhereNotBetween(column, value);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereNotBetween<TValue>(Expression<Func<TModel, TValue>> column, Between value)
        {
            _query.WhereNotBetween(column, value);
            return this;
        }


        public EagerLoadBuilder<TModel> WhereDateBetween(string column, DateBetween value)
        {
            _query.WhereDateBetween(column, value);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereDateBetween<TValue>(Expression<Func<TModel, TValue>> column, DateBetween value)
        {
            _query.WhereDateBetween(column, value);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereHas<TRelationship>(string relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? builder = null) where TRelationship : Model<TRelationship>, new()
        {
            _query.WhereHas(relationship, builder);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereHas<TRelationship>(string relationship, string comparisonOperator, int value) where TRelationship : Model<TRelationship>, new()
        {
            _query.WhereHas<TRelationship>(relationship, comparisonOperator, value);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereHas<TRelationship>(string relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>> builder, string comparisonOperator, int value) where TRelationship : Model<TRelationship>, new()
        {
            _query.WhereHas(relationship, builder, comparisonOperator, value);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereHas<TRelationship>(Expression<Func<TModel, IEnumerable<TRelationship>>> relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? builder = null) where TRelationship : Model<TRelationship>, new()
        {
            _query.WhereHas(relationship, builder);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereHas<TRelationship>(Expression<Func<TModel, TRelationship?>> relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? builder = null) where TRelationship : Model<TRelationship>, new()
        {
            _query.WhereHas(relationship, builder);
            return this;
        }

        public EagerLoadBuilder<TModel> OrWhereHas<TRelationship>(string relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? builder = null) where TRelationship : Model<TRelationship>, new()
        {
            _query.OrWhereHas(relationship, builder);
            return this;
        }

        public EagerLoadBuilder<TModel> OrWhereHas<TRelationship>(Expression<Func<TModel, IEnumerable<TRelationship>>> relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? builder = null) where TRelationship : Model<TRelationship>, new()
        {
            _query.OrWhereHas(relationship, builder);
            return this;
        }

        public EagerLoadBuilder<TModel> OrWhereHas<TRelationship>(Expression<Func<TModel, TRelationship?>> relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? builder = null) where TRelationship : Model<TRelationship>, new()
        {
            _query.OrWhereHas(relationship, builder);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereDoesntHave<TRelationship>(string relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? builder = null) where TRelationship : Model<TRelationship>, new()
        {
            _query.WhereDoesntHave(relationship, builder);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereDoesntHave<TRelationship>(Expression<Func<TModel, IEnumerable<TRelationship>>> relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? builder = null) where TRelationship : Model<TRelationship>, new()
        {
            _query.WhereDoesntHave(relationship, builder);
            return this;
        }

        public EagerLoadBuilder<TModel> WhereDoesntHave<TRelationship>(Expression<Func<TModel, TRelationship?>> relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? builder = null) where TRelationship : Model<TRelationship>, new()
        {
            _query.WhereDoesntHave(relationship, builder);
            return this;
        }

        public EagerLoadBuilder<TModel> OrWhereDoesntHave<TRelationship>(string relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? builder = null) where TRelationship : Model<TRelationship>, new()
        {
            _query.OrWhereDoesntHave(relationship, builder);
            return this;
        }

        public EagerLoadBuilder<TModel> OrWhereDoesntHave<TRelationship>(Expression<Func<TModel, IEnumerable<TRelationship>>> relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? builder = null) where TRelationship : Model<TRelationship>, new()
        {
            _query.OrWhereDoesntHave(relationship, builder);
            return this;
        }

        public EagerLoadBuilder<TModel> OrWhereDoesntHave<TRelationship>(Expression<Func<TModel, TRelationship?>> relationship, Func<SubQuery<TRelationship>, SubQuery<TRelationship>>? builder = null) where TRelationship : Model<TRelationship>, new()
        {
            _query.OrWhereDoesntHave(relationship, builder);
            return this;
        }

        public EagerLoadBuilder<TModel> When(bool condition, Func<SubQuery<TModel>, SubQuery<TModel>>? builder = null)
        {
            _query.When(condition, builder);
            return this;
        }

        public EagerLoadBuilder<TModel> OrderBy(string column, string direction = "ASC")
        {
            if (string.IsNullOrWhiteSpace(column))
            {
                throw new QueryBuilderException("OrderBy requires a valid column.");
            }

            if (string.IsNullOrWhiteSpace(direction))
            {
                throw new QueryBuilderException("OrderBy requires a valid direction.");
            }

            string normalizedDirection = direction.Trim().ToUpperInvariant();

            if (normalizedDirection != "ASC" && normalizedDirection != "DESC")
            {
                throw new QueryBuilderException($"OrderBy direction '{direction}' is invalid. Use ASC or DESC.");
            }

            _orders.Add((column.Trim(), normalizedDirection, false));

            return this;
        }

        public EagerLoadBuilder<TModel> InRandomOrder()
        {
            _orders.Add((string.Empty, string.Empty, true));
            return this;
        }

        public EagerLoadBuilder<TModel> Skip(int rows)
        {
            if (rows < 0)
            {
                throw new ArgumentOutOfRangeException(nameof(rows), "Skip cannot be negative.");
            }

            if (!SkipRows.HasValue)
            {
                SkipRows = rows;
            }

            return this;
        }

        public EagerLoadBuilder<TModel> Take(int rows)
        {
            if (rows <= 0)
            {
                throw new ArgumentOutOfRangeException(nameof(rows), "Take must be greater than zero.");
            }

            if (!TakeRows.HasValue)
            {
                TakeRows = rows;
            }

            return this;
        }

        public EagerLoadBuilder<TModel> With(string relationship)
        {
            EagerLoadPlan.AddChildPath(_node, typeof(TModel), relationship);

            return this;
        }

        public EagerLoadBuilder<TModel> With(params string[] relationships)
        {
            if (relationships == null)
            {
                throw new ArgumentNullException(nameof(relationships));
            }

            if (relationships.Length == 0)
            {
                throw new ArgumentException("With requires at least one relationship.", nameof(relationships));
            }

            foreach (string relationship in relationships)
            {
                EagerLoadPlan.AddChildPath(_node, typeof(TModel), relationship);
            }

            return this;
        }

        public EagerLoadBuilder<TModel> With<TRelated>(string relationship, Action<EagerLoadBuilder<TRelated>> constraint) where TRelated : Model<TRelated>, new()
        {
            if (constraint == null)
            {
                throw new ArgumentNullException(nameof(constraint));
            }

            EagerLoadNode child = EagerLoadPlan.AddChildPath(_node, typeof(TModel), relationship);

            if (child.Definition.RelatedType != typeof(TRelated))
            {
                throw new RelationshipException($"Nested relationship '{relationship}' on model '{typeof(TModel).Name}' points to '{child.Definition.RelatedType.Name}', but the constraint uses '{typeof(TRelated).Name}'.");
            }

            child.AddConstraint(constraint);

            return this;
        }

        public EagerLoadBuilder<TModel> With<TRelated>(Expression<Func<TModel, IEnumerable<TRelated>>> relationship) where TRelated : Model<TRelated>, new()
        {
            RelationshipDefinition definition = GetHasManyDefinition(relationship);

            _node.GetOrAddChild(definition);

            return this;
        }

        public EagerLoadBuilder<TModel> With<TRelated>(Expression<Func<TModel, IEnumerable<TRelated>>> relationship, Action<EagerLoadBuilder<TRelated>> constraint) where TRelated : Model<TRelated>, new()
        {
            if (constraint == null)
            {
                throw new ArgumentNullException(nameof(constraint));
            }

            RelationshipDefinition definition = GetHasManyDefinition(relationship);
            EagerLoadNode child = _node.GetOrAddChild(definition);

            child.AddConstraint(constraint);

            return this;
        }

        public EagerLoadBuilder<TModel> With<TRelated>(Expression<Func<TModel, TRelated?>> relationship) where TRelated : Model<TRelated>, new()
        {
            RelationshipDefinition definition = GetSingleDefinition(relationship);

            _node.GetOrAddChild(definition);

            return this;
        }

        public EagerLoadBuilder<TModel> With<TRelated>(Expression<Func<TModel, TRelated?>> relationship, Action<EagerLoadBuilder<TRelated>> constraint) where TRelated : Model<TRelated>, new()
        {
            if (constraint == null)
            {
                throw new ArgumentNullException(nameof(constraint));
            }

            RelationshipDefinition definition = GetSingleDefinition(relationship);
            EagerLoadNode child = _node.GetOrAddChild(definition);

            child.AddConstraint(constraint);

            return this;
        }

        public EagerLoadBuilder<TModel> Chaperone()
        {
            if (!_chaperoneAllowed)
            {
                throw new RelationshipException($"Chaperone can only be used with HasMany relationships. Relationship '{_relationshipName}' does not support it.");
            }

            ChaperoneRequested = true;

            return this;
        }

        internal void ApplyOrdering()
        {
            foreach (var order in _orders)
            {
                if (order.Random)
                {
                    _query.InRandomOrder();
                }
                else
                {
                    _query.OrderBy(order.Column, order.Direction);
                }
            }
        }

        internal string BuildWindowOrderBy(string sourceAlias)
        {
            if (_orders.Count == 0)
            {
                return "(SELECT NULL)";
            }

            return string.Join(", ", _orders.Select(order => order.Random ? "NEWID()" : $"{sourceAlias}.{QuoteColumn(order.Column)} {order.Direction}"));
        }

        private static string QuoteColumn(string column)
        {
            string name = column.Trim();
            int dotIndex = name.LastIndexOf('.');

            if (dotIndex >= 0)
            {
                name = name[(dotIndex + 1)..].Trim();
            }

            if (name.StartsWith("[") && name.EndsWith("]") && name.Length >= 2)
            {
                name = name[1..^1].Replace("]]", "]");
            }

            if (string.IsNullOrWhiteSpace(name))
            {
                throw new QueryBuilderException("OrderBy contains an invalid column.");
            }

            return $"[{name.Replace("]", "]]")}]";
        }

        private static RelationshipDefinition GetHasManyDefinition<TRelated>(Expression<Func<TModel, IEnumerable<TRelated>>> relationship) where TRelated : Model<TRelated>, new()
        {
            PropertyInfo property = RelationshipExpression.GetProperty(relationship, typeof(TModel));
            RelationshipDefinition definition = RelationshipMetadataCache.GetRequired(typeof(TModel), property.Name);

            if (definition.Kind != RelationshipKind.HasMany)
            {
                throw new RelationshipException($"Relationship '{definition.Name}' on model '{typeof(TModel).Name}' is configured as '{definition.Kind}' and cannot be used as a collection relationship.");
            }

            if (definition.RelatedType != typeof(TRelated))
            {
                throw new RelationshipException($"Relationship '{definition.Name}' on model '{typeof(TModel).Name}' points to '{definition.RelatedType.Name}', but the requested nested relationship uses '{typeof(TRelated).Name}'.");
            }

            return definition;
        }

        private static RelationshipDefinition GetSingleDefinition<TRelated>(Expression<Func<TModel, TRelated?>> relationship) where TRelated : Model<TRelated>, new()
        {
            PropertyInfo property = RelationshipExpression.GetProperty(relationship, typeof(TModel));
            RelationshipDefinition definition = RelationshipMetadataCache.GetRequired(typeof(TModel), property.Name);

            if (definition.Kind == RelationshipKind.HasMany)
            {
                throw new RelationshipException($"Relationship '{definition.Name}' on model '{typeof(TModel).Name}' is configured as 'HasMany' and cannot be used as a single relationship.");
            }

            if (definition.RelatedType != typeof(TRelated))
            {
                throw new RelationshipException($"Relationship '{definition.Name}' on model '{typeof(TModel).Name}' points to '{definition.RelatedType.Name}', but the requested nested relationship uses '{typeof(TRelated).Name}'.");
            }

            return definition;
        }





    }
}