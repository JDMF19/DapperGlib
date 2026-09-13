using DapperGlib.Exceptions;
using DapperGlib.Interfaces;
using System.Reflection;
using System.Runtime.ExceptionServices;
using System.Text;
using System.Collections;


namespace DapperGlib.Relationships
{
    internal static class EagerLoader
    {
        private static readonly MethodInfo LoadHasManyMethod = typeof(EagerLoader).GetMethod(nameof(LoadHasManyCore), BindingFlags.NonPublic | BindingFlags.Static)!;
        private static readonly MethodInfo LoadHasOneMethod = typeof(EagerLoader).GetMethod(nameof(LoadHasOneCore), BindingFlags.NonPublic | BindingFlags.Static)!;
        private static readonly MethodInfo LoadHasManyAsyncMethod = typeof(EagerLoader).GetMethod(nameof(LoadHasManyCoreAsync), BindingFlags.NonPublic | BindingFlags.Static)!;
        private static readonly MethodInfo LoadHasOneAsyncMethod = typeof(EagerLoader).GetMethod(nameof(LoadHasOneCoreAsync), BindingFlags.NonPublic | BindingFlags.Static)!;

        private const int SqlServerParameterBudget = 2000;
        private const string EagerKeysParameterName = "__dglib_eager_keys";

        internal static void Load<TModel>(IReadOnlyList<TModel> models, EagerLoadPlan plan, int? commandTimeout = null)
        {
            if (models == null)
            {
                throw new ArgumentNullException(nameof(models));
            }

            if (plan == null)
            {
                throw new ArgumentNullException(nameof(plan));
            }

            if (models.Count == 0 || !plan.HasLoads)
            {
                return;
            }

            LoadNodes(models, plan.Roots, commandTimeout);
        }

        internal static async Task LoadAsync<TModel>(IReadOnlyList<TModel> models, EagerLoadPlan plan, int? commandTimeout = null, CancellationToken cancellationToken = default)
        {
            if (models == null)
            {
                throw new ArgumentNullException(nameof(models));
            }

            if (plan == null)
            {
                throw new ArgumentNullException(nameof(plan));
            }

            if (models.Count == 0 || !plan.HasLoads)
            {
                return;
            }

            await LoadNodesAsync(models, plan.Roots, commandTimeout, cancellationToken).ConfigureAwait(false);
        }

        private static void LoadNodes<TModel>(IReadOnlyList<TModel> models, IEnumerable<EagerLoadNode> nodes, int? commandTimeout)
        {
            foreach (EagerLoadNode node in nodes)
            {
                switch (node.Definition.Kind)
                {
                    case RelationshipKind.HasMany:
                        InvokeSync(LoadHasManyMethod, typeof(TModel), node.Definition.RelatedType, models, node, commandTimeout);
                        break;

                    case RelationshipKind.HasOne:
                    case RelationshipKind.BelongsTo:
                        InvokeSync(LoadHasOneMethod, typeof(TModel), node.Definition.RelatedType, models, node, commandTimeout);
                        break;

                    default:
                        throw new RelationshipException($"Relationship '{node.Name}' on model '{typeof(TModel).Name}' uses an unsupported relationship type.");
                }

                MarkLoaded(models, node.Name);
            }
        }


        private static async Task LoadNodesAsync<TModel>(IReadOnlyList<TModel> models, IEnumerable<EagerLoadNode> nodes, int? commandTimeout, CancellationToken cancellationToken)
        {
            foreach (EagerLoadNode node in nodes)
            {
                cancellationToken.ThrowIfCancellationRequested();

                switch (node.Definition.Kind)
                {
                    case RelationshipKind.HasMany:
                        await InvokeAsync(LoadHasManyAsyncMethod, typeof(TModel), node.Definition.RelatedType, models, node, commandTimeout, cancellationToken).ConfigureAwait(false);
                        break;

                    case RelationshipKind.HasOne:
                    case RelationshipKind.BelongsTo:
                        await InvokeAsync(LoadHasOneAsyncMethod, typeof(TModel), node.Definition.RelatedType, models, node, commandTimeout, cancellationToken).ConfigureAwait(false);
                        break;

                    default:
                        throw new RelationshipException($"Relationship '{node.Name}' on model '{typeof(TModel).Name}' uses an unsupported relationship type.");
                }

                MarkLoaded(models, node.Name);
            }
        }

        private static void LoadHasManyCore<TParent, TRelated>(IReadOnlyList<TParent> parents, EagerLoadNode node, int? commandTimeout)
        {
            RelationshipDefinition definition = node.Definition;

            EnsureSameConnection<TParent, TRelated>(definition);

            List<object> parentKeys = GetParentKeys(parents, definition.LocalKeyProperty);

            if (parentKeys.Count == 0)
            {
                QueryBuilder<TRelated> emptyBuilder = new QueryBuilder<TRelated>().SimpleQuery();
                EagerLoadBuilder<TRelated> emptyConstraint = ApplyConstraints(emptyBuilder, node);

                AssignHasMany(parents, Array.Empty<TRelated>(), definition, node.Chaperone || emptyConstraint.ChaperoneRequested);

                return;
            }

            QueryBuilder<TRelated> builder = CreateEagerQueryBuilder<TRelated>(definition);
            EagerLoadBuilder<TRelated> constraint = ApplyConstraints(builder, node);
            int chunkSize = GetEagerChunkSize(builder, node);

            if (commandTimeout.HasValue)
            {
                builder.Timeout(commandTimeout.Value);
            }

            List<TRelated> relatedItems = ExecuteHasManyQuery(builder, constraint, definition, parentKeys, chunkSize);
            bool chaperone = node.Chaperone || constraint.ChaperoneRequested;

            AssignHasMany(parents, relatedItems, definition, chaperone);

            if (relatedItems.Count > 0 && node.Children.Count > 0)
            {
                LoadNodes(relatedItems, node.Children.Values, commandTimeout);
            }
        }

        private static async Task LoadHasManyCoreAsync<TParent, TRelated>(IReadOnlyList<TParent> parents, EagerLoadNode node, int? commandTimeout, CancellationToken cancellationToken)
        {
            RelationshipDefinition definition = node.Definition;

            EnsureSameConnection<TParent, TRelated>(definition);

            List<object> parentKeys = GetParentKeys(parents, definition.LocalKeyProperty);

            if (parentKeys.Count == 0)
            {
                QueryBuilder<TRelated> emptyBuilder = new QueryBuilder<TRelated>().SimpleQuery();
                EagerLoadBuilder<TRelated> emptyConstraint = ApplyConstraints(emptyBuilder, node);

                AssignHasMany(parents, Array.Empty<TRelated>(), definition, node.Chaperone || emptyConstraint.ChaperoneRequested);

                return;
            }

            QueryBuilder<TRelated> builder = CreateEagerQueryBuilder<TRelated>(definition);
            EagerLoadBuilder<TRelated> constraint = ApplyConstraints(builder, node);
            int chunkSize = GetEagerChunkSize(builder, node);

            if (commandTimeout.HasValue)
            {
                builder.Timeout(commandTimeout.Value);
            }

            List<TRelated> relatedItems = await ExecuteHasManyQueryAsync(builder, constraint, definition, parentKeys, chunkSize, cancellationToken).ConfigureAwait(false);
            bool chaperone = node.Chaperone || constraint.ChaperoneRequested;

            AssignHasMany(parents, relatedItems, definition, chaperone);

            if (relatedItems.Count > 0 && node.Children.Count > 0)
            {
                await LoadNodesAsync(relatedItems, node.Children.Values, commandTimeout, cancellationToken).ConfigureAwait(false);
            }
        }

        private static void LoadHasOneCore<TParent, TRelated>(IReadOnlyList<TParent> parents, EagerLoadNode node, int? commandTimeout)
        {
            RelationshipDefinition definition = node.Definition;

            EnsureSameConnection<TParent, TRelated>(definition);

            List<object> parentKeys = GetParentKeys(parents, definition.LocalKeyProperty);

            if (parentKeys.Count == 0)
            {
                QueryBuilder<TRelated> emptyBuilder = new QueryBuilder<TRelated>().SimpleQuery();
                EagerLoadBuilder<TRelated> emptyConstraint = ApplyConstraints(emptyBuilder, node);

                ValidateSinglePagination(emptyConstraint, node);
                AssignHasOne(parents, Array.Empty<TRelated>(), definition);

                return;
            }

            QueryBuilder<TRelated> builder = CreateEagerQueryBuilder<TRelated>(definition);
            EagerLoadBuilder<TRelated> constraint = ApplyConstraints(builder, node);

            ValidateSinglePagination(constraint, node);
            constraint.ApplyOrdering();

            int chunkSize = GetEagerChunkSize(builder, node);

            if (commandTimeout.HasValue)
            {
                builder.Timeout(commandTimeout.Value);
            }

            List<TRelated> relatedItems = ExecuteEagerChunks(builder, parentKeys, chunkSize);
            List<TRelated> assignedItems = AssignHasOne(parents, relatedItems, definition);

            if (assignedItems.Count > 0 && node.Children.Count > 0)
            {
                LoadNodes(assignedItems, node.Children.Values, commandTimeout);
            }
        }

        private static async Task LoadHasOneCoreAsync<TParent, TRelated>(IReadOnlyList<TParent> parents, EagerLoadNode node, int? commandTimeout, CancellationToken cancellationToken)
        {
            RelationshipDefinition definition = node.Definition;

            EnsureSameConnection<TParent, TRelated>(definition);

            List<object> parentKeys = GetParentKeys(parents, definition.LocalKeyProperty);

            if (parentKeys.Count == 0)
            {
                QueryBuilder<TRelated> emptyBuilder = new QueryBuilder<TRelated>().SimpleQuery();
                EagerLoadBuilder<TRelated> emptyConstraint = ApplyConstraints(emptyBuilder, node);

                ValidateSinglePagination(emptyConstraint, node);
                AssignHasOne(parents, Array.Empty<TRelated>(), definition);

                return;
            }

            QueryBuilder<TRelated> builder = CreateEagerQueryBuilder<TRelated>(definition);
            EagerLoadBuilder<TRelated> constraint = ApplyConstraints(builder, node);

            ValidateSinglePagination(constraint, node);
            constraint.ApplyOrdering();

            int chunkSize = GetEagerChunkSize(builder, node);

            if (commandTimeout.HasValue)
            {
                builder.Timeout(commandTimeout.Value);
            }

            List<TRelated> relatedItems = await ExecuteEagerChunksAsync(builder, parentKeys, chunkSize, cancellationToken).ConfigureAwait(false);
            List<TRelated> assignedItems = AssignHasOne(parents, relatedItems, definition);

            if (assignedItems.Count > 0 && node.Children.Count > 0)
            {
                await LoadNodesAsync(assignedItems, node.Children.Values, commandTimeout, cancellationToken).ConfigureAwait(false);
            }
        }

        private static List<object> GetParentKeys<TParent>(IReadOnlyList<TParent> parents, PropertyInfo localKeyProperty)
        {
            var keys = new List<object>();
            var uniqueKeys = new HashSet<object>();

            foreach (TParent parent in parents)
            {
                object? value = localKeyProperty.GetValue(parent);

                if (IsNullOrDefault(value))
                {
                    continue;
                }

                if (uniqueKeys.Add(value!))
                {
                    keys.Add(value!);
                }
            }

            return keys;
        }

        private static void AssignHasMany<TParent, TRelated>(IReadOnlyList<TParent> parents, IEnumerable<TRelated> relatedItems, RelationshipDefinition definition, bool chaperone)
        {
            var grouped = new Dictionary<object, List<TRelated>>();

            foreach (TRelated item in relatedItems)
            {
                object? key = definition.RelatedKeyProperty.GetValue(item);

                if (key == null)
                {
                    continue;
                }

                if (!grouped.TryGetValue(key, out List<TRelated>? list))
                {
                    list = new List<TRelated>();
                    grouped.Add(key, list);
                }

                list.Add(item);
            }

            RelationshipDefinition? inverse = chaperone ? ResolveChaperoneInverse(definition) : null;

            foreach (TParent parent in parents)
            {
                object? key = definition.LocalKeyProperty.GetValue(parent);
                List<TRelated> items = key != null && grouped.TryGetValue(key, out List<TRelated>? matches) ? matches : new List<TRelated>();

                definition.NavigationProperty.SetValue(parent, CreateCollectionValue(definition.NavigationProperty, items));

                if (inverse != null)
                {
                    foreach (TRelated item in items)
                    {
                        inverse.NavigationProperty.SetValue(item, parent);
                        MarkLoaded(item, inverse.Name);
                    }
                }
            }
        }

        private static List<TRelated> AssignHasOne<TParent, TRelated>(IReadOnlyList<TParent> parents, IEnumerable<TRelated> relatedItems, RelationshipDefinition definition)
        {
            var relatedByKey = new Dictionary<object, TRelated>();
            var assignedItems = new List<TRelated>();
            var assignedReferences = new HashSet<object>(ReferenceEqualityComparer.Instance);

            foreach (TRelated item in relatedItems)
            {
                object? key = definition.RelatedKeyProperty.GetValue(item);

                if (key != null && !relatedByKey.ContainsKey(key))
                {
                    relatedByKey.Add(key, item);
                }
            }

            foreach (TParent parent in parents)
            {
                object? key = definition.LocalKeyProperty.GetValue(parent);
                object? related = null;

                if (key != null && relatedByKey.TryGetValue(key, out TRelated? match))
                {
                    related = match;
                }

                definition.NavigationProperty.SetValue(parent, related);

                if (related != null && assignedReferences.Add(related))
                {
                    assignedItems.Add((TRelated)related);
                }
            }

            return assignedItems;
        }

        private static RelationshipDefinition ResolveChaperoneInverse(RelationshipDefinition definition)
        {
            if (definition.Kind != RelationshipKind.HasMany)
            {
                throw new RelationshipException($"Chaperone can only be used with HasMany relationships. Relationship '{definition.Name}' is '{definition.Kind}'.");
            }

            RelationshipMetadata relatedMetadata = RelationshipMetadataCache.Get(definition.RelatedType);

            if (!string.IsNullOrWhiteSpace(definition.Inverse))
            {
                RelationshipDefinition inverse = relatedMetadata.GetRequired(definition.Inverse!);

                if (!IsValidInverse(definition, inverse))
                {
                    throw new RelationshipException($"Relationship '{definition.Inverse}' on model '{definition.RelatedType.Name}' cannot be used as the inverse of '{definition.Name}' on model '{definition.ParentType.Name}'. The inverse must be a BelongsTo relationship that maps '{definition.RelatedKey}' back to '{definition.LocalKey}'.");
                }

                return inverse;
            }

            List<RelationshipDefinition> candidates = relatedMetadata.Relationships.Values.Where(inverse => IsValidInverse(definition, inverse)).ToList();

            if (candidates.Count == 0)
            {
                throw new RelationshipException($"Relationship '{definition.Name}' on model '{definition.ParentType.Name}' has Chaperone enabled, but no matching BelongsTo relationship was found on model '{definition.RelatedType.Name}'. Define the inverse BelongsTo relationship or specify Inverse explicitly.");
            }

            if (candidates.Count > 1)
            {
                throw new RelationshipException($"Relationship '{definition.Name}' on model '{definition.ParentType.Name}' has Chaperone enabled, but multiple inverse BelongsTo relationships were found on model '{definition.RelatedType.Name}': {string.Join(", ", candidates.Select(x => x.Name))}. Specify Inverse explicitly on [HasMany].");
            }

            return candidates[0];
        }

        private static bool IsValidInverse(RelationshipDefinition relationship, RelationshipDefinition inverse)
        {
            return inverse.Kind == RelationshipKind.BelongsTo
                && inverse.ParentType == relationship.RelatedType
                && inverse.RelatedType == relationship.ParentType
                && string.Equals(inverse.LocalKey, relationship.RelatedKey, StringComparison.Ordinal)
                && string.Equals(inverse.RelatedKey, relationship.LocalKey, StringComparison.Ordinal);
        }

        private static object CreateCollectionValue<TRelated>(PropertyInfo navigationProperty, List<TRelated> items)
        {
            Type propertyType = navigationProperty.PropertyType;

            if (propertyType.IsArray)
            {
                return items.ToArray();
            }

            if (propertyType.IsAssignableFrom(typeof(List<TRelated>)))
            {
                return items;
            }

            if (propertyType.IsAssignableFrom(typeof(HashSet<TRelated>)))
            {
                return new HashSet<TRelated>(items);
            }

            if (!propertyType.IsInterface && !propertyType.IsAbstract)
            {
                ConstructorInfo? enumerableConstructor = propertyType.GetConstructor(new[] { typeof(IEnumerable<TRelated>) });

                if (enumerableConstructor != null)
                {
                    return enumerableConstructor.Invoke(new object[] { items });
                }

                object? instance = Activator.CreateInstance(propertyType);

                if (instance is ICollection<TRelated> collection)
                {
                    foreach (TRelated item in items)
                    {
                        collection.Add(item);
                    }

                    return collection;
                }
            }

            throw new RelationshipException($"Relationship '{navigationProperty.Name}' cannot receive eager-loaded data because collection type '{propertyType.Name}' is not supported. Use List<{typeof(TRelated).Name}>, ICollection<{typeof(TRelated).Name}>, IEnumerable<{typeof(TRelated).Name}>, an array or another assignable collection type.");
        }



        private static EagerLoadBuilder<TRelated> ApplyConstraints<TRelated>(QueryBuilder<TRelated> builder, EagerLoadNode node)
        {
            var eagerBuilder = new EagerLoadBuilder<TRelated>(builder, node);

            foreach (Delegate constraint in node.Constraints)
            {
                if (constraint is not Action<EagerLoadBuilder<TRelated>> typedConstraint)
                {
                    throw new RelationshipException($"Invalid eager-loading constraint registered for relationship '{node.Name}'. Expected EagerLoadBuilder<{typeof(TRelated).Name}>.");
                }

                typedConstraint(eagerBuilder);
            }

            return eagerBuilder;
        }

        private static QueryBuilder<TRelated> CreateEagerQueryBuilder<TRelated>(RelationshipDefinition definition)
        {
            QueryBuilder<TRelated> builder = new QueryBuilder<TRelated>().SimpleQuery();

            builder.WhereRaw($"{definition.RelatedKey} IN @{EagerKeysParameterName}");

            return builder;
        }

        private static List<TRelated> ExecuteHasManyQuery<TRelated>(QueryBuilder<TRelated> builder, EagerLoadBuilder<TRelated> constraint, RelationshipDefinition definition, List<object> parentKeys, int chunkSize)
        {
            if (!constraint.HasPagination)
            {
                constraint.ApplyOrdering();
            }
            else
            {
                PreparePartitionedQuery(builder, constraint, definition);
            }

            return ExecuteEagerChunks(builder, parentKeys, chunkSize);
        }

        private static async Task<List<TRelated>> ExecuteHasManyQueryAsync<TRelated>(QueryBuilder<TRelated> builder, EagerLoadBuilder<TRelated> constraint, RelationshipDefinition definition, List<object> parentKeys, int chunkSize, CancellationToken cancellationToken)
        {
            if (!constraint.HasPagination)
            {
                constraint.ApplyOrdering();
            }
            else
            {
                PreparePartitionedQuery(builder, constraint, definition);
            }

            return await ExecuteEagerChunksAsync(builder, parentKeys, chunkSize, cancellationToken).ConfigureAwait(false);
        }

        private static List<TRelated> ExecuteEagerChunks<TRelated>(QueryBuilder<TRelated> builder, List<object> parentKeys, int chunkSize)
        {
            string sql = builder.ToParameterizedSql();
            var result = new List<TRelated>();

            for (int offset = 0; offset < parentKeys.Count; offset += chunkSize)
            {
                int count = Math.Min(chunkSize, parentKeys.Count - offset);
                List<object> chunk = parentKeys.GetRange(offset, count);

                List<TRelated> chunkResult = builder.QueryList<TRelated>(
                    sql,
                    new
                    {
                        __dglib_eager_keys = chunk
                    }
                );

                result.AddRange(chunkResult);
            }

            return result;
        }

        private static async Task<List<TRelated>> ExecuteEagerChunksAsync<TRelated>(QueryBuilder<TRelated> builder, List<object> parentKeys, int chunkSize, CancellationToken cancellationToken)
        {
            string sql = builder.ToParameterizedSql();
            var result = new List<TRelated>();

            for (int offset = 0; offset < parentKeys.Count; offset += chunkSize)
            {
                cancellationToken.ThrowIfCancellationRequested();

                int count = Math.Min(chunkSize, parentKeys.Count - offset);
                List<object> chunk = parentKeys.GetRange(offset, count);

                List<TRelated> chunkResult = await builder.QueryListAsync<TRelated>(
                    sql,
                    new
                    {
                        __dglib_eager_keys = chunk
                    },
                    cancellationToken
                ).ConfigureAwait(false);

                result.AddRange(chunkResult);
            }

            return result;
        }

        private static int GetEagerChunkSize<TRelated>(QueryBuilder<TRelated> builder, EagerLoadNode node)
        {
            int constraintParameters = GetEffectiveParameterCount(builder);
            int available = SqlServerParameterBudget - constraintParameters;

            if (available <= 0)
            {
                throw new RelationshipException($"Eager-loading relationship '{node.Name}' uses {constraintParameters} SQL parameters in its constraints. DapperGlib reserves a maximum budget of {SqlServerParameterBudget} parameters per SQL Server command, leaving no room for relationship keys. Reduce the number of constraint parameters.");
            }

            return available;
        }

        private static int GetEffectiveParameterCount<TRelated>(QueryBuilder<TRelated> builder)
        {
            int count = 0;

            foreach (object? value in builder.ParameterContext.Values.Values)
            {
                count += CountExpandedParameterValue(value);
            }

            return count;
        }

        private static int CountExpandedParameterValue(object? value)
        {
            if (value == null || value is string || value is byte[])
            {
                return 1;
            }

            if (value is IEnumerable enumerable)
            {
                int count = 0;

                foreach (object? item in enumerable)
                {
                    count++;
                }

                return count;
            }

            return 1;
        }

        private static void ValidateSinglePagination<TRelated>(EagerLoadBuilder<TRelated> constraint, EagerLoadNode node)
        {
            if (constraint.HasPagination)
            {
                throw new RelationshipException($"Take and Skip inside eager-loading constraints are supported only for HasMany relationships. Relationship '{node.Name}' is '{node.Definition.Kind}'.");
            }
        }

        private static void PreparePartitionedQuery<TRelated>(QueryBuilder<TRelated> builder, EagerLoadBuilder<TRelated> constraint, RelationshipDefinition definition)
        {
            string baseSql = builder.ToParameterizedSql().Trim().TrimEnd(';');
            string relatedKey = QuoteIdentifier(definition.RelatedKey);
            string orderBy = constraint.BuildWindowOrderBy("__dglib_source");

            int skip = constraint.SkipRows ?? 0;
            int? take = constraint.TakeRows;

            string rowCondition;

            if (take.HasValue)
            {
                long upperBound = (long)skip + take.Value;
                rowCondition = skip > 0
                    ? $"__dglib_ranked.__dglib_row_number > {skip} AND __dglib_ranked.__dglib_row_number <= {upperBound}"
                    : $"__dglib_ranked.__dglib_row_number <= {upperBound}";
            }
            else
            {
                rowCondition = $"__dglib_ranked.__dglib_row_number > {skip}";
            }

            string sql = $@"
                SELECT *
                FROM
                (
                    SELECT
                        __dglib_source.*,
                        ROW_NUMBER() OVER
                        (
                            PARTITION BY __dglib_source.{relatedKey}
                            ORDER BY {orderBy}
                        ) AS __dglib_row_number
                    FROM
                    (
                        {baseSql}
                    ) AS __dglib_source
                ) AS __dglib_ranked
                WHERE {rowCondition}
                ORDER BY __dglib_ranked.{relatedKey}, __dglib_ranked.__dglib_row_number";

            builder.Query = new StringBuilder(sql);
            builder.SubQueries.Clear();
            builder.RelationshipProjections.Clear();
            builder.OrderList.Clear();
            builder.SelectList = Array.Empty<string>();
            builder.SkipString = null;
            builder.TakeString = null;
        }

        private static string QuoteIdentifier(string identifier)
        {
            return $"[{identifier.Replace("]", "]]")}]";
        }


        private static void EnsureSameConnection<TParent, TRelated>(RelationshipDefinition definition)
        {
            string parentConnection = QueryBuilder<TParent>.GetConnectionString();
            string relatedConnection = QueryBuilder<TRelated>.GetConnectionString();

            if (!string.Equals(parentConnection, relatedConnection, StringComparison.Ordinal))
            {
                throw new RelationshipException($"Relationship '{definition.Name}' between '{typeof(TParent).Name}' and '{typeof(TRelated).Name}' cannot be eager loaded because they use different connection keys. Parent connection: '{parentConnection}'. Related connection: '{relatedConnection}'. Cross-connection relationships are not supported.");
            }
        }

        private static bool IsNullOrDefault(object? value)
        {
            if (value == null)
            {
                return true;
            }

            Type type = value.GetType();

            if (!type.IsValueType)
            {
                return false;
            }

            return Equals(value, Activator.CreateInstance(type));
        }

        private static void MarkLoaded<TModel>(IReadOnlyList<TModel> models, string relationshipName)
        {
            foreach (TModel model in models)
            {
                MarkLoaded(model, relationshipName);
            }
        }

        private static void MarkLoaded(object? model, string relationshipName)
        {
            if (model is IRelationshipLoadState state)
            {
                state.MarkRelationLoadedInternal(relationshipName);
            }
        }

        private static void InvokeSync(MethodInfo method, Type parentType, Type relatedType, params object?[] arguments)
        {
            try
            {
                method.MakeGenericMethod(parentType, relatedType).Invoke(null, arguments);
            }
            catch (TargetInvocationException ex) when (ex.InnerException != null)
            {
                ExceptionDispatchInfo.Capture(ex.InnerException).Throw();
                throw;
            }
        }

        private static async Task InvokeAsync(MethodInfo method, Type parentType, Type relatedType, params object?[] arguments)
        {
            try
            {
                object? result = method.MakeGenericMethod(parentType, relatedType).Invoke(null, arguments);

                if (result is not Task task)
                {
                    RelationshipDefinition? definition = arguments.Length > 1 ? arguments[1] as RelationshipDefinition : null;
                    throw new RelationshipException($"Unable to execute asynchronous eager loading{(definition == null ? "." : $" for relationship '{definition.Name}'.")}");
                }

                await task.ConfigureAwait(false);
            }
            catch (TargetInvocationException ex) when (ex.InnerException != null)
            {
                ExceptionDispatchInfo.Capture(ex.InnerException).Throw();
                throw;
            }
        }
    }
}