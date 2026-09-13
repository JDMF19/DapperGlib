using DapperGlib.Exceptions;

namespace DapperGlib.Relationships
{
    internal sealed class EagerLoadPlan
    {
        private readonly Dictionary<string, EagerLoadNode> _roots = new(StringComparer.Ordinal);

        internal IEnumerable<EagerLoadNode> Roots => _roots.Values;

        internal bool HasLoads => _roots.Count > 0;

        internal void Add(Type modelType, string relationshipPath, bool chaperone = false)
        {
            EagerLoadNode node = AddRootPath(modelType, relationshipPath);

            if (chaperone)
            {
                node.EnableChaperone();
            }
        }

        internal void Add<TRelated>(Type modelType, string relationshipPath, Action<EagerLoadBuilder<TRelated>> constraint)
        {
            if (constraint == null)
            {
                throw new ArgumentNullException(nameof(constraint));
            }

            EagerLoadNode node = AddRootPath(modelType, relationshipPath);

            if (node.Definition.RelatedType != typeof(TRelated))
            {
                throw new RelationshipException($"Relationship path '{relationshipPath}' on model '{modelType.Name}' ends in model '{node.Definition.RelatedType.Name}', but the eager-loading constraint uses '{typeof(TRelated).Name}'.");
            }

            node.AddConstraint(constraint);
        }

        internal static EagerLoadNode AddChildPath(EagerLoadNode parent, Type modelType, string relationshipPath)
        {
            if (parent == null)
            {
                throw new ArgumentNullException(nameof(parent));
            }

            if (modelType == null)
            {
                throw new ArgumentNullException(nameof(modelType));
            }

            if (parent.Definition.RelatedType != modelType)
            {
                throw new RelationshipException($"Nested eager loading expected model '{parent.Definition.RelatedType.Name}', but received '{modelType.Name}'.");
            }

            string[] segments = ParsePath(modelType, relationshipPath);
            Type currentType = modelType;
            EagerLoadNode currentNode = parent;

            foreach (string segment in segments)
            {
                RelationshipDefinition definition = RelationshipMetadataCache.GetRequired(currentType, segment);
                currentNode = currentNode.GetOrAddChild(definition);
                currentType = definition.RelatedType;
            }

            return currentNode;
        }

        internal EagerLoadPlan Clone()
        {
            var clone = new EagerLoadPlan();

            foreach (var root in _roots)
            {
                clone._roots.Add(root.Key, root.Value.Clone());
            }

            return clone;
        }

        private EagerLoadNode AddRootPath(Type modelType, string relationshipPath)
        {
            if (modelType == null)
            {
                throw new ArgumentNullException(nameof(modelType));
            }

            string[] segments = ParsePath(modelType, relationshipPath);

            Type currentType = modelType;
            EagerLoadNode? currentNode = null;

            for (int i = 0; i < segments.Length; i++)
            {
                RelationshipDefinition definition = RelationshipMetadataCache.GetRequired(currentType, segments[i]);

                if (i == 0)
                {
                    if (!_roots.TryGetValue(definition.Name, out currentNode))
                    {
                        currentNode = new EagerLoadNode(definition);
                        _roots.Add(definition.Name, currentNode);
                    }
                }
                else
                {
                    currentNode = currentNode!.GetOrAddChild(definition);
                }

                currentType = definition.RelatedType;
            }

            return currentNode!;
        }

        private static string[] ParsePath(Type modelType, string relationshipPath)
        {
            if (string.IsNullOrWhiteSpace(relationshipPath))
            {
                throw new RelationshipException($"Relationship name cannot be null or empty for model '{modelType.Name}'.");
            }

            string[] segments = relationshipPath.Split('.', StringSplitOptions.None);

            if (segments.Any(string.IsNullOrWhiteSpace))
            {
                throw new RelationshipException($"Relationship path '{relationshipPath}' on model '{modelType.Name}' contains an invalid empty relationship segment.");
            }

            return segments.Select(x => x.Trim()).ToArray();
        }
    }
}