using DapperGlib.Exceptions;

namespace DapperGlib.Relationships
{
    internal sealed class EagerLoadNode
    {
        internal RelationshipDefinition Definition { get; }

        internal string Name => Definition.Name;

        internal bool Chaperone { get; private set; }

        internal List<Delegate> Constraints { get; } = new();

        internal Dictionary<string, EagerLoadNode> Children { get; } = new(StringComparer.Ordinal);

        internal EagerLoadNode(RelationshipDefinition definition, bool chaperone = false)
        {
            Definition = definition ?? throw new ArgumentNullException(nameof(definition));
            Chaperone = definition.Chaperone || chaperone;
        }

        internal void EnableChaperone()
        {
            if (Definition.Kind != RelationshipKind.HasMany)
            {
                throw new RelationshipException($"Chaperone can only be enabled on HasMany relationships. Relationship '{Definition.Name}' is '{Definition.Kind}'.");
            }

            Chaperone = true;
        }

        internal void AddConstraint(Delegate constraint)
        {
            Constraints.Add(constraint ?? throw new ArgumentNullException(nameof(constraint)));
        }

        internal EagerLoadNode GetOrAddChild(RelationshipDefinition definition)
        {
            if (definition.ParentType != Definition.RelatedType)
            {
                throw new RelationshipException($"Relationship '{definition.Name}' belongs to model '{definition.ParentType.Name}' and cannot be nested under relationship '{Name}', whose related model is '{Definition.RelatedType.Name}'.");
            }

            if (Children.TryGetValue(definition.Name, out EagerLoadNode? existing))
            {
                return existing;
            }

            var child = new EagerLoadNode(definition);

            Children.Add(definition.Name, child);

            return child;
        }

        internal EagerLoadNode Clone()
        {
            var clone = new EagerLoadNode(Definition, Chaperone);

            foreach (Delegate constraint in Constraints)
            {
                clone.Constraints.Add(constraint);
            }

            foreach (var child in Children)
            {
                clone.Children.Add(child.Key, child.Value.Clone());
            }

            return clone;
        }
    }
}