using System;
using System.Reflection;

namespace DapperGlib.Relationships
{
    internal sealed class RelationshipDefinition
    {
        internal string Name => NavigationProperty.Name;

        internal RelationshipKind Kind { get; }

        internal Type ParentType { get; }

        internal Type RelatedType { get; }

        internal string LocalKey { get; }

        internal string RelatedKey { get; }

        internal PropertyInfo NavigationProperty { get; }

        internal PropertyInfo LocalKeyProperty { get; }

        internal PropertyInfo RelatedKeyProperty { get; }

        internal bool Chaperone { get; }

        internal string? Inverse { get; }

        internal bool IsCollection => Kind == RelationshipKind.HasMany;

        internal RelationshipDefinition(RelationshipKind kind, Type parentType, Type relatedType, string localKey, string relatedKey, PropertyInfo navigationProperty, PropertyInfo localKeyProperty, PropertyInfo relatedKeyProperty, bool chaperone = false, string? inverse = null)
        {
            Kind = kind;
            ParentType = parentType;
            RelatedType = relatedType;
            LocalKey = localKey;
            RelatedKey = relatedKey;
            NavigationProperty = navigationProperty;
            LocalKeyProperty = localKeyProperty;
            RelatedKeyProperty = relatedKeyProperty;
            Chaperone = chaperone;
            Inverse = inverse;
        }
    }
}