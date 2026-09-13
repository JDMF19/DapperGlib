using DapperGlib.Exceptions;
using DapperGlib.Relationships;
using System.Reflection;

namespace DapperGlib
{
    public sealed class RelationshipQuery<TRelated> : QueryBuilder<TRelated> where TRelated : Model<TRelated>, new()
    {
        internal RelationshipDefinition Definition { get; }

        internal object Parent { get; }

        internal object? LocalValue { get; }

        internal RelationshipQuery(object parent, RelationshipDefinition definition)
        {
            Parent = parent ?? throw new ArgumentNullException(nameof(parent));
            Definition = definition ?? throw new ArgumentNullException(nameof(definition));

            if (!definition.ParentType.IsInstanceOfType(parent))
            {
                throw new RelationshipException($"Relationship '{definition.Name}' expects parent model '{definition.ParentType.Name}', but received '{parent.GetType().Name}'.");
            }

            if (definition.RelatedType != typeof(TRelated))
            {
                throw new RelationshipException($"Relationship '{definition.Name}' on model '{definition.ParentType.Name}' points to '{definition.RelatedType.Name}', but the requested relationship query uses '{typeof(TRelated).Name}'.");
            }

            EnsureSameConnection(parent);

            LocalValue = definition.LocalKeyProperty.GetValue(parent);

            SimpleQuery();

            if (IsNullOrDefault(LocalValue))
            {
                WhereRaw("1 = 0");
                return;
            }

            Where(definition.RelatedKey, LocalValue);
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

            object? defaultValue = Activator.CreateInstance(type);

            return Equals(value, defaultValue);
        }

        private static void EnsureSameConnection(object parent)
        {
            PropertyInfo? connectionProperty = parent.GetType().GetProperty("Connection", BindingFlags.Instance | BindingFlags.Public);

            string parentConnection = connectionProperty?.GetValue(parent) as string ?? "SqlConnection";
            string relatedConnection = QueryBuilder<TRelated>.GetConnectionString();

            if (!string.Equals(parentConnection, relatedConnection, StringComparison.Ordinal))
            {
                throw new RelationshipException($"Relationship between '{parent.GetType().Name}' and '{typeof(TRelated).Name}' cannot be queried because they use different connection keys. Parent connection: '{parentConnection}'. Related connection: '{relatedConnection}'. Cross-connection relationships are not supported.");
            }
        }
    }
}