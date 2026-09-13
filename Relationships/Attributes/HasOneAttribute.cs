using DapperGlib.Relationships;

namespace DapperGlib
{
    public sealed class HasOneAttribute : RelationshipAttribute
    {
        internal override RelationshipKind Kind => RelationshipKind.HasOne;

        public HasOneAttribute(string localKey, string? relatedKey = null) : base(localKey, relatedKey)
        {
        }
    }
}