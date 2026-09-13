using DapperGlib.Relationships;

namespace DapperGlib
{
    public sealed class BelongsToAttribute : RelationshipAttribute
    {
        internal override RelationshipKind Kind => RelationshipKind.BelongsTo;

        public BelongsToAttribute(string localKey, string? relatedKey = null) : base(localKey, relatedKey)
        {
        }
    }
}