using DapperGlib.Relationships;

namespace DapperGlib
{
    public sealed class HasManyAttribute : RelationshipAttribute
    {
        internal override RelationshipKind Kind => RelationshipKind.HasMany;

        public bool Chaperone { get; set; }

        public string? Inverse { get; set; }

        public HasManyAttribute(string localKey, string? relatedKey = null) : base(localKey, relatedKey)
        {
        }
    }
}