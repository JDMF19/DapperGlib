using DapperGlib.Relationships;
using System;

namespace DapperGlib
{
    [AttributeUsage(
        AttributeTargets.Property,
        AllowMultiple = false,
        Inherited = true
    )]
    public abstract class RelationshipAttribute : Attribute
    {
        public string LocalKey { get; }

        public string RelatedKey { get; }

        internal abstract RelationshipKind Kind { get; }


        protected RelationshipAttribute(string localKey, string? relatedKey = null)
        {
            if (string.IsNullOrWhiteSpace(localKey))
            {
                throw new ArgumentException(
                    "The local key of a relationship cannot be null or empty.",
                    nameof(localKey)
                );
            }

            if (relatedKey != null && string.IsNullOrWhiteSpace(relatedKey))
            {
                throw new ArgumentException(
                    "The related key of a relationship cannot be empty.",
                    nameof(relatedKey)
                );
            }

            LocalKey = localKey.Trim();

            RelatedKey = relatedKey?.Trim() ?? LocalKey;
        }
    }
}