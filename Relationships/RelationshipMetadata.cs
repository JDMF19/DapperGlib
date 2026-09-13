using DapperGlib.Exceptions;
using System;
using System.Collections.Generic;

namespace DapperGlib.Relationships
{
    internal sealed class RelationshipMetadata
    {
        private readonly IReadOnlyDictionary<string, RelationshipDefinition> _relationships;


        internal Type ModelType
        {
            get;
        }


        internal IReadOnlyDictionary<string, RelationshipDefinition> Relationships => _relationships;


        internal RelationshipMetadata(
            Type modelType,
            IReadOnlyDictionary<
                string,
                RelationshipDefinition
            > relationships)
        {
            ModelType =
                modelType;

            _relationships =
                relationships;
        }


        internal bool TryGet(string relationshipName, out RelationshipDefinition? relationship)
        {
            if (string.IsNullOrWhiteSpace(
                relationshipName
            ))
            {
                relationship =
                    null;

                return false;
            }

            return _relationships
                .TryGetValue(
                    relationshipName.Trim(),
                    out relationship
                );
        }


        internal RelationshipDefinition GetRequired(string relationshipName)
        {
            if (string.IsNullOrWhiteSpace(
                relationshipName
            ))
            {
                throw new RelationshipException(
                    $"Relationship name cannot be null or empty " +
                    $"for model '{ModelType.Name}'."
                );
            }

            string name =
                relationshipName.Trim();


            if (!_relationships.TryGetValue(
                name,
                out RelationshipDefinition? relationship
            ))
            {
                throw new RelationshipException(
                    $"Relationship '{name}' was not found " +
                    $"on model '{ModelType.Name}'."
                );
            }


            return relationship;
        }
    }
}