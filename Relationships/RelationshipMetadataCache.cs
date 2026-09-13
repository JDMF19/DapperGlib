using DapperGlib.Exceptions;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Threading;

namespace DapperGlib.Relationships
{
    internal static class RelationshipMetadataCache
    {
        private static readonly
            ConcurrentDictionary<
                Type,
                Lazy<RelationshipMetadata>
            > Cache =
                new();


        internal static RelationshipMetadata Get<TModel>()
            where TModel : Model<TModel>, new()
        {
            return Get(
                typeof(TModel)
            );
        }


        internal static RelationshipMetadata Get(
            Type modelType)
        {
            if (modelType == null)
            {
                throw new ArgumentNullException(
                    nameof(modelType)
                );
            }


            EnsureDapperGlibModel(
                modelType
            );


            Lazy<RelationshipMetadata> metadata =
                Cache.GetOrAdd(
                    modelType,
                    static type =>
                        new Lazy<RelationshipMetadata>(
                            () => Build(type),
                            LazyThreadSafetyMode
                                .ExecutionAndPublication
                        )
                );


            return metadata.Value;
        }


        internal static RelationshipDefinition GetRequired<TModel>(
            string relationshipName)
            where TModel : Model<TModel>, new()
        {
            return Get<TModel>()
                .GetRequired(
                    relationshipName
                );
        }


        internal static RelationshipDefinition GetRequired(Type modelType, string relationshipName)
        {
            return Get(modelType).GetRequired(relationshipName);
        }


        /*
         * ============================================================
         * BUILD METADATA
         * ============================================================
         */

        private static RelationshipMetadata Build(Type modelType)
        {
            var relationships =
                new Dictionary<
                    string,
                    RelationshipDefinition
                >(
                    StringComparer.Ordinal
                );


            PropertyInfo[] properties =
                modelType.GetProperties(
                    BindingFlags.Instance |
                    BindingFlags.Public
                );


            foreach (PropertyInfo property in properties)
            {
                List<RelationshipAttribute> attributes =
                    property
                        .GetCustomAttributes(
                            inherit: true
                        )
                        .OfType<RelationshipAttribute>()
                        .ToList();


                if (attributes.Count == 0)
                {
                    continue;
                }


                if (attributes.Count > 1)
                {
                    throw new RelationshipException(
                        $"Relationship property '{property.Name}' " +
                        $"on model '{modelType.Name}' contains more " +
                        $"than one relationship attribute. " +
                        $"Use only one of [HasOne], [HasMany] " +
                        $"or [BelongsTo]."
                    );
                }


                RelationshipDefinition definition =
                    BuildDefinition(
                        modelType,
                        property,
                        attributes[0]
                    );


                if (!relationships.TryAdd(
                    definition.Name,
                    definition
                ))
                {
                    throw new RelationshipException(
                        $"Relationship '{definition.Name}' " +
                        $"is defined more than once on model " +
                        $"'{modelType.Name}'."
                    );
                }
            }


            return new RelationshipMetadata(
                modelType,
                relationships
            );
        }


        /*
         * ============================================================
         * BUILD RELATIONSHIP
         * ============================================================
         */

        private static RelationshipDefinition BuildDefinition(Type parentType, PropertyInfo navigationProperty, RelationshipAttribute attribute)
        {
            ValidateNavigationProperty(
                parentType,
                navigationProperty
            );


            Type relatedType =
                ResolveRelatedType(
                    parentType,
                    navigationProperty,
                    attribute.Kind
                );


            EnsureDapperGlibModel(
                relatedType,
                parentType,
                navigationProperty.Name
            );


            PropertyInfo localKeyProperty =
                GetRequiredKeyProperty(
                    parentType,
                    attribute.LocalKey,
                    parentType,
                    navigationProperty.Name,
                    "local"
                );


            PropertyInfo relatedKeyProperty =
                GetRequiredKeyProperty(
                    relatedType,
                    attribute.RelatedKey,
                    parentType,
                    navigationProperty.Name,
                    "related"
                );


            ValidateKeyTypes(
                parentType,
                navigationProperty.Name,
                localKeyProperty,
                relatedKeyProperty
            );



            HasManyAttribute? hasManyAttribute = attribute as HasManyAttribute;
            string? inverse = hasManyAttribute?.Inverse;

            if (inverse != null)
            {
                if (string.IsNullOrWhiteSpace(inverse))
                {
                    throw new RelationshipException($"Relationship '{navigationProperty.Name}' on model '{parentType.Name}' defines an empty inverse relationship name.");
                }

                inverse = inverse.Trim();
            }

            return new RelationshipDefinition(attribute.Kind, parentType, relatedType, attribute.LocalKey, attribute.RelatedKey, navigationProperty, localKeyProperty, relatedKeyProperty, hasManyAttribute?.Chaperone == true, inverse);

        }


        /*
         * ============================================================
         * NAVIGATION PROPERTY VALIDATION
         * ============================================================
         */

        private static void ValidateNavigationProperty(Type parentType, PropertyInfo navigationProperty)
        {
            if (navigationProperty
                .GetIndexParameters()
                .Length > 0)
            {
                throw new RelationshipException(
                    $"Relationship '{navigationProperty.Name}' " +
                    $"on model '{parentType.Name}' cannot be " +
                    $"an indexer property."
                );
            }


            if (!navigationProperty.CanRead)
            {
                throw new RelationshipException(
                    $"Relationship '{navigationProperty.Name}' " +
                    $"on model '{parentType.Name}' must have " +
                    $"a getter."
                );
            }


            if (!navigationProperty.CanWrite)
            {
                throw new RelationshipException(
                    $"Relationship '{navigationProperty.Name}' " +
                    $"on model '{parentType.Name}' must have " +
                    $"a setter so DapperGlib can assign " +
                    $"eager-loaded data."
                );
            }
        }


        /*
         * ============================================================
         * RELATED TYPE
         * ============================================================
         */

        private static Type ResolveRelatedType(Type parentType, PropertyInfo navigationProperty, RelationshipKind kind)
        {
            if (kind == RelationshipKind.HasMany)
            {
                Type? elementType =
                    GetCollectionElementType(
                        navigationProperty.PropertyType
                    );


                if (elementType == null)
                {
                    throw new RelationshipException(
                        $"Relationship '{navigationProperty.Name}' " +
                        $"on model '{parentType.Name}' is marked " +
                        $"with [HasMany] but property type " +
                        $"'{navigationProperty.PropertyType.Name}' " +
                        $"is not a supported generic collection " +
                        $"or array."
                    );
                }


                return elementType;
            }


            Type navigationType =
                Nullable.GetUnderlyingType(
                    navigationProperty.PropertyType
                )
                ?? navigationProperty.PropertyType;


            if (GetCollectionElementType(
                navigationType
            ) != null)
            {
                throw new RelationshipException(
                    $"Relationship '{navigationProperty.Name}' " +
                    $"on model '{parentType.Name}' is marked " +
                    $"with [{kind}] and must reference a single " +
                    $"related model, not a collection."
                );
            }


            return navigationType;
        }


        /*
         * ============================================================
         * COLLECTION TYPE
         * ============================================================
         */

        private static Type? GetCollectionElementType(Type type)
        {
            if (type == typeof(string))
            {
                return null;
            }


            if (type.IsArray)
            {
                return type.GetElementType();
            }


            var elementTypes =
                new List<Type>();


            if (type.IsGenericType &&
                type.GetGenericTypeDefinition()
                    == typeof(IEnumerable<>))
            {
                elementTypes.Add(
                    type.GetGenericArguments()[0]
                );
            }


            elementTypes.AddRange(
                type
                    .GetInterfaces()
                    .Where(
                        interfaceType =>
                            interfaceType.IsGenericType &&
                            interfaceType
                                .GetGenericTypeDefinition()
                                == typeof(IEnumerable<>)
                    )
                    .Select(
                        interfaceType =>
                            interfaceType
                                .GetGenericArguments()[0]
                    )
            );


            Type[] distinctTypes =
                elementTypes
                    .Distinct()
                    .ToArray();


            if (distinctTypes.Length == 0)
            {
                return null;
            }


            if (distinctTypes.Length > 1)
            {
                return null;
            }


            return distinctTypes[0];
        }


        /*
         * ============================================================
         * KEYS
         * ============================================================
         */

        private static PropertyInfo GetRequiredKeyProperty(
            Type keyOwnerType,
            string keyName,
            Type parentType,
            string relationshipName,
            string keyRole)
        {
            PropertyInfo? property =
                keyOwnerType.GetProperty(
                    keyName,
                    BindingFlags.Instance |
                    BindingFlags.Public
                );


            if (property == null)
            {
                throw new RelationshipException(
                    $"Relationship '{relationshipName}' " +
                    $"on model '{parentType.Name}' references " +
                    $"{keyRole} key '{keyName}', but property " +
                    $"'{keyName}' was not found on model " +
                    $"'{keyOwnerType.Name}'."
                );
            }


            if (!property.CanRead)
            {
                throw new RelationshipException(
                    $"Relationship '{relationshipName}' " +
                    $"on model '{parentType.Name}' references " +
                    $"{keyRole} key '{keyName}' on model " +
                    $"'{keyOwnerType.Name}', but that property " +
                    $"is not readable."
                );
            }


            return property;
        }


        private static void ValidateKeyTypes(
            Type parentType,
            string relationshipName,
            PropertyInfo localKeyProperty,
            PropertyInfo relatedKeyProperty)
        {
            Type localType =
                Nullable.GetUnderlyingType(
                    localKeyProperty.PropertyType
                )
                ?? localKeyProperty.PropertyType;


            Type relatedType =
                Nullable.GetUnderlyingType(
                    relatedKeyProperty.PropertyType
                )
                ?? relatedKeyProperty.PropertyType;


            /*
             * int e int? son compatibles.
             * Guid e Guid? son compatibles.
             *
             * Pero int y long, por ejemplo, se consideran
             * configuraciones diferentes.
             */
            if (localType == relatedType)
            {
                return;
            }


            throw new RelationshipException(
                $"Relationship '{relationshipName}' " +
                $"on model '{parentType.Name}' has incompatible " +
                $"key types. Local key " +
                $"'{localKeyProperty.Name}' uses " +
                $"'{localKeyProperty.PropertyType.Name}' while " +
                $"related key '{relatedKeyProperty.Name}' uses " +
                $"'{relatedKeyProperty.PropertyType.Name}'."
            );
        }


        /*
         * ============================================================
         * MODEL VALIDATION
         * ============================================================
         */

        private static void EnsureDapperGlibModel(
            Type modelType)
        {
            if (IsDapperGlibModel(
                modelType
            ))
            {
                return;
            }


            throw new RelationshipException(
                $"Type '{modelType.Name}' is not a valid " +
                $"DapperGlib model. Relationship models " +
                $"must inherit from Model<T>."
            );
        }


        private static void EnsureDapperGlibModel(
            Type modelType,
            Type parentType,
            string relationshipName)
        {
            if (IsDapperGlibModel(
                modelType
            ))
            {
                return;
            }


            throw new RelationshipException(
                $"Relationship '{relationshipName}' " +
                $"on model '{parentType.Name}' points to type " +
                $"'{modelType.Name}', which is not a valid " +
                $"DapperGlib model. Related models must " +
                $"inherit from Model<T>."
            );
        }


        private static bool IsDapperGlibModel(
            Type modelType)
        {
            Type? current =
                modelType;


            while (current != null &&
                   current != typeof(object))
            {
                if (current.IsGenericType &&
                    current.GetGenericTypeDefinition()
                        == typeof(Model<>))
                {
                    Type genericModelType =
                        current.GetGenericArguments()[0];


                    return genericModelType
                        == modelType;
                }


                current =
                    current.BaseType;
            }


            return false;
        }
    }
}