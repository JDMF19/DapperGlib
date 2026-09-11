using DapperGlib.Exceptions;
using DapperGlib.Interfaces;
using System.Reflection;
using DapperGlib.Internal;
using System.Threading;

namespace DapperGlib
{
    public class Relationship<TRelationship> : QueryBuilder<TRelationship>
    {
        internal string LocalKey { get; set; }
        internal string ForeignKey { get; set; }

        internal object? LocalValue { get; private set; }

        public Relationship(string localKey, string? foreignKey = null)
        {
            if (string.IsNullOrWhiteSpace(localKey))
            {
                throw new ArgumentException(
                    "The local key of a relationship cannot be null or empty.",
                    nameof(localKey)
                );
            }

            LocalKey = localKey;
            ForeignKey = foreignKey ?? localKey;
        }

        internal void Bind(object? localValue)
        {
            LocalValue = localValue;

            Where(ForeignKey, "=", localValue);
            UnderRelationship = true;
        }

        public TRelationship Create()
        {
            TRelationship item = CreateRelationshipInstance();

            return Create(item);
        }

        public TRelationship Create(TRelationship item)
        {
            IModel model = PrepareRelatedModel(item);

            model.Insert();

            return item;
        }

        public Task<TRelationship> CreateAsync()
        {
            return CreateAsync(
                CancellationToken.None
            );
        }

        public Task<TRelationship> CreateAsync(CancellationToken cancellationToken)
        {
            TRelationship item = CreateRelationshipInstance();

            return CreateAsync(item, cancellationToken);
        }

        public Task<TRelationship> CreateAsync(TRelationship item)
        {
            return CreateAsync(
                item,
                CancellationToken.None
            );
        }

        public async Task<TRelationship> CreateAsync(TRelationship item, CancellationToken cancellationToken)
        {
            IModel model =
                PrepareRelatedModel(
                    item
                );

            await model
                .InsertAsync(
                    cancellationToken
                )
                .ConfigureAwait(false);

            return item;
        }

        public List<TRelationship> CreateMany(IEnumerable<TRelationship> items)
        {
            List<TRelationship> itemsList = PrepareRelatedItems(items, nameof(CreateMany));

            if (itemsList.Count == 0)
            {
                return new List<TRelationship>();
            }

            foreach (TRelationship item in itemsList)
            {
                PrepareRelatedModel(item);
            }

            int chunkSize = QueryBuilder<TRelationship>.GetInsertManyChunkSize();

            return _executor.ExecuteBatch(GetConnectionString(), () =>
            {
                var builder = new QueryBuilder<TRelationship>();

                for (int offset = 0; offset < itemsList.Count; offset += chunkSize)
                {
                    int count = Math.Min(chunkSize, itemsList.Count - offset);
                    List<TRelationship> chunk = itemsList.GetRange(offset, count);

                    BulkInsertCommand command = QueryBuilder<TRelationship>.BuildInsertManyCommand(chunk, true);

                    if (command.ReturnsGeneratedKeys)
                    {
                        List<BulkInsertKeyResult> generatedKeys = builder.QueryList<BulkInsertKeyResult>(command.Sql, command.Parameters);
                        AssignGeneratedPrimaryKeys(chunk, generatedKeys);
                    }
                    else
                    {
                        builder.ExecuteCommand(command.Sql, command.Parameters);
                    }
                }

                return itemsList;
            });
        }

        public Task<List<TRelationship>> CreateManyAsync(IEnumerable<TRelationship> items)
        {
            return CreateManyAsync(
                items,
                CancellationToken.None
            );
        }

        public Task<List<TRelationship>> CreateManyAsync(IEnumerable<TRelationship> items, CancellationToken cancellationToken)
        {
            List<TRelationship> itemsList = PrepareRelatedItems(items, nameof(CreateManyAsync));

            if (itemsList.Count == 0)
            {
                return Task.FromResult(new List<TRelationship>());
            }

            foreach (TRelationship item in itemsList)
            {
                PrepareRelatedModel(item);
            }

            int chunkSize = QueryBuilder<TRelationship>.GetInsertManyChunkSize();

            return _executor.ExecuteBatchAsync(GetConnectionString(), async () =>
            {
                var builder = new QueryBuilder<TRelationship>();

                for (int offset = 0; offset < itemsList.Count; offset += chunkSize)
                {
                    cancellationToken.ThrowIfCancellationRequested();

                    int count = Math.Min(chunkSize, itemsList.Count - offset);
                    List<TRelationship> chunk = itemsList.GetRange(offset, count);

                    BulkInsertCommand command = QueryBuilder<TRelationship>.BuildInsertManyCommand(chunk, true);

                    if (command.ReturnsGeneratedKeys)
                    {
                        List<BulkInsertKeyResult> generatedKeys = await builder.QueryListAsync<BulkInsertKeyResult>(command.Sql, command.Parameters, cancellationToken).ConfigureAwait(false);
                        AssignGeneratedPrimaryKeys(chunk, generatedKeys);
                    }
                    else
                    {
                        await builder.ExecuteCommandAsync(command.Sql, command.Parameters, cancellationToken).ConfigureAwait(false);
                    }
                }

                return itemsList;
            }, cancellationToken);
        }

        public new void Delete()
        {
            EnsureRelationshipIsBound();

            base.Delete();
        }

        public new Task<int> DeleteAsync()
        {
            EnsureRelationshipIsBound();

            return base.DeleteAsync();
        }

        public new Task<int> DeleteAsync(CancellationToken cancellationToken)
        {
            EnsureRelationshipIsBound();

            return base.DeleteAsync(
                cancellationToken
            );
        }

        private void EnsureRelationshipIsBound()
        {
            if (LocalValue == null)
            {
                throw new RelationshipException(
                    $"Cannot execute this operation on relationship " +
                    $"'{typeof(TRelationship).Name}'. " +
                    $"The local key '{LocalKey}' has no value. " +
                    $"Make sure the parent model has been saved first."
                );
            }

            Type localValueType =
                LocalValue.GetType();

            if (localValueType.IsValueType)
            {
                object? defaultValue =
                    Activator.CreateInstance(
                        localValueType
                    );

                if (Equals(
                    LocalValue,
                    defaultValue
                ))
                {
                    throw new RelationshipException(
                        $"Cannot execute this operation on relationship " +
                        $"'{typeof(TRelationship).Name}'. " +
                        $"The local key '{LocalKey}' contains its default value " +
                        $"'{LocalValue}'. Make sure the parent model has been saved first."
                    );
                }
            }
        }

        private static void AssignGeneratedPrimaryKeys(IReadOnlyList<TRelationship> items, IReadOnlyList<BulkInsertKeyResult> generatedKeys)
        {
            if (generatedKeys.Count != items.Count)
            {
                throw new ModelConfigurationException($"Relationship bulk create for model '{typeof(TRelationship).Name}' inserted {items.Count} records but returned {generatedKeys.Count} generated primary keys.");
            }

            string? primaryKeyName = QueryBuilder<TRelationship>.GetPrimaryKey();

            if (primaryKeyName == null)
            {
                throw new ModelConfigurationException($"Primary key is not defined for model '{typeof(TRelationship).Name}'.");
            }

            PropertyInfo? primaryKey = typeof(TRelationship).GetProperty(primaryKeyName);

            if (primaryKey == null)
            {
                throw new ModelConfigurationException($"Primary key property '{primaryKeyName}' was not found on model '{typeof(TRelationship).Name}'.");
            }

            if (!primaryKey.CanWrite)
            {
                throw new ModelConfigurationException($"Primary key property '{primaryKey.Name}' on model '{typeof(TRelationship).Name}' is read-only.");
            }

            var assignedIndexes = new bool[items.Count];

            foreach (BulkInsertKeyResult generatedKey in generatedKeys)
            {
                if (generatedKey.Index < 0 || generatedKey.Index >= items.Count)
                {
                    throw new ModelConfigurationException($"Relationship bulk create for model '{typeof(TRelationship).Name}' returned an invalid row index '{generatedKey.Index}'.");
                }

                if (assignedIndexes[generatedKey.Index])
                {
                    throw new ModelConfigurationException($"Relationship bulk create for model '{typeof(TRelationship).Name}' returned row index '{generatedKey.Index}' more than once.");
                }

                if (generatedKey.Value == null || generatedKey.Value == DBNull.Value)
                {
                    throw new ModelConfigurationException($"The database did not return a generated value for primary key '{primaryKey.Name}' on model '{typeof(TRelationship).Name}'.");
                }

                object? convertedValue;

                try
                {
                    convertedValue = ConvertValue(generatedKey.Value, primaryKey.PropertyType);
                }
                catch (Exception ex)
                {
                    throw new ModelConfigurationException($"Unable to convert generated primary key value '{generatedKey.Value}' to '{primaryKey.PropertyType.Name}' for model '{typeof(TRelationship).Name}'.", ex);
                }

                primaryKey.SetValue(items[generatedKey.Index], convertedValue);
                assignedIndexes[generatedKey.Index] = true;
            }

            if (assignedIndexes.Any(assigned => !assigned))
            {
                throw new ModelConfigurationException($"Relationship bulk create for model '{typeof(TRelationship).Name}' did not return a generated primary key for every inserted record.");
            }
        }

        private List<TRelationship> PrepareRelatedItems(IEnumerable<TRelationship> items, string methodName)
        {
            if (items == null)
            {
                throw new ArgumentNullException(
                    nameof(items)
                );
            }

            EnsureRelationshipIsBound();

            var itemsList =
                items.ToList();

            if (itemsList.Any(
                item => item == null
            ))
            {
                throw new RelationshipException(
                    $"{methodName}<{typeof(TRelationship).Name}> " +
                    $"cannot contain null items."
                );
            }

            return itemsList;
        }

        private static TRelationship CreateRelationshipInstance()
        {
            TRelationship? item;

            try
            {
                item =
                    Activator.CreateInstance<TRelationship>();
            }
            catch (Exception ex)
            {
                throw new RelationshipException(
                    $"Unable to create an instance of " +
                    $"'{typeof(TRelationship).Name}'. " +
                    $"Make sure the model has a parameterless constructor.",
                    ex
                );
            }

            if (item == null)
            {
                throw new RelationshipException(
                    $"Unable to create an instance of " +
                    $"'{typeof(TRelationship).Name}'."
                );
            }

            return item;
        }

        private IModel PrepareRelatedModel(TRelationship item)
        {
            if (item == null)
            {
                throw new ArgumentNullException(
                    nameof(item)
                );
            }

            EnsureRelationshipIsBound();

            PropertyInfo? foreignProperty =
                typeof(TRelationship)
                    .GetProperty(
                        ForeignKey
                    );

            if (foreignProperty == null)
            {
                throw new RelationshipException(
                    $"Cannot create related model " +
                    $"'{typeof(TRelationship).Name}'. " +
                    $"Foreign key property '{ForeignKey}' was not found."
                );
            }

            if (!foreignProperty.CanWrite)
            {
                throw new RelationshipException(
                    $"Foreign key property '{ForeignKey}' on model " +
                    $"'{typeof(TRelationship).Name}' is read-only."
                );
            }

            object? convertedValue;

            try
            {
                convertedValue =
                    ConvertValue(
                        LocalValue,
                        foreignProperty.PropertyType
                    );
            }
            catch (Exception ex)
            {
                throw new RelationshipException(
                    $"Unable to assign local key '{LocalKey}' " +
                    $"with value '{LocalValue}' to foreign key " +
                    $"'{ForeignKey}' on model " +
                    $"'{typeof(TRelationship).Name}'.",
                    ex
                );
            }

            foreignProperty.SetValue(
                item,
                convertedValue
            );

            if (item is not IModel model)
            {
                throw new RelationshipException(
                    $"Unable to create related model " +
                    $"'{typeof(TRelationship).Name}'. " +
                    $"The model must implement IModel. " +
                    $"Classes inheriting from Model<T> should implement " +
                    $"this automatically. Verify that Model<T> implements " +
                    $"IModel and that the latest DapperGlib version is installed."
                );
            }

            return model;
        }

        private static object? ConvertValue(object? value, Type destinationType)
        {
            if (value == null)
                return null;

            Type targetType =
                Nullable.GetUnderlyingType(destinationType)
                ?? destinationType;

            if (targetType.IsInstanceOfType(value))
                return value;

            if (targetType == typeof(Guid))
                return Guid.Parse(value.ToString()!);

            if (targetType.IsEnum)
                return Enum.ToObject(targetType, value);

            return Convert.ChangeType(value, targetType);
        }
    }
}