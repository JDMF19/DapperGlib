using DapperGlib.Exceptions;
using DapperGlib.Interfaces;
using System.Reflection;

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
            TRelationship? item;

            try
            {
                item = Activator.CreateInstance<TRelationship>();
            }
            catch (Exception ex)
            {
                throw new RelationshipException(
                    $"Unable to create an instance of '{typeof(TRelationship).Name}'. " +
                    $"Make sure the model has a parameterless constructor.",
                    ex
                );
            }

            if (item == null)
            {
                throw new RelationshipException(
                    $"Unable to create an instance of '{typeof(TRelationship).Name}'."
                );
            }

            return Create(item);
        }

        public TRelationship Create(TRelationship item)
        {
            if (item == null)
            {
                throw new ArgumentNullException(nameof(item));
            }

            if (LocalValue == null)
            {
                throw new RelationshipException(
                    $"Cannot create related model '{typeof(TRelationship).Name}'. " +
                    $"The local key '{LocalKey}' has no value. " +
                    $"Make sure the parent model has been saved first."
                );
            }

            PropertyInfo? foreignProperty =
                typeof(TRelationship).GetProperty(ForeignKey);

            if (foreignProperty == null)
            {
                throw new RelationshipException(
                    $"Cannot create related model '{typeof(TRelationship).Name}'. " +
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
                convertedValue = ConvertValue(
                    LocalValue,
                    foreignProperty.PropertyType
                );
            }
            catch (Exception ex)
            {
                throw new RelationshipException(
                    $"Unable to assign local key '{LocalKey}' with value " +
                    $"'{LocalValue}' to foreign key '{ForeignKey}' on model " +
                    $"'{typeof(TRelationship).Name}'.",
                    ex
                );
            }

            foreignProperty.SetValue(item, convertedValue);

            if (item is not IModel model)
            {
                throw new RelationshipException(
                    $"Unable to create related model '{typeof(TRelationship).Name}'. " +
                    $"The model must implement IModel. " +
                    $"Classes inheriting from Model<T> should implement this automatically. " +
                    $"Verify that Model<T> implements IModel and that the latest DapperGlib version is installed."
                );
            }

            model.Insert();

            return item;
        }

        public List<TRelationship> CreateMany(IEnumerable<TRelationship> items)
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

            if (itemsList.Count == 0)
            {
                return new List<TRelationship>();
            }

            if (itemsList.Any(x => x == null))
            {
                throw new RelationshipException(
                    $"CreateMany<{typeof(TRelationship).Name}> " +
                    $"cannot contain null items."
                );
            }

            var result =
                new List<TRelationship>();

            foreach (var item in itemsList)
            {
                result.Add(
                    Create(item)
                );
            }

            return result;
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

        private void EnsureRelationshipIsBound()
        {
            if (LocalValue == null)
            {
                throw new InvalidOperationException(
                    $"Cannot execute this operation on relationship " +
                    $"'{typeof(TRelationship).Name}'. " +
                    $"The local key '{LocalKey}' has no value. " +
                    $"Make sure the parent model has been saved first."
                );
            }

            Type localValueType = LocalValue.GetType();

            if (localValueType.IsValueType)
            {
                object? defaultValue =
                    Activator.CreateInstance(localValueType);

                if (Equals(LocalValue, defaultValue))
                {
                    throw new InvalidOperationException(
                        $"Cannot execute this operation on relationship " +
                        $"'{typeof(TRelationship).Name}'. " +
                        $"The local key '{LocalKey}' contains its default value " +
                        $"'{LocalValue}'. Make sure the parent model has been saved first."
                    );
                }
            }
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