namespace DapperGlib.Exceptions
{
    public class ModelNotFoundException
        : InvalidOperationException
    {
        public Type ModelType { get; }

        public object? Key { get; }

        public ModelNotFoundException(
            Type modelType,
            object? key)
            : base(
                $"Model '{modelType.Name}' " +
                $"with key '{key}' was not found."
            )
        {
            ModelType = modelType;
            Key = key;
        }

        public ModelNotFoundException(
            Type modelType,
            object? key,
            Exception innerException)
            : base(
                $"Model '{modelType.Name}' " +
                $"with key '{key}' was not found.",
                innerException
            )
        {
            ModelType = modelType;
            Key = key;
        }
    }
}