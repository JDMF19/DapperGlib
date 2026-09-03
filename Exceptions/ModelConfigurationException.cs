namespace DapperGlib.Exceptions
{
    public class ModelConfigurationException
        : ApplicationException
    {
        public ModelConfigurationException(
            string message)
            : base(message)
        {
        }

        public ModelConfigurationException(string message, Exception innerException)
            : base(
                message,
                innerException
            )
        {
        }
    }
}