namespace DapperGlib.Exceptions
{
    public class QueryBuilderException
        : InvalidOperationException
    {
        public QueryBuilderException(
            string message)
            : base(message)
        {
        }

        public QueryBuilderException(
            string message,
            Exception innerException)
            : base(
                message,
                innerException
            )
        {
        }
    }
}