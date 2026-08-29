namespace DapperGlib.Exceptions
{
    public class RelationshipException
        : InvalidOperationException
    {
        public RelationshipException(
            string message)
            : base(message)
        {
        }

        public RelationshipException(
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