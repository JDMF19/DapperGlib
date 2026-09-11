namespace DapperGlib.Internal
{
    internal readonly struct DatabaseExecutionContextLease
    {
        internal DatabaseExecutionContext Context { get; }
        internal bool OwnsContext { get; }

        internal DatabaseExecutionContextLease(DatabaseExecutionContext context, bool ownsContext)
        {
            Context = context ?? throw new ArgumentNullException(nameof(context));
            OwnsContext = ownsContext;
        }
    }
}