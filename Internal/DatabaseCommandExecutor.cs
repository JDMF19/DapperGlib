namespace DapperGlib.Internal
{
    internal sealed class DatabaseCommandExecutor
    {
        private readonly GlipContext _context;

        internal DatabaseCommandExecutor(GlipContext context)
        {
            _context = context ?? throw new ArgumentNullException(nameof(context));
        }

        internal TResult Execute<TResult>(string connectionKey, Func<DatabaseExecutionContext, TResult> executor, DatabaseExecutionContext? executionContext = null)
        {
            ValidateArguments(connectionKey, executor);

            DatabaseExecutionContextLease lease = AcquireExecutionContext(connectionKey, executionContext);

            try
            {
                lease.Context.EnsureOpen();

                return executor(lease.Context);
            }
            finally
            {
                if (lease.OwnsContext)
                {
                    lease.Context.Dispose();
                }
            }
        }

        internal async Task<TResult> ExecuteAsync<TResult>(string connectionKey, Func<DatabaseExecutionContext, Task<TResult>> executor, CancellationToken cancellationToken = default, DatabaseExecutionContext? executionContext = null)
        {
            ValidateArguments(connectionKey, executor);

            DatabaseExecutionContextLease lease = AcquireExecutionContext(connectionKey, executionContext);

            try
            {
                await lease.Context.EnsureOpenAsync(cancellationToken).ConfigureAwait(false);

                return await executor(lease.Context).ConfigureAwait(false);
            }
            finally
            {
                if (lease.OwnsContext)
                {
                    await lease.Context.DisposeAsync().ConfigureAwait(false);
                }
            }
        }


        internal TResult ExecuteBatch<TResult>(string connectionKey, Func<TResult> executor)
        {
            if (executor == null)
            {
                throw new ArgumentNullException(nameof(executor));
            }

            DatabaseExecutionContextLease lease = AcquireExecutionContext(connectionKey);

            if (!lease.OwnsContext)
            {
                return executor();
            }

            try
            {
                lease.Context.EnsureOpen();

                using IDisposable scope = DatabaseExecutionContextScope.Push(lease.Context);

                return executor();
            }
            finally
            {
                lease.Context.Dispose();
            }
        }

        internal async Task<TResult> ExecuteBatchAsync<TResult>(string connectionKey, Func<Task<TResult>> executor, CancellationToken cancellationToken = default)
        {
            if (executor == null)
            {
                throw new ArgumentNullException(nameof(executor));
            }

            DatabaseExecutionContextLease lease = AcquireExecutionContext(connectionKey);

            if (!lease.OwnsContext)
            {
                return await executor().ConfigureAwait(false);
            }

            try
            {
                await lease.Context.EnsureOpenAsync(cancellationToken).ConfigureAwait(false);

                using IDisposable scope = DatabaseExecutionContextScope.Push(lease.Context);

                return await executor().ConfigureAwait(false);
            }
            finally
            {
                await lease.Context.DisposeAsync().ConfigureAwait(false);
            }
        }


        internal DatabaseExecutionContextLease AcquireExecutionContext(string connectionKey, DatabaseExecutionContext? executionContext = null)
        {
            if (string.IsNullOrWhiteSpace(connectionKey))
            {
                throw new ArgumentException("Connection key cannot be null, empty or whitespace.", nameof(connectionKey));
            }

            DatabaseExecutionContext? context = executionContext ?? DatabaseExecutionContextScope.Current;

            if (context != null)
            {
                ValidateExecutionContext(connectionKey, context);

                return new DatabaseExecutionContextLease(context, false);
            }

            return new DatabaseExecutionContextLease(CreateExecutionContext(connectionKey), true);
        }

        internal DatabaseExecutionContext CreateExecutionContext(string connectionKey)
        {
            if (string.IsNullOrWhiteSpace(connectionKey))
            {
                throw new ArgumentException("Connection key cannot be null, empty or whitespace.", nameof(connectionKey));
            }

            return new DatabaseExecutionContext(connectionKey, _context.CreateSqlConnection(connectionKey), true);
        }

        private static void ValidateExecutionContext(string connectionKey, DatabaseExecutionContext executionContext)
        {
            executionContext.ThrowIfDisposed();

            if (!string.Equals(connectionKey, executionContext.ConnectionKey, StringComparison.OrdinalIgnoreCase))
            {
                throw new InvalidOperationException($"Execution context uses connection '{executionContext.ConnectionKey}', but the current operation requires connection '{connectionKey}'.");
            }
        }

        private static void ValidateArguments<TResult>(string connectionKey, Func<DatabaseExecutionContext, TResult> executor)
        {
            if (string.IsNullOrWhiteSpace(connectionKey))
            {
                throw new ArgumentException("Connection key cannot be null, empty or whitespace.", nameof(connectionKey));
            }

            if (executor == null)
            {
                throw new ArgumentNullException(nameof(executor));
            }
        }

        private static void ValidateArguments<TResult>(string connectionKey, Func<DatabaseExecutionContext, Task<TResult>> executor)
        {
            if (string.IsNullOrWhiteSpace(connectionKey))
            {
                throw new ArgumentException("Connection key cannot be null, empty or whitespace.", nameof(connectionKey));
            }

            if (executor == null)
            {
                throw new ArgumentNullException(nameof(executor));
            }
        }
    }
}