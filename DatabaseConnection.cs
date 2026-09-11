using DapperGlib.Internal;
using System.Data;
using System.Threading;

namespace DapperGlib
{
    public sealed class DatabaseConnection
    {
        internal string ConnectionKey
        {
            get;
        }


        internal DatabaseConnection(
            string connectionKey)
        {
            if (string.IsNullOrWhiteSpace(
                connectionKey))
            {
                throw new ArgumentException(
                    "Connection key cannot be null or empty.",
                    nameof(connectionKey)
                );
            }

            ConnectionKey =
                connectionKey.Trim();
        }


        /*
         * ============================================================
         * TRANSACTION
         * ============================================================
         */


        public DatabaseTransaction BeginTransaction(IsolationLevel isolationLevel = IsolationLevel.ReadCommitted)
        {
            EnsureNoActiveTransaction();

            var context = new GlipContext();
            var executor = new DatabaseCommandExecutor(context);
            DatabaseExecutionContext executionContext = executor.CreateExecutionContext(ConnectionKey);

            try
            {
                executionContext.BeginTransaction(isolationLevel);

                IDisposable scope = DatabaseExecutionContextScope.Push(executionContext);

                return new DatabaseTransaction(executionContext, scope);
            }
            catch
            {
                executionContext.Dispose();
                throw;
            }
        }


        public Task<DatabaseTransaction> BeginTransactionAsync(IsolationLevel isolationLevel = IsolationLevel.ReadCommitted)
        {
            return BeginTransactionAsync(isolationLevel, CancellationToken.None);
        }

        public Task<DatabaseTransaction> BeginTransactionAsync(IsolationLevel isolationLevel, CancellationToken cancellationToken)
        {
            EnsureNoActiveTransaction();

            var context = new GlipContext();
            var executor = new DatabaseCommandExecutor(context);
            DatabaseExecutionContext executionContext = executor.CreateExecutionContext(ConnectionKey);
            IDisposable scope = DatabaseExecutionContextScope.Push(executionContext);

            return BeginTransactionAsyncCore(executionContext, scope, isolationLevel, cancellationToken);
        }


        public void Transaction(Action action, IsolationLevel isolationLevel = IsolationLevel.ReadCommitted)
        {
            if (action == null)
            {
                throw new ArgumentNullException(nameof(action));
            }

            EnsureNoActiveTransaction();

            var context = new GlipContext();
            var executor = new DatabaseCommandExecutor(context);

            using DatabaseExecutionContext executionContext =
                executor.CreateExecutionContext(ConnectionKey);

            executionContext.BeginTransaction(isolationLevel);

            using IDisposable scope = DatabaseExecutionContextScope.Push(executionContext);

            try
            {
                action();

                executionContext.CommitTransaction();
            }
            catch
            {
                if (executionContext.HasTransaction)
                {
                    executionContext.RollbackTransaction();
                }

                throw;
            }
        }


        public Task TransactionAsync(Func<Task> action, IsolationLevel isolationLevel = IsolationLevel.ReadCommitted)
        {
            return TransactionAsync(action, isolationLevel, CancellationToken.None);
        }

        public async Task TransactionAsync(Func<Task> action, IsolationLevel isolationLevel, CancellationToken cancellationToken)
        {
            if (action == null)
            {
                throw new ArgumentNullException(nameof(action));
            }

            EnsureNoActiveTransaction();


            var context = new GlipContext();
            var executor = new DatabaseCommandExecutor(context);

            await using DatabaseExecutionContext executionContext = executor.CreateExecutionContext(ConnectionKey);

            await executionContext.BeginTransactionAsync(isolationLevel, cancellationToken).ConfigureAwait(false);

            using IDisposable scope = DatabaseExecutionContextScope.Push(executionContext);

            try
            {
                await action().ConfigureAwait(false);

                await executionContext.CommitTransactionAsync(cancellationToken).ConfigureAwait(false);
            }
            catch
            {
                if (executionContext.HasTransaction)
                {
                    await executionContext.RollbackTransactionAsync(CancellationToken.None).ConfigureAwait(false);
                }

                throw;
            }
        }


        private static void EnsureNoActiveTransaction()
        {
            DatabaseExecutionContext? current = DatabaseExecutionContextScope.Current;

            if (current?.HasTransaction == true)
            {
                throw new InvalidOperationException($"A transaction is already active for connection '{current.ConnectionKey}'. Nested transactions are not supported.");
            }
        }


        private static async Task<DatabaseTransaction> BeginTransactionAsyncCore(DatabaseExecutionContext executionContext, IDisposable scope, IsolationLevel isolationLevel, CancellationToken cancellationToken)
        {
            try
            {
                await executionContext.BeginTransactionAsync(isolationLevel, cancellationToken).ConfigureAwait(false);

                return new DatabaseTransaction(executionContext, scope);
            }
            catch
            {
                try
                {
                    scope.Dispose();
                }
                finally
                {
                    await executionContext.DisposeAsync().ConfigureAwait(false);
                }

                throw;
            }
        }


        /*
         * ============================================================
         * STORED PROCEDURE
         * ============================================================
         */

        public StoredProcedureBuilder Procedure(
            string procedureName)
        {
            return new StoredProcedureBuilder(
                procedureName,
                ConnectionKey
            );
        }


        /*
         * ============================================================
         * RAW
         * ============================================================
         */

        public RawCommandBuilder Raw(string sql, object? parameters = null)
        {
            return new RawCommandBuilder(
                sql,
                ConnectionKey,
                parameters
            );
        }

        /*
         * ============================================================
         * QUERY
         * ============================================================
         */

        public List<TResult> Query<TResult>(string sql, object? parameters = null)
        {
            return Raw(
                sql,
                parameters
            )
            .Query<TResult>();
        }


        public Task<List<TResult>> QueryAsync<TResult>(string sql, object? parameters = null)
        {
            return Raw(
                sql,
                parameters
            )
            .QueryAsync<TResult>();
        }


        public Task<List<TResult>> QueryAsync<TResult>(string sql, object? parameters, CancellationToken cancellationToken)
        {
            return Raw(
                sql,
                parameters
            )
            .QueryAsync<TResult>(
                cancellationToken
            );
        }

        /*
         * ============================================================
         * EXECUTE
         * ============================================================
         */

        public int Execute(
            string sql,
            object? parameters = null)
        {
            return Raw(
                sql,
                parameters
            )
            .Execute();
        }


        public Task<int> ExecuteAsync(
            string sql,
            object? parameters = null)
        {
            return Raw(
                sql,
                parameters
            )
            .ExecuteAsync();
        }


        public Task<int> ExecuteAsync(
            string sql,
            object? parameters,
            CancellationToken cancellationToken)
        {
            return Raw(
                sql,
                parameters
            )
            .ExecuteAsync(
                cancellationToken
            );
        }


        /*
         * ============================================================
         * SCALAR
         * ============================================================
         */

        public TResult Scalar<TResult>(
            string sql,
            object? parameters = null)
        {
            return Raw(
                sql,
                parameters
            )
            .Scalar<TResult>();
        }


        public Task<TResult> ScalarAsync<TResult>(
            string sql,
            object? parameters = null)
        {
            return Raw(
                sql,
                parameters
            )
            .ScalarAsync<TResult>();
        }


        public Task<TResult> ScalarAsync<TResult>(
            string sql,
            object? parameters,
            CancellationToken cancellationToken)
        {
            return Raw(
                sql,
                parameters
            )
            .ScalarAsync<TResult>(
                cancellationToken
            );
        }

        /*
         * ============================================================
         * QUERY MULTIPLE
         * ============================================================
         */

        public MultipleResultReader QueryMultiple(
            string sql,
            object? parameters = null)
        {
            return Raw(
                sql,
                parameters
            )
            .QueryMultiple();
        }


        public Task<MultipleResultReader> QueryMultipleAsync(
            string sql,
            object? parameters = null)
        {
            return Raw(
                sql,
                parameters
            )
            .QueryMultipleAsync();
        }


        public Task<MultipleResultReader> QueryMultipleAsync(
            string sql,
            object? parameters,
            CancellationToken cancellationToken)
        {
            return Raw(
                sql,
                parameters
            )
            .QueryMultipleAsync(
                cancellationToken
            );
        }


    }
}