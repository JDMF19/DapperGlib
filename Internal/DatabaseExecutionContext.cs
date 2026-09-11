using Microsoft.Data.SqlClient;
using System.Data;
using System.Threading;

namespace DapperGlib.Internal
{
    internal sealed class DatabaseExecutionContext : IDisposable, IAsyncDisposable
    {
        private bool _disposed;

        internal string ConnectionKey { get; }
        internal SqlConnection Connection { get; }
        internal SqlTransaction? Transaction { get; private set; }
        internal bool OwnsConnection { get; }
        internal bool IsDisposed => _disposed;
        internal bool HasTransaction => Transaction != null;

        internal DatabaseExecutionContext(string connectionKey, SqlConnection connection, bool ownsConnection = true)
        {
            if (string.IsNullOrWhiteSpace(connectionKey))
            {
                throw new ArgumentException("Connection key cannot be null, empty or whitespace.", nameof(connectionKey));
            }

            Connection = connection ?? throw new ArgumentNullException(nameof(connection));
            ConnectionKey = connectionKey;
            OwnsConnection = ownsConnection;
        }

        internal void EnsureOpen()
        {
            ThrowIfDisposed();

            if (Connection.State == ConnectionState.Broken)
            {
                Connection.Close();
            }

            if (Connection.State == ConnectionState.Closed)
            {
                Connection.Open();
            }

            if (Connection.State != ConnectionState.Open)
            {
                throw new InvalidOperationException($"Connection '{ConnectionKey}' could not be opened. Current state: {Connection.State}.");
            }
        }

        internal async Task EnsureOpenAsync(CancellationToken cancellationToken = default)
        {
            ThrowIfDisposed();

            if (Connection.State == ConnectionState.Broken)
            {
                await Connection.CloseAsync().ConfigureAwait(false);
            }

            if (Connection.State == ConnectionState.Closed)
            {
                await Connection.OpenAsync(cancellationToken).ConfigureAwait(false);
            }

            if (Connection.State != ConnectionState.Open)
            {
                throw new InvalidOperationException($"Connection '{ConnectionKey}' could not be opened. Current state: {Connection.State}.");
            }
        }

        internal SqlTransaction BeginTransaction(IsolationLevel isolationLevel = IsolationLevel.ReadCommitted)
        {
            ThrowIfDisposed();

            if (Transaction != null)
            {
                throw new InvalidOperationException($"Connection '{ConnectionKey}' already has an active transaction.");
            }

            EnsureOpen();

            Transaction = Connection.BeginTransaction(isolationLevel);

            return Transaction;
        }

        internal async Task<SqlTransaction> BeginTransactionAsync(IsolationLevel isolationLevel = IsolationLevel.ReadCommitted, CancellationToken cancellationToken = default)
        {
            ThrowIfDisposed();

            if (Transaction != null)
            {
                throw new InvalidOperationException($"Connection '{ConnectionKey}' already has an active transaction.");
            }

            await EnsureOpenAsync(cancellationToken).ConfigureAwait(false);

            Transaction = (SqlTransaction)await Connection.BeginTransactionAsync(isolationLevel, cancellationToken).ConfigureAwait(false);

            return Transaction;
        }

        internal void CommitTransaction()
        {
            ThrowIfDisposed();

            SqlTransaction transaction = Transaction ?? throw new InvalidOperationException($"Connection '{ConnectionKey}' does not have an active transaction.");

            try
            {
                transaction.Commit();
            }
            finally
            {
                Transaction = null;
                transaction.Dispose();
            }
        }

        internal async Task CommitTransactionAsync(CancellationToken cancellationToken = default)
        {
            ThrowIfDisposed();

            SqlTransaction transaction = Transaction ?? throw new InvalidOperationException($"Connection '{ConnectionKey}' does not have an active transaction.");

            try
            {
                await transaction.CommitAsync(cancellationToken).ConfigureAwait(false);
            }
            finally
            {
                Transaction = null;
                await transaction.DisposeAsync().ConfigureAwait(false);
            }
        }

        internal void RollbackTransaction()
        {
            ThrowIfDisposed();

            SqlTransaction transaction = Transaction ?? throw new InvalidOperationException($"Connection '{ConnectionKey}' does not have an active transaction.");

            try
            {
                transaction.Rollback();
            }
            finally
            {
                Transaction = null;
                transaction.Dispose();
            }
        }

        internal async Task RollbackTransactionAsync(CancellationToken cancellationToken = default)
        {
            ThrowIfDisposed();

            SqlTransaction transaction = Transaction ?? throw new InvalidOperationException($"Connection '{ConnectionKey}' does not have an active transaction.");

            try
            {
                await transaction.RollbackAsync(cancellationToken).ConfigureAwait(false);
            }
            finally
            {
                Transaction = null;
                await transaction.DisposeAsync().ConfigureAwait(false);
            }
        }

        internal void ThrowIfDisposed()
        {
            if (_disposed)
            {
                throw new ObjectDisposedException(nameof(DatabaseExecutionContext));
            }
        }

        public void Dispose()
        {
            if (_disposed)
            {
                return;
            }

            try
            {
                if (Transaction != null)
                {
                    Transaction.Dispose();
                    Transaction = null;
                }
            }
            finally
            {
                _disposed = true;

                if (OwnsConnection)
                {
                    Connection.Dispose();
                }
            }
        }

        public async ValueTask DisposeAsync()
        {
            if (_disposed)
            {
                return;
            }

            try
            {
                if (Transaction != null)
                {
                    await Transaction.DisposeAsync().ConfigureAwait(false);
                    Transaction = null;
                }
            }
            finally
            {
                _disposed = true;

                if (OwnsConnection)
                {
                    await Connection.DisposeAsync().ConfigureAwait(false);
                }
            }
        }
    }
}