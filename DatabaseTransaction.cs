using DapperGlib.Internal;

namespace DapperGlib
{
    public sealed class DatabaseTransaction : IDisposable, IAsyncDisposable
    {
        private readonly DatabaseExecutionContext _executionContext;
        private readonly IDisposable _scope;
        private bool _completed;
        private bool _disposed;

        internal DatabaseTransaction(DatabaseExecutionContext executionContext, IDisposable scope)
        {
            _executionContext = executionContext ?? throw new ArgumentNullException(nameof(executionContext));
            _scope = scope ?? throw new ArgumentNullException(nameof(scope));
        }

        public void Commit()
        {
            ThrowIfDisposed();
            EnsureNotCompleted();

            try
            {
                _executionContext.CommitTransaction();
            }
            finally
            {
                _completed = true;
                DisposeResources();
            }
        }

        public async Task CommitAsync(CancellationToken cancellationToken = default)
        {
            ThrowIfDisposed();
            EnsureNotCompleted();

            try
            {
                await _executionContext.CommitTransactionAsync(cancellationToken).ConfigureAwait(false);
            }
            finally
            {
                _completed = true;
                await DisposeResourcesAsync().ConfigureAwait(false);
            }
        }

        public void Rollback()
        {
            ThrowIfDisposed();
            EnsureNotCompleted();

            try
            {
                _executionContext.RollbackTransaction();
            }
            finally
            {
                _completed = true;
                DisposeResources();
            }
        }

        public async Task RollbackAsync(CancellationToken cancellationToken = default)
        {
            ThrowIfDisposed();
            EnsureNotCompleted();

            try
            {
                await _executionContext.RollbackTransactionAsync(cancellationToken).ConfigureAwait(false);
            }
            finally
            {
                _completed = true;
                await DisposeResourcesAsync().ConfigureAwait(false);
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
                if (!_completed && _executionContext.HasTransaction)
                {
                    _executionContext.RollbackTransaction();
                    _completed = true;
                }
            }
            finally
            {
                DisposeResources();
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
                if (!_completed && _executionContext.HasTransaction)
                {
                    await _executionContext.RollbackTransactionAsync(CancellationToken.None).ConfigureAwait(false);
                    _completed = true;
                }
            }
            finally
            {
                await DisposeResourcesAsync().ConfigureAwait(false);
            }
        }

        private void DisposeResources()
        {
            if (_disposed)
            {
                return;
            }

            try
            {
                _scope.Dispose();
            }
            finally
            {
                _executionContext.Dispose();
                _disposed = true;
            }
        }

        private async ValueTask DisposeResourcesAsync()
        {
            if (_disposed)
            {
                return;
            }

            try
            {
                _scope.Dispose();
            }
            finally
            {
                await _executionContext.DisposeAsync().ConfigureAwait(false);
                _disposed = true;
            }
        }

        private void EnsureNotCompleted()
        {
            if (_completed)
            {
                throw new InvalidOperationException("The transaction has already been completed.");
            }
        }

        private void ThrowIfDisposed()
        {
            if (_disposed)
            {
                throw new ObjectDisposedException(nameof(DatabaseTransaction));
            }
        }
    }
}