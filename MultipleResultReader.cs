using Dapper;
using DapperGlib.Internal;

namespace DapperGlib
{
    public sealed class MultipleResultReader : IDisposable
    {
        private readonly DatabaseExecutionContext _executionContext;
        private readonly SqlMapper.GridReader _reader;
        private readonly Action? _onCompleted;
        private readonly bool _disposeExecutionContext;
        private bool _disposed;

        internal MultipleResultReader(DatabaseExecutionContext executionContext, SqlMapper.GridReader reader, Action? onCompleted = null, bool disposeExecutionContext = true)
        {
            _executionContext = executionContext ?? throw new ArgumentNullException(nameof(executionContext));
            _reader = reader ?? throw new ArgumentNullException(nameof(reader));
            _onCompleted = onCompleted;
            _disposeExecutionContext = disposeExecutionContext;
        }

        /*
         * ============================================================
         * READ
         * ============================================================
         */

        public List<TResult> Read<TResult>()
        {
            EnsureNotDisposed();

            return _reader.Read<TResult>().AsList();
        }

        public async Task<List<TResult>> ReadAsync<TResult>()
        {
            EnsureNotDisposed();

            var result = await _reader.ReadAsync<TResult>().ConfigureAwait(false);

            return result.AsList();
        }

        /*
         * ============================================================
         * READ FIRST
         * ============================================================
         */

        public TResult ReadFirst<TResult>()
        {
            EnsureNotDisposed();

            return _reader.ReadFirst<TResult>();
        }

        public Task<TResult> ReadFirstAsync<TResult>()
        {
            EnsureNotDisposed();

            return _reader.ReadFirstAsync<TResult>();
        }

        /*
         * ============================================================
         * READ FIRST OR DEFAULT
         * ============================================================
         */

        public TResult? ReadFirstOrDefault<TResult>()
        {
            EnsureNotDisposed();

            return _reader.ReadFirstOrDefault<TResult>();
        }

        public Task<TResult?> ReadFirstOrDefaultAsync<TResult>()
        {
            EnsureNotDisposed();

            return _reader.ReadFirstOrDefaultAsync<TResult>();
        }

        /*
         * ============================================================
         * READ SINGLE
         * ============================================================
         */

        public TResult ReadSingle<TResult>()
        {
            EnsureNotDisposed();

            return _reader.ReadSingle<TResult>();
        }

        public Task<TResult> ReadSingleAsync<TResult>()
        {
            EnsureNotDisposed();

            return _reader.ReadSingleAsync<TResult>();
        }

        /*
         * ============================================================
         * READ SINGLE OR DEFAULT
         * ============================================================
         */

        public TResult? ReadSingleOrDefault<TResult>()
        {
            EnsureNotDisposed();

            return _reader.ReadSingleOrDefault<TResult>();
        }

        public Task<TResult?> ReadSingleOrDefaultAsync<TResult>()
        {
            EnsureNotDisposed();

            return _reader.ReadSingleOrDefaultAsync<TResult>();
        }

        /*
         * ============================================================
         * DISPOSE
         * ============================================================
         */

        public void Dispose()
        {
            if (_disposed)
            {
                return;
            }

            try
            {
                /*
                 * El GridReader debe cerrarse primero.
                 *
                 * Dapper completa los parámetros OUTPUT y RETURN VALUE
                 * cuando el DataReader termina.
                 */
                _reader.Dispose();
            }
            finally
            {
                try
                {
                    if (_disposeExecutionContext)
                    {
                        _executionContext.Dispose();
                    }
                }
                finally
                {
                    _disposed = true;
                    _onCompleted?.Invoke();
                }
            }
        }

        private void EnsureNotDisposed()
        {
            if (_disposed)
            {
                throw new ObjectDisposedException(nameof(MultipleResultReader));
            }
        }
    }
}