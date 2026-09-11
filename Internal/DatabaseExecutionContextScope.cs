using System.Threading;

namespace DapperGlib.Internal
{
    internal static class DatabaseExecutionContextScope
    {
        private static readonly AsyncLocal<ContextHolder?> _current = new();

        internal static DatabaseExecutionContext? Current
        {
            get
            {
                ContextHolder? holder = _current.Value;

                while (holder != null)
                {
                    if (holder.ExecutionContext != null)
                    {
                        return holder.ExecutionContext;
                    }

                    holder = holder.Previous;
                }

                return null;
            }
        }

        internal static IDisposable Push(DatabaseExecutionContext executionContext)
        {
            if (executionContext == null)
            {
                throw new ArgumentNullException(nameof(executionContext));
            }

            executionContext.ThrowIfDisposed();

            var holder = new ContextHolder(executionContext, _current.Value);
            _current.Value = holder;

            return new Scope(holder);
        }

        private sealed class ContextHolder
        {
            internal DatabaseExecutionContext? ExecutionContext { get; set; }
            internal ContextHolder? Previous { get; }

            internal ContextHolder(DatabaseExecutionContext executionContext, ContextHolder? previous)
            {
                ExecutionContext = executionContext;
                Previous = previous;
            }
        }

        private sealed class Scope : IDisposable
        {
            private readonly ContextHolder _holder;
            private bool _disposed;

            internal Scope(ContextHolder holder)
            {
                _holder = holder;
            }

            public void Dispose()
            {
                if (_disposed)
                {
                    return;
                }

                /*
                 * Invalidamos el holder, no solamente el valor local del
                 * AsyncLocal. El mismo holder puede estar fluyendo por
                 * distintas continuaciones async.
                 */
                _holder.ExecutionContext = null;

                if (ReferenceEquals(_current.Value, _holder))
                {
                    _current.Value = _holder.Previous;
                }

                _disposed = true;
            }
        }
    }
}