using Dapper;
using DapperGlib.Internal;
using System.Data;
using System.Threading;

namespace DapperGlib
{
    public sealed class RawCommandBuilder
    {
        private readonly GlipContext _context = new();

        private readonly DatabaseCommandExecutor _executor;

        private readonly string _connectionKey;

        private readonly object? _parameters;

        private int? _commandTimeout;


        public string Sql
        {
            get;
        }


        internal RawCommandBuilder(string sql, string connectionKey, object? parameters = null)
        {
            if (string.IsNullOrWhiteSpace(
                sql))
            {
                throw new ArgumentException(
                    "Raw SQL cannot be null or empty.",
                    nameof(sql)
                );
            }

            if (string.IsNullOrWhiteSpace(
                connectionKey))
            {
                throw new ArgumentException(
                    "Connection key cannot be null or empty.",
                    nameof(connectionKey)
                );
            }

            Sql = sql.Trim();

            _connectionKey = connectionKey.Trim();

            _parameters = parameters;

            _executor = new DatabaseCommandExecutor(_context);

        }


        /*
         * ============================================================
         * TIMEOUT
         * ============================================================
         */

        public RawCommandBuilder Timeout(
            int seconds)
        {
            if (seconds <= 0)
            {
                throw new ArgumentOutOfRangeException(
                    nameof(seconds),
                    seconds,
                    "Timeout must be greater than zero seconds."
                );
            }

            _commandTimeout =
                seconds;

            return this;
        }


        /*
         * ============================================================
         * QUERY
         * ============================================================
         */

        public List<TResult> Query<TResult>()
        {
            return _executor.Execute(_connectionKey, context =>
            {
                CommandDefinition command = CreateCommand(transaction: context.Transaction);
                return context.Connection.Query<TResult>(command).AsList();
            });
        }


        public Task<List<TResult>> QueryAsync<TResult>()
        {
            return QueryAsync<TResult>(
                CancellationToken.None
            );
        }


        public async Task<List<TResult>> QueryAsync<TResult>(CancellationToken cancellationToken)
        {
            IEnumerable<TResult> result = await _executor.ExecuteAsync(_connectionKey, context =>
            {
                CommandDefinition command = CreateCommand(cancellationToken, context.Transaction);
                return context.Connection.QueryAsync<TResult>(command);
            }, cancellationToken).ConfigureAwait(false);

            return result.AsList();
        }


        /*
         * ============================================================
         * EXECUTE
         * ============================================================
         */

        public int Execute()
        {
            return _executor.Execute(_connectionKey, context =>
            {
                CommandDefinition command = CreateCommand(transaction: context.Transaction);
                return context.Connection.Execute(command);
            });
        }


        public Task<int> ExecuteAsync()
        {
            return ExecuteAsync(
                CancellationToken.None
            );
        }


        public async Task<int> ExecuteAsync(CancellationToken cancellationToken)
        {
            return await _executor.ExecuteAsync(_connectionKey, context =>
            {
                CommandDefinition command = CreateCommand(cancellationToken, context.Transaction);
                return context.Connection.ExecuteAsync(command);
            }, cancellationToken).ConfigureAwait(false);
        }


        /*
         * ============================================================
         * SCALAR
         * ============================================================
         */

        public TResult Scalar<TResult>()
        {
            return _executor.Execute(_connectionKey, context =>
            {
                CommandDefinition command = CreateCommand(transaction: context.Transaction);
                return context.Connection.ExecuteScalar<TResult>(command)!;
            });
        }


        public Task<TResult> ScalarAsync<TResult>()
        {
            return ScalarAsync<TResult>(
                CancellationToken.None
            );
        }

        public async Task<TResult> ScalarAsync<TResult>(CancellationToken cancellationToken)
        {
            return (await _executor.ExecuteAsync(_connectionKey, context =>
            {
                CommandDefinition command = CreateCommand(cancellationToken, context.Transaction);
                return context.Connection.ExecuteScalarAsync<TResult>(command);
            }, cancellationToken).ConfigureAwait(false))!;
        }


        /*
         * ============================================================
         * QUERY MULTIPLE
         * ============================================================
         */

        public MultipleResultReader QueryMultiple()
        {
            DatabaseExecutionContextLease lease =
                _executor.AcquireExecutionContext(
                    _connectionKey
                );

            try
            {
                lease.Context.EnsureOpen();

                CommandDefinition command = CreateCommand(transaction: lease.Context.Transaction);

                SqlMapper.GridReader reader =
                    lease.Context.Connection.QueryMultiple(
                        command
                    );

                return new MultipleResultReader(
                    lease.Context,
                    reader,
                    disposeExecutionContext: lease.OwnsContext
                );
            }
            catch
            {
                if (lease.OwnsContext)
                {
                    lease.Context.Dispose();
                }

                throw;
            }
        }


        public Task<MultipleResultReader> QueryMultipleAsync()
        {
            return QueryMultipleAsync(
                CancellationToken.None
            );
        }


        public async Task<MultipleResultReader> QueryMultipleAsync(CancellationToken cancellationToken)
        {
            DatabaseExecutionContextLease lease =
                _executor.AcquireExecutionContext(
                    _connectionKey
                );

            try
            {
                await lease.Context
                    .EnsureOpenAsync(
                        cancellationToken
                    )
                    .ConfigureAwait(false);

               CommandDefinition command = CreateCommand(cancellationToken, lease.Context.Transaction);

                SqlMapper.GridReader reader =
                    await lease.Context.Connection
                        .QueryMultipleAsync(
                            command
                        )
                        .ConfigureAwait(false);

                return new MultipleResultReader(
                    lease.Context,
                    reader,
                    disposeExecutionContext: lease.OwnsContext
                );
            }
            catch
            {
                if (lease.OwnsContext)
                {
                    await lease.Context
                        .DisposeAsync()
                        .ConfigureAwait(false);
                }

                throw;
            }
        }


        /*
         * ============================================================
         * INTERNAL
         * ============================================================
         */

        private int? GetCommandTimeout()
        {
            return _commandTimeout
                ?? _context.CommandTimeout;
        }


        private CommandDefinition CreateCommand(CancellationToken cancellationToken = default, IDbTransaction? transaction = null)
        {
            return CommandDefinitionFactory.Create(
                commandText: Sql,
                parameters: _parameters,
                commandTimeout: GetCommandTimeout(),
                commandType: CommandType.Text,
                transaction: transaction,
                cancellationToken: cancellationToken
            );
        }


    }
}