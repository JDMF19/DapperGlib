using Dapper;
using DapperGlib.Internal;
using System.Data;
using System.Threading;

namespace DapperGlib
{
    public sealed class RawCommandBuilder
    {
        private readonly GlipContext _context =
            new();

        private readonly string _connectionKey;

        private readonly object? _parameters;

        private int? _commandTimeout;


        public string Sql
        {
            get;
        }


        internal RawCommandBuilder(
            string sql,
            string connectionKey,
            object? parameters = null)
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

            Sql =
                sql.Trim();

            _connectionKey =
                connectionKey.Trim();

            _parameters =
                parameters;
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
            using var connection =
                _context.CreateConnection(
                    _connectionKey
                );

            CommandDefinition command =
                CreateCommand();

            return connection
                .Query<TResult>(
                    command
                )
                .AsList();
        }


        public Task<List<TResult>> QueryAsync<TResult>()
        {
            return QueryAsync<TResult>(
                CancellationToken.None
            );
        }


        public async Task<List<TResult>> QueryAsync<TResult>(
            CancellationToken cancellationToken)
        {
            using var connection =
                _context.CreateConnection(
                    _connectionKey
                );

            CommandDefinition command =
                CreateCommand(
                    cancellationToken
                );

            var result =
                await connection
                    .QueryAsync<TResult>(
                        command
                    )
                    .ConfigureAwait(false);

            return result.AsList();
        }


        /*
         * ============================================================
         * EXECUTE
         * ============================================================
         */

        public int Execute()
        {
            using var connection =
                _context.CreateConnection(
                    _connectionKey
                );

            CommandDefinition command =
                CreateCommand();

            return connection
                .Execute(
                    command
                );
        }


        public Task<int> ExecuteAsync()
        {
            return ExecuteAsync(
                CancellationToken.None
            );
        }


        public async Task<int> ExecuteAsync(
            CancellationToken cancellationToken)
        {
            using var connection =
                _context.CreateConnection(
                    _connectionKey
                );

            CommandDefinition command =
                CreateCommand(
                    cancellationToken
                );

            return await connection
                .ExecuteAsync(
                    command
                )
                .ConfigureAwait(false);
        }


        /*
         * ============================================================
         * SCALAR
         * ============================================================
         */

        public TResult Scalar<TResult>()
        {
            using var connection =
                _context.CreateConnection(
                    _connectionKey
                );

            CommandDefinition command =
                CreateCommand();

            return connection
                .ExecuteScalar<TResult>(
                    command
                )!;
        }


        public Task<TResult> ScalarAsync<TResult>()
        {
            return ScalarAsync<TResult>(
                CancellationToken.None
            );
        }


        public async Task<TResult> ScalarAsync<TResult>(
            CancellationToken cancellationToken)
        {
            using var connection =
                _context.CreateConnection(
                    _connectionKey
                );

            CommandDefinition command =
                CreateCommand(
                    cancellationToken
                );

            return (
                await connection
                    .ExecuteScalarAsync<TResult>(
                        command
                    )
                    .ConfigureAwait(false)
            )!;
        }


        /*
         * ============================================================
         * QUERY MULTIPLE
         * ============================================================
         */

        public MultipleResultReader QueryMultiple()
        {
            IDbConnection connection =
                _context.CreateConnection(
                    _connectionKey
                );

            try
            {
                CommandDefinition command =
                    CreateCommand();

                SqlMapper.GridReader reader =
                    connection.QueryMultiple(
                        command
                    );

                /*
                 * MultipleResultReader pasa a ser
                 * propietario de connection + reader.
                 */
                return new MultipleResultReader(
                    connection,
                    reader
                );
            }
            catch
            {
                connection.Dispose();

                throw;
            }
        }


        public Task<MultipleResultReader> QueryMultipleAsync()
        {
            return QueryMultipleAsync(
                CancellationToken.None
            );
        }


        public async Task<MultipleResultReader> QueryMultipleAsync(
            CancellationToken cancellationToken)
        {
            IDbConnection connection =
                _context.CreateConnection(
                    _connectionKey
                );

            try
            {
                CommandDefinition command =
                    CreateCommand(
                        cancellationToken
                    );

                SqlMapper.GridReader reader =
                    await connection
                        .QueryMultipleAsync(
                            command
                        )
                        .ConfigureAwait(false);

                return new MultipleResultReader(
                    connection,
                    reader
                );
            }
            catch
            {
                connection.Dispose();

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


        private CommandDefinition CreateCommand(
            CancellationToken cancellationToken = default)
        {
            return CommandDefinitionFactory
                .Create(
                    commandText:
                        Sql,

                    parameters:
                        _parameters,

                    commandTimeout:
                        GetCommandTimeout(),

                    commandType:
                        CommandType.Text,

                    cancellationToken:
                        cancellationToken
                );
        }
    }
}