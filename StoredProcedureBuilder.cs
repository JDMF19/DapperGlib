using Dapper;
using DapperGlib.Internal;
using System.Data;
using System.Threading;

namespace DapperGlib
{
    public class StoredProcedureBuilder
    {
        private readonly GlipContext _context = new();
        private readonly DatabaseCommandExecutor _executor;

        private readonly string _connectionKey;

        private readonly DynamicParameters _parameters = new();

        private readonly HashSet<string> _parameterNames = new(
                StringComparer.OrdinalIgnoreCase
            );

        private int? _commandTimeout;

        private bool _hasExecuted;


        public string ProcedureName
        {
            get;
        }


        internal StoredProcedureBuilder(string procedureName, string connectionKey)
        {
            if (string.IsNullOrWhiteSpace(
                procedureName))
            {
                throw new ArgumentException(
                    "Stored procedure name cannot be null or empty.",
                    nameof(procedureName)
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

            ProcedureName = procedureName.Trim();

            _connectionKey = connectionKey.Trim();

            _executor = new DatabaseCommandExecutor(_context);
        }

        public StoredProcedureBuilder Parameter<TValue>(string name, TValue value, DbType? dbType = null, int? size = null, byte? precision = null, byte? scale = null)
        {
            string parameterName =
                PrepareParameterName(
                    name
                );

            DbType? effectiveDbType =
                dbType;

            if (value is null &&
                !effectiveDbType.HasValue)
            {
                effectiveDbType =
                    ResolveDbType(
                        typeof(TValue)
                    );
            }

            _parameters.Add(
                parameterName,
                value,
                effectiveDbType,
                ParameterDirection.Input,
                size,
                precision,
                scale
            );

            return this;
        }

        public StoredProcedureBuilder Parameter(string name, object? value, DbType dbType, int? size = null, byte? precision = null, byte? scale = null)
        {
            string parameterName = PrepareParameterName(
                    name
                );

            _parameters.Add(
                parameterName,
                value,
                dbType,
                ParameterDirection.Input,
                size,
                precision,
                scale
            );

            return this;
        }


        public StoredProcedureBuilder Output<TValue>(string name, int? size = null, byte? precision = null, byte? scale = null)
        {
            string parameterName =
                PrepareParameterName(
                    name
                );

            DbType dbType =
                ResolveDbType(
                    typeof(TValue)
                );

            _parameters.Add(
                parameterName,
                value: null,
                dbType:
                    dbType,
                direction:
                    ParameterDirection.Output,
                size:
                    size,
                precision:
                    precision,
                scale:
                    scale
            );

            return this;
        }

        public StoredProcedureBuilder Output(string name, DbType dbType, int? size = null, byte? precision = null, byte? scale = null)
        {
            string parameterName =
                PrepareParameterName(
                    name
                );

            _parameters.Add(
                parameterName,
                value: null,
                dbType:
                    dbType,
                direction:
                    ParameterDirection.Output,
                size:
                    size,
                precision:
                    precision,
                scale:
                    scale
            );

            return this;
        }


        public StoredProcedureBuilder InputOutput<TValue>(string name, TValue value, DbType? dbType = null, int? size = null, byte? precision = null, byte? scale = null)
        {
            string parameterName =
                PrepareParameterName(
                    name
                );

            DbType effectiveDbType =
                dbType
                ??
                ResolveDbType(
                    typeof(TValue)
                );

            _parameters.Add(
                parameterName,
                value,
                effectiveDbType,
                ParameterDirection.InputOutput,
                size,
                precision,
                scale
            );

            return this;
        }


        public StoredProcedureBuilder ReturnValue(string name = "ReturnValue")
        {
            string parameterName =
                PrepareParameterName(
                    name
                );

            _parameters.Add(
                parameterName,
                value: null,
                dbType:
                    DbType.Int32,
                direction:
                    ParameterDirection.ReturnValue
            );

            return this;
        }

        public TValue Get<TValue>(string name)
        {
            if (!_hasExecuted)
            {
                throw new InvalidOperationException(
                    "The stored procedure must be executed " +
                    "before reading output parameters."
                );
            }

            string parameterName =
                NormalizeParameterName(
                    name
                );

            return _parameters
                .Get<TValue>(
                    parameterName
                );
        }


        public StoredProcedureBuilder Timeout(int seconds)
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

        private int? GetCommandTimeout()
        {
            return _commandTimeout
                ?? _context.CommandTimeout;
        }


        private CommandDefinition CreateCommand(CancellationToken cancellationToken = default, IDbTransaction? transaction = null)
        {
            return CommandDefinitionFactory.Create(
                commandText: ProcedureName,
                parameters: _parameters,
                commandTimeout: GetCommandTimeout(),
                commandType: CommandType.StoredProcedure,
                transaction: transaction,
                cancellationToken: cancellationToken
            );
        }


        private TResult ExecuteCore<TResult>(Func<IDbConnection, CommandDefinition, TResult> executor)
        {
            return _executor.Execute(_connectionKey, context =>
            {
                CommandDefinition command = CreateCommand(transaction: context.Transaction);
                TResult result = executor(context.Connection, command);
                _hasExecuted = true;

                return result;
            });
        }

        private Task<TResult> ExecuteCoreAsync<TResult>(Func<IDbConnection, CommandDefinition, Task<TResult>> executor, CancellationToken cancellationToken)
        {
            return _executor.ExecuteAsync(_connectionKey, async context =>
            {
                CommandDefinition command = CreateCommand(cancellationToken, context.Transaction);
                TResult result = await executor(context.Connection, command).ConfigureAwait(false);
                _hasExecuted = true;

                return result;
            }, cancellationToken);
        }


        public int Execute()
        {
            return ExecuteCore(
                (
                    connection,
                    command
                ) =>
                    connection.Execute(
                        command
                    )
            );
        }


        public Task<int> ExecuteAsync()
        {
            return ExecuteAsync(
                CancellationToken.None
            );
        }

        public Task<int> ExecuteAsync(
            CancellationToken cancellationToken)
        {
            return ExecuteCoreAsync(
                (
                    connection,
                    command
                ) =>
                    connection.ExecuteAsync(
                        command
                    ),
                cancellationToken
            );
        }

        public List<TResult> ToList<TResult>()
        {
            return ExecuteCore(
                (
                    connection,
                    command
                ) =>
                    connection
                        .Query<TResult>(
                            command
                        )
                        .AsList()
            );
        }


        public Task<List<TResult>> ToListAsync<TResult>()
        {
            return ToListAsync<TResult>(
                CancellationToken.None
            );
        }

        public Task<List<TResult>> ToListAsync<TResult>(
            CancellationToken cancellationToken)
        {
            return ExecuteCoreAsync(
                async (
                    connection,
                    command
                ) =>
                {
                    var result =
                        await connection
                            .QueryAsync<TResult>(
                                command
                            )
                            .ConfigureAwait(false);

                    return result.AsList();
                },
                cancellationToken
            );
        }


        public TResult First<TResult>()
        {
            return ExecuteCore(
                (
                    connection,
                    command
                ) =>
                    connection
                        .QueryFirst<TResult>(
                            command
                        )
            );
        }

        public Task<TResult> FirstAsync<TResult>()
        {
            return FirstAsync<TResult>(
                CancellationToken.None
            );
        }

        public Task<TResult> FirstAsync<TResult>(
            CancellationToken cancellationToken)
        {
            return ExecuteCoreAsync(
                (
                    connection,
                    command
                ) =>
                    connection
                        .QueryFirstAsync<TResult>(
                            command
                        ),
                cancellationToken
            );
        }


        public TResult? FirstOrDefault<TResult>()
        {
            return ExecuteCore(
                (
                    connection,
                    command
                ) =>
                    connection
                        .QueryFirstOrDefault<TResult>(
                            command
                        )
            );
        }

        public Task<TResult?> FirstOrDefaultAsync<TResult>()
        {
            return FirstOrDefaultAsync<TResult>(
                CancellationToken.None
            );
        }

        public Task<TResult?> FirstOrDefaultAsync<TResult>(
            CancellationToken cancellationToken)
        {
            return ExecuteCoreAsync(
                (
                    connection,
                    command
                ) =>
                    connection
                        .QueryFirstOrDefaultAsync<TResult>(
                            command
                        ),
                cancellationToken
            );
        }


        /*
         * ============================================================
         * SINGLE
         * ============================================================
         */

        public TResult Single<TResult>()
        {
            return ExecuteCore(
                (
                    connection,
                    command
                ) =>
                    connection
                        .QuerySingle<TResult>(
                            command
                        )
            );
        }


        public Task<TResult> SingleAsync<TResult>()
        {
            return SingleAsync<TResult>(
                CancellationToken.None
            );
        }


        public Task<TResult> SingleAsync<TResult>(
            CancellationToken cancellationToken)
        {
            return ExecuteCoreAsync(
                (
                    connection,
                    command
                ) =>
                    connection
                        .QuerySingleAsync<TResult>(
                            command
                        ),
                cancellationToken
            );
        }


        /*
         * ============================================================
         * SINGLE OR DEFAULT
         * ============================================================
         */

        public TResult? SingleOrDefault<TResult>()
        {
            return ExecuteCore(
                (
                    connection,
                    command
                ) =>
                    connection
                        .QuerySingleOrDefault<TResult>(
                            command
                        )
            );
        }


        public Task<TResult?> SingleOrDefaultAsync<TResult>()
        {
            return SingleOrDefaultAsync<TResult>(
                CancellationToken.None
            );
        }


        public Task<TResult?> SingleOrDefaultAsync<TResult>(
            CancellationToken cancellationToken)
        {
            return ExecuteCoreAsync(
                (
                    connection,
                    command
                ) =>
                    connection
                        .QuerySingleOrDefaultAsync<TResult>(
                            command
                        ),
                cancellationToken
            );
        }

        public TResult Scalar<TResult>()
        {
            return ExecuteCore(
                (
                    connection,
                    command
                ) =>
                    connection
                        .ExecuteScalar<TResult>(
                            command
                        )!
            );
        }


        public Task<TResult> ScalarAsync<TResult>()
        {
            return ScalarAsync<TResult>(
                CancellationToken.None
            );
        }

        public Task<TResult> ScalarAsync<TResult>(
            CancellationToken cancellationToken)
        {
            return ExecuteCoreAsync(
                async (
                    connection,
                    command
                ) =>
                {
                    return (
                        await connection
                            .ExecuteScalarAsync<TResult>(
                                command
                            )
                            .ConfigureAwait(false)
                    )!;
                },
                cancellationToken
            );
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
                    () => _hasExecuted = true,
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
                    () => _hasExecuted = true,
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

        private string PrepareParameterName(string name)
        {
            string normalized =
                NormalizeParameterName(
                    name
                );

            if (!_parameterNames.Add(
                normalized))
            {
                throw new ArgumentException(
                    $"Stored procedure parameter " +
                    $"'{normalized}' has already been added.",
                    nameof(name)
                );
            }

            return normalized;
        }

        private static string NormalizeParameterName(
    string name)
        {
            if (string.IsNullOrWhiteSpace(
                name))
            {
                throw new ArgumentException(
                    "Parameter name cannot be null or empty.",
                    nameof(name)
                );
            }

            string result =
                name.Trim();

            while (
                result.StartsWith("@") ||
                result.StartsWith(":") ||
                result.StartsWith("?"))
            {
                result =
                    result.Substring(1);
            }

            if (string.IsNullOrWhiteSpace(
                result))
            {
                throw new ArgumentException(
                    "Parameter name cannot be empty.",
                    nameof(name)
                );
            }

            return result;
        }


        private static DbType ResolveDbType(
    Type type)
        {
            Type targetType =
                Nullable.GetUnderlyingType(
                    type
                )
                ?? type;

            if (targetType.IsEnum)
            {
                targetType =
                    Enum.GetUnderlyingType(
                        targetType
                    );
            }

            if (targetType == typeof(string))
                return DbType.String;

            if (targetType == typeof(int))
                return DbType.Int32;

            if (targetType == typeof(long))
                return DbType.Int64;

            if (targetType == typeof(short))
                return DbType.Int16;

            if (targetType == typeof(byte))
                return DbType.Byte;

            if (targetType == typeof(bool))
                return DbType.Boolean;

            if (targetType == typeof(decimal))
                return DbType.Decimal;

            if (targetType == typeof(double))
                return DbType.Double;

            if (targetType == typeof(float))
                return DbType.Single;

            if (targetType == typeof(Guid))
                return DbType.Guid;

            if (targetType == typeof(DateTime))
                return DbType.DateTime;

            if (targetType == typeof(DateTimeOffset))
                return DbType.DateTimeOffset;

            if (targetType == typeof(TimeSpan))
                return DbType.Time;

            if (targetType == typeof(DateOnly))
                return DbType.Date;

            if (targetType == typeof(TimeOnly))
                return DbType.Time;

            if (targetType == typeof(byte[]))
                return DbType.Binary;

            if (targetType == typeof(char))
                return DbType.StringFixedLength;

            throw new ArgumentException(
                $"Unable to infer DbType for CLR type " +
                $"'{type.FullName}'. Specify DbType explicitly."
            );
        }

    }

}