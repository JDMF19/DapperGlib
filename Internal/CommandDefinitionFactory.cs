using Dapper;
using System.Data;
using System.Threading;

namespace DapperGlib.Internal
{
    internal static class CommandDefinitionFactory
    {
        internal static CommandDefinition Create(
            string commandText,
            object? parameters = null,
            int? commandTimeout = null,
            CommandType? commandType = null,
            CancellationToken cancellationToken = default)
        {
            return new CommandDefinition(
                commandText:
                    commandText,

                parameters:
                    parameters,

                commandTimeout:
                    commandTimeout,

                commandType:
                    commandType,

                cancellationToken:
                    cancellationToken
            );
        }
    }
}