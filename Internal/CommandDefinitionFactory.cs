using Dapper;
using System.Data;
using System.Threading;

namespace DapperGlib.Internal
{
    internal static class CommandDefinitionFactory
    {
        internal static CommandDefinition Create(string commandText, object? parameters = null, int? commandTimeout = null, CommandType? commandType = null, IDbTransaction? transaction = null, CancellationToken cancellationToken = default)
        {
            return new CommandDefinition(
                commandText: commandText,
                parameters: parameters,
                transaction: transaction,
                commandTimeout: commandTimeout,
                commandType: commandType,
                cancellationToken: cancellationToken
            );
        }
    }
}