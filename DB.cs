using System.Data;
using System.Threading;

namespace DapperGlib
{
    public static class DB
    {
        /*
         * ============================================================
         * DEFAULT CONNECTION
         * ============================================================
         */

        public static DatabaseConnection Connection()
        {
            return new DatabaseConnection(
                "SqlConnection"
            );
        }


        /*
         * ============================================================
         * NAMED CONNECTION
         * ============================================================
         */

        public static DatabaseConnection Connection(
            string connectionKey)
        {
            return new DatabaseConnection(
                connectionKey
            );
        }


        /*
         * ============================================================
         * TRANSACTIONS
         * ============================================================
         */

        public static void Transaction(Action action, IsolationLevel isolationLevel = IsolationLevel.ReadCommitted)
        {
            Connection().Transaction(action, isolationLevel);
        }

        public static Task TransactionAsync(Func<Task> action, IsolationLevel isolationLevel = IsolationLevel.ReadCommitted)
        {
            return Connection().TransactionAsync(action, isolationLevel);
        }

        public static Task TransactionAsync(Func<Task> action, IsolationLevel isolationLevel, CancellationToken cancellationToken)
        {
            return Connection().TransactionAsync(action, isolationLevel, cancellationToken);
        }

        public static DatabaseTransaction BeginTransaction(IsolationLevel isolationLevel = IsolationLevel.ReadCommitted)
        {
            return Connection().BeginTransaction(isolationLevel);
        }

        public static Task<DatabaseTransaction> BeginTransactionAsync(IsolationLevel isolationLevel = IsolationLevel.ReadCommitted)
        {
            return Connection().BeginTransactionAsync(isolationLevel);
        }

        public static Task<DatabaseTransaction> BeginTransactionAsync(IsolationLevel isolationLevel, CancellationToken cancellationToken)
        {
            return Connection().BeginTransactionAsync(isolationLevel, cancellationToken);
        }



        /*
         * ============================================================
         * STORED PROCEDURE - DEFAULT CONNECTION
         * ============================================================
         */

        public static StoredProcedureBuilder Procedure(string procedureName)
        {
            return Connection()
                .Procedure(
                    procedureName
                );
        }


        /*
         * ============================================================
         * RAW - DEFAULT CONNECTION
         * ============================================================
         */

        public static RawCommandBuilder Raw(
            string sql,
            object? parameters = null)
        {
            return Connection()
                .Raw(
                    sql,
                    parameters
                );
        }

    }
}