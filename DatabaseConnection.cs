using System.Threading;

namespace DapperGlib
{
    public sealed class DatabaseConnection
    {
        internal string ConnectionKey
        {
            get;
        }


        internal DatabaseConnection(
            string connectionKey)
        {
            if (string.IsNullOrWhiteSpace(
                connectionKey))
            {
                throw new ArgumentException(
                    "Connection key cannot be null or empty.",
                    nameof(connectionKey)
                );
            }

            ConnectionKey =
                connectionKey.Trim();
        }


        /*
         * ============================================================
         * STORED PROCEDURE
         * ============================================================
         */

        public StoredProcedureBuilder Procedure(
            string procedureName)
        {
            return new StoredProcedureBuilder(
                procedureName,
                ConnectionKey
            );
        }


        /*
         * ============================================================
         * RAW
         * ============================================================
         */

        public RawCommandBuilder Raw(string sql, object? parameters = null)
        {
            return new RawCommandBuilder(
                sql,
                ConnectionKey,
                parameters
            );
        }

        /*
         * ============================================================
         * QUERY
         * ============================================================
         */

        public List<TResult> Query<TResult>(string sql, object? parameters = null)
        {
            return Raw(
                sql,
                parameters
            )
            .Query<TResult>();
        }


        public Task<List<TResult>> QueryAsync<TResult>(string sql, object? parameters = null)
        {
            return Raw(
                sql,
                parameters
            )
            .QueryAsync<TResult>();
        }


        public Task<List<TResult>> QueryAsync<TResult>(string sql, object? parameters, CancellationToken cancellationToken)
        {
            return Raw(
                sql,
                parameters
            )
            .QueryAsync<TResult>(
                cancellationToken
            );
        }

        /*
         * ============================================================
         * EXECUTE
         * ============================================================
         */

        public int Execute(
            string sql,
            object? parameters = null)
        {
            return Raw(
                sql,
                parameters
            )
            .Execute();
        }


        public Task<int> ExecuteAsync(
            string sql,
            object? parameters = null)
        {
            return Raw(
                sql,
                parameters
            )
            .ExecuteAsync();
        }


        public Task<int> ExecuteAsync(
            string sql,
            object? parameters,
            CancellationToken cancellationToken)
        {
            return Raw(
                sql,
                parameters
            )
            .ExecuteAsync(
                cancellationToken
            );
        }


        /*
         * ============================================================
         * SCALAR
         * ============================================================
         */

        public TResult Scalar<TResult>(
            string sql,
            object? parameters = null)
        {
            return Raw(
                sql,
                parameters
            )
            .Scalar<TResult>();
        }


        public Task<TResult> ScalarAsync<TResult>(
            string sql,
            object? parameters = null)
        {
            return Raw(
                sql,
                parameters
            )
            .ScalarAsync<TResult>();
        }


        public Task<TResult> ScalarAsync<TResult>(
            string sql,
            object? parameters,
            CancellationToken cancellationToken)
        {
            return Raw(
                sql,
                parameters
            )
            .ScalarAsync<TResult>(
                cancellationToken
            );
        }

        /*
         * ============================================================
         * QUERY MULTIPLE
         * ============================================================
         */

        public MultipleResultReader QueryMultiple(
            string sql,
            object? parameters = null)
        {
            return Raw(
                sql,
                parameters
            )
            .QueryMultiple();
        }


        public Task<MultipleResultReader> QueryMultipleAsync(
            string sql,
            object? parameters = null)
        {
            return Raw(
                sql,
                parameters
            )
            .QueryMultipleAsync();
        }


        public Task<MultipleResultReader> QueryMultipleAsync(
            string sql,
            object? parameters,
            CancellationToken cancellationToken)
        {
            return Raw(
                sql,
                parameters
            )
            .QueryMultipleAsync(
                cancellationToken
            );
        }


    }
}