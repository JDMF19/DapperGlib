using Dapper;

namespace DapperGlib.Internal
{
    internal sealed class BulkInsertCommand
    {
        internal string Sql { get; }
        internal DynamicParameters Parameters { get; }
        internal bool ReturnsGeneratedKeys { get; }
        internal int RowCount { get; }

        internal BulkInsertCommand(string sql, DynamicParameters parameters, bool returnsGeneratedKeys, int rowCount)
        {
            Sql = sql ?? throw new ArgumentNullException(nameof(sql));
            Parameters = parameters ?? throw new ArgumentNullException(nameof(parameters));
            ReturnsGeneratedKeys = returnsGeneratedKeys;
            RowCount = rowCount;
        }
    }

    internal sealed class BulkInsertKeyResult
    {
        public int Index { get; set; }
        public object? Value { get; set; }
    }
}