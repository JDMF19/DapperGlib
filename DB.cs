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