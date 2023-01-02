
namespace DapperGlib.Util
{
    internal enum LogicalOperators
    {
        AND, OR, IN, LIKE, NOT, DATE, WHEREYEAR, WHEREMONTH, WHEREDAY, ORWHEREYEAR, ORWHEREMONTH, ORWHEREDAY, YEAR, MONTH, DAY, MINUTE, BETWEEN, NOT_BETWEEN, DATEBETWEEN, NOT_IN, COLUMN
    }

    public enum Clauses
    {
        WHERE, EXISTS, NOT_EXISTS, TOP, OFFSET, DISTINCT, ORDER_BY, GROUP_BY, HAVING, DELETE, TRUNCATE
    }

    public enum DateDiff
    {
        YEAR, MONTH, DAY, MINUTE
    }
}
