
namespace DapperGlib.Util
{
    public enum LogicalOperators
    {
        AND, OR, IN, LIKE, NOT, DATE, MONTH, YEAR, DAY, BETWEEN, NOT_BETWEEN, DATEBETWEEN, NOT_IN, COLUMN
    }

    public enum Clauses
    {
        WHERE, EXISTS, NOT_EXISTS, TOP, OFFSET, DISTINCT, ORDER_BY, GROUP_BY, DELETE
    }
}
