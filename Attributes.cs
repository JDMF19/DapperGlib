namespace DapperGlib
{
    [AttributeUsage(AttributeTargets.Property)]
    public class PrimaryKey : Attribute
    {
    }

    [AttributeUsage(AttributeTargets.Property)]
    public class Schema : Attribute
    {
    }

    [AttributeUsage(AttributeTargets.Property)]
    public class TableName : Attribute
    {
    }

    [AttributeUsage(AttributeTargets.Property)]
    public class Fillable : Attribute
    {
        
    }

}