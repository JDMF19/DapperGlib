
namespace DapperGlib
{
    public class Relationship<TRelationship>: QueryBuilder<TRelationship>
    {
        internal string LocalKey { get; set; }
        internal string ForeignKey { get; set; }

        public Relationship(string localKey, string? foreignKey = null)
        {
            if (foreignKey == null)
            {
                foreignKey = localKey;
            }

            LocalKey = localKey;
            ForeignKey = foreignKey;
        }


    }
}
