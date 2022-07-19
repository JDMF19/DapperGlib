
namespace DapperGlib
{
    public class Relationship<TRelationship>: QueryBuilder<TRelationship>
    {
        public string LocalKey { get; set; }
        public string ForeignKey { get; set; }

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
