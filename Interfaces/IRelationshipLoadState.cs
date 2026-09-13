namespace DapperGlib.Interfaces
{
    internal interface IRelationshipLoadState
    {
        bool IsRelationLoadedInternal(string relationshipName);
        void MarkRelationLoadedInternal(string relationshipName);
    }
}