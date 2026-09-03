using System.Threading;
using System.Threading.Tasks;

namespace DapperGlib.Interfaces
{
    public interface IModel
    {
        void Insert();

        Task InsertAsync();

        Task InsertAsync(CancellationToken cancellationToken);
    }
}