using Microsoft.Data.SqlClient;
using Microsoft.Extensions.Configuration;
using System.Data;
using System.Reflection;

namespace DapperGlib
{
    public class GlipContext
    {
        private readonly string _coneccionString;

        public GlipContext()
        {
            var path = Path.GetDirectoryName(Assembly.GetEntryAssembly()!.Location);
            
            IConfigurationRoot configuration = new ConfigurationBuilder()
                                          .SetBasePath($"{path}/../../../")
                                          .AddJsonFile("appsettings.json")
                                          .Build();

            _coneccionString = configuration.GetConnectionString("SqlConnection");

        }

        public IDbConnection CreateConnection() => new SqlConnection(_coneccionString);
    }
}
