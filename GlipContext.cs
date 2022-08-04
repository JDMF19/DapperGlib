using Microsoft.Data.SqlClient;
using Microsoft.Extensions.Configuration;
using System.Data;
using System.Reflection;
using System.IO;

namespace DapperGlib
{
    public class GlipContext
    {
        private readonly string _coneccionString;

        public GlipContext()
        {
            var path = Path.GetDirectoryName(Assembly.GetEntryAssembly()!.Location);

            string real_path = "";
            if (File.Exists($"{path}/../../../appsettings.json"))
            {
                real_path = $"{path}/../../../";
            }
            else
            {
                real_path = path!;
            }

            IConfigurationRoot configuration = new ConfigurationBuilder()
                                          .SetBasePath($"{real_path}")
                                          .AddJsonFile("appsettings.json")
                                          .Build();

            _coneccionString = configuration.GetConnectionString("SqlConnection");

        }

        public IDbConnection CreateConnection() => new SqlConnection(_coneccionString);
    }
}
