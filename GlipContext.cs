using Microsoft.Data.SqlClient;
using Microsoft.Extensions.Configuration;
using System.Data;
using System.Reflection;

namespace DapperGlib
{
    public class GlipContext
    {

        public Dictionary<string, string> Connections = new();

        public int? CommandTimeout { get; private set; }

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

            var conecciones = configuration.GetSection("ConnectionStrings").GetChildren();

            foreach (var item in conecciones)
            {
                Connections.Add(item.Key, item.Value);
            }


            // Timeout global de DapperGlib
            string? commandTimeoutValue = configuration["DapperGlib:CommandTimeout"];

            if (!string.IsNullOrWhiteSpace(
                commandTimeoutValue))
            {
                if (!int.TryParse(
                    commandTimeoutValue,
                    out int commandTimeout))
                {
                    throw new InvalidOperationException(
                        "DapperGlib:CommandTimeout must be a valid integer."
                    );
                }

                if (commandTimeout <= 0)
                {
                    throw new InvalidOperationException(
                        "DapperGlib:CommandTimeout must be greater than zero."
                    );
                }

                CommandTimeout = commandTimeout;
            }


        }

        public IDbConnection CreateConnection()
        {
            return CreateSqlConnection("SqlConnection");
        }

        public IDbConnection CreateConnection(string ConnectionString)
        {
            return CreateSqlConnection(ConnectionString);
        }

        internal SqlConnection CreateSqlConnection(string ConnectionString)
        {
            return new SqlConnection(Connections[ConnectionString]);
        }

    }
}
