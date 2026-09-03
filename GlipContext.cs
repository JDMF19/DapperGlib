using Microsoft.Data.SqlClient;
using Microsoft.Extensions.Configuration;
using System.Data;
using System.Reflection;
using System.IO;
using System.Collections.Generic;
using System.Collections;

namespace DapperGlib
{
    public class GlipContext
    {

        //  private List<string> _coneccions { get; set; } = new();

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

            if (!Connections.ContainsKey("SqlConnection"))
            {
                throw new ArgumentException($"Key 'SqlConnection' not found on ConnectionStrings ");
            }

            var conectionString = Connections["SqlConnection"];

            return new SqlConnection(conectionString);
        }

        public IDbConnection CreateConnection(string ConnectionKey)
        {

            if (!Connections.ContainsKey(ConnectionKey))
            {
                throw new ArgumentException($"Key '{ConnectionKey}' not found on ConnectionStrings ");
            }

            var conectionString = Connections[ConnectionKey];

            return new SqlConnection(conectionString);
        }
    }
}
