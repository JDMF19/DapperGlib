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

            foreach(var item in conecciones)
            {
                Connections.Add(item.Key, item.Value);
            }

           /* _coneccionString = configuration.GetConnectionString("SqlConnection");*/


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
