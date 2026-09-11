namespace DapperGlib.Internal
{
    internal sealed class QueryParameterContext
    {
        private int _counter = 0;

        internal Dictionary<string, object?> Values { get; } = new();

        internal string Add(object? value)
        {
            string name = $"__dglib_p{_counter++}";

            Values.Add(name, value);

            return $"@{name}";
        }

        internal QueryParameterContext Clone()
        {
            var clone = new QueryParameterContext
            {
                _counter = _counter
            };

            foreach (var parameter in Values)
            {
                clone.Values.Add(
                    parameter.Key,
                    parameter.Value
                );
            }

            return clone;
        }
    }
}