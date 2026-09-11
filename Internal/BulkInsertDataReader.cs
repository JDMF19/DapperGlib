using System.Collections;
using System.Data.Common;
using System.Globalization;
using System.Reflection;

namespace DapperGlib.Internal
{
    internal sealed class BulkInsertDataReader<T> : DbDataReader
    {
        private readonly IReadOnlyList<T> _items;
        private readonly IReadOnlyList<PropertyInfo> _properties;

        private int _rowIndex = -1;
        private bool _closed;

        internal BulkInsertDataReader(IReadOnlyList<T> items, IReadOnlyList<PropertyInfo> properties)
        {
            _items = items ?? throw new ArgumentNullException(nameof(items));
            _properties = properties ?? throw new ArgumentNullException(nameof(properties));
        }

        public override int FieldCount => _properties.Count;
        public override bool HasRows => _items.Count > 0;
        public override bool IsClosed => _closed;
        public override int RecordsAffected => -1;
        public override int Depth => 0;

        public override object this[int ordinal] => GetValue(ordinal);
        public override object this[string name] => GetValue(GetOrdinal(name));

        public override bool Read()
        {
            EnsureOpen();

            if (_rowIndex + 1 >= _items.Count)
            {
                return false;
            }

            _rowIndex++;

            return true;
        }

        public override bool NextResult()
        {
            return false;
        }

        public override string GetName(int ordinal)
        {
            ValidateOrdinal(ordinal);

            return _properties[ordinal].Name;
        }

        public override int GetOrdinal(string name)
        {
            if (string.IsNullOrWhiteSpace(name))
            {
                throw new ArgumentNullException(nameof(name));
            }

            for (int i = 0; i < _properties.Count; i++)
            {
                if (string.Equals(_properties[i].Name, name, StringComparison.OrdinalIgnoreCase))
                {
                    return i;
                }
            }

            throw new IndexOutOfRangeException($"Column '{name}' was not found in the bulk insert reader.");
        }

        public override Type GetFieldType(int ordinal)
        {
            ValidateOrdinal(ordinal);

            Type type = Nullable.GetUnderlyingType(_properties[ordinal].PropertyType) ?? _properties[ordinal].PropertyType;

            if (type.IsEnum)
            {
                return Enum.GetUnderlyingType(type);
            }

            if (type == typeof(char))
            {
                return typeof(string);
            }

            return type;
        }

        public override string GetDataTypeName(int ordinal)
        {
            return GetFieldType(ordinal).Name;
        }

        public override object GetValue(int ordinal)
        {
            EnsureCurrentRow();
            ValidateOrdinal(ordinal);

            object? value = _properties[ordinal].GetValue(_items[_rowIndex]);

            if (value == null)
            {
                return DBNull.Value;
            }

            Type valueType = value.GetType();

            if (valueType.IsEnum)
            {
                Type underlyingType = Enum.GetUnderlyingType(valueType);
                return Convert.ChangeType(value, underlyingType, CultureInfo.InvariantCulture);
            }

            if (value is char character)
            {
                return character.ToString();
            }

            return value;
        }

        public override int GetValues(object[] values)
        {
            if (values == null)
            {
                throw new ArgumentNullException(nameof(values));
            }

            int count = Math.Min(values.Length, FieldCount);

            for (int i = 0; i < count; i++)
            {
                values[i] = GetValue(i);
            }

            return count;
        }

        public override bool IsDBNull(int ordinal)
        {
            return GetValue(ordinal) == DBNull.Value;
        }

        public override bool GetBoolean(int ordinal)
        {
            return Convert.ToBoolean(GetValue(ordinal), CultureInfo.InvariantCulture);
        }

        public override byte GetByte(int ordinal)
        {
            return Convert.ToByte(GetValue(ordinal), CultureInfo.InvariantCulture);
        }

        public override char GetChar(int ordinal)
        {
            return Convert.ToChar(GetValue(ordinal), CultureInfo.InvariantCulture);
        }

        public override DateTime GetDateTime(int ordinal)
        {
            return Convert.ToDateTime(GetValue(ordinal), CultureInfo.InvariantCulture);
        }

        public override decimal GetDecimal(int ordinal)
        {
            return Convert.ToDecimal(GetValue(ordinal), CultureInfo.InvariantCulture);
        }

        public override double GetDouble(int ordinal)
        {
            return Convert.ToDouble(GetValue(ordinal), CultureInfo.InvariantCulture);
        }

        public override float GetFloat(int ordinal)
        {
            return Convert.ToSingle(GetValue(ordinal), CultureInfo.InvariantCulture);
        }

        public override Guid GetGuid(int ordinal)
        {
            object value = GetValue(ordinal);

            return value is Guid guid ? guid : Guid.Parse(value.ToString()!);
        }

        public override short GetInt16(int ordinal)
        {
            return Convert.ToInt16(GetValue(ordinal), CultureInfo.InvariantCulture);
        }

        public override int GetInt32(int ordinal)
        {
            return Convert.ToInt32(GetValue(ordinal), CultureInfo.InvariantCulture);
        }

        public override long GetInt64(int ordinal)
        {
            return Convert.ToInt64(GetValue(ordinal), CultureInfo.InvariantCulture);
        }

        public override string GetString(int ordinal)
        {
            return Convert.ToString(GetValue(ordinal), CultureInfo.InvariantCulture)!;
        }

        public override long GetBytes(int ordinal, long dataOffset, byte[]? buffer, int bufferOffset, int length)
        {
            byte[] data = (byte[])GetValue(ordinal);

            if (buffer == null)
            {
                return data.Length;
            }

            if (dataOffset >= data.Length)
            {
                return 0;
            }

            int available = data.Length - (int)dataOffset;
            int count = Math.Min(length, available);

            Array.Copy(data, dataOffset, buffer, bufferOffset, count);

            return count;
        }

        public override long GetChars(int ordinal, long dataOffset, char[]? buffer, int bufferOffset, int length)
        {
            char[] data = GetString(ordinal).ToCharArray();

            if (buffer == null)
            {
                return data.Length;
            }

            if (dataOffset >= data.Length)
            {
                return 0;
            }

            int available = data.Length - (int)dataOffset;
            int count = Math.Min(length, available);

            Array.Copy(data, dataOffset, buffer, bufferOffset, count);

            return count;
        }

        public override IEnumerator GetEnumerator()
        {
            return _items.GetEnumerator();
        }

        public override void Close()
        {
            _closed = true;
        }

        private void EnsureOpen()
        {
            if (_closed)
            {
                throw new InvalidOperationException("The bulk insert data reader is closed.");
            }
        }

        private void EnsureCurrentRow()
        {
            EnsureOpen();

            if (_rowIndex < 0 || _rowIndex >= _items.Count)
            {
                throw new InvalidOperationException("The bulk insert data reader is not positioned on a valid row.");
            }
        }

        private void ValidateOrdinal(int ordinal)
        {
            if (ordinal < 0 || ordinal >= _properties.Count)
            {
                throw new IndexOutOfRangeException($"Column ordinal '{ordinal}' is outside the valid range.");
            }
        }
    }
}