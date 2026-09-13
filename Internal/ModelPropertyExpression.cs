using DapperGlib.Exceptions;
using System.Linq.Expressions;
using System.Reflection;

namespace DapperGlib.Internal
{
    internal static class ModelPropertyExpression
    {
        internal static string GetName<TModel, TValue>(Expression<Func<TModel, TValue>> expression, string methodName)
        {
            if (expression == null) throw new ArgumentNullException(nameof(expression));

            Expression body = expression.Body;

            while (body is UnaryExpression unary && (unary.NodeType == ExpressionType.Convert || unary.NodeType == ExpressionType.ConvertChecked))
            {
                body = unary.Operand;
            }

            if (body is not MemberExpression member || member.Member is not PropertyInfo property)
            {
                throw new QueryBuilderException($"{methodName} requires a direct model property expression, for example 'x => x.UserId'.");
            }

            Expression? target = member.Expression;

            while (target is UnaryExpression targetUnary && (targetUnary.NodeType == ExpressionType.Convert || targetUnary.NodeType == ExpressionType.ConvertChecked))
            {
                target = targetUnary.Operand;
            }

            if (target != expression.Parameters[0])
            {
                throw new QueryBuilderException($"{methodName} requires a direct property of model '{typeof(TModel).Name}'. Nested property expressions are not supported.");
            }

            PropertyInfo? modelProperty = typeof(TModel).GetProperty(property.Name, BindingFlags.Instance | BindingFlags.Public);

            if (modelProperty == null)
            {
                throw new QueryBuilderException($"Property '{property.Name}' was not found on model '{typeof(TModel).Name}'.");
            }

            return modelProperty.Name;
        }
    }
}