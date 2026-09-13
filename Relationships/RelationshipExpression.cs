using DapperGlib.Exceptions;
using System.Linq.Expressions;
using System.Reflection;

namespace DapperGlib.Relationships
{
    internal static class RelationshipExpression
    {
        internal static PropertyInfo GetProperty(LambdaExpression expression, Type modelType)
        {
            if (expression == null)
            {
                throw new ArgumentNullException(nameof(expression));
            }

            if (modelType == null)
            {
                throw new ArgumentNullException(nameof(modelType));
            }

            Expression body = expression.Body;

            if (body is UnaryExpression unary && (unary.NodeType == ExpressionType.Convert || unary.NodeType == ExpressionType.ConvertChecked))
            {
                body = unary.Operand;
            }

            if (body is not MemberExpression member || member.Member is not PropertyInfo property)
            {
                throw new RelationshipException($"Relationship expression on model '{modelType.Name}' must point directly to a relationship property. Example: model => model.Comments.");
            }

            if (member.Expression is not ParameterExpression)
            {
                throw new RelationshipException($"Relationship expression '{property.Name}' on model '{modelType.Name}' must point directly to a property of the model.");
            }

            if (property.DeclaringType == null || !property.DeclaringType.IsAssignableFrom(modelType))
            {
                throw new RelationshipException($"Property '{property.Name}' does not belong to model '{modelType.Name}'.");
            }

            return property;
        }
    }
}