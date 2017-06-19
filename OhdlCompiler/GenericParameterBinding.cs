using System;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace OhdlCompiler
{
	class GenericParameterBinding : ExpressionBinding
	{
		public MetaType MetaType { get; }
		public object Value { get; private set; }
		public Binding DefinedIn { get; }

		public GenericParameterBinding(TypeParameterSyntax syntax, Binding parent, object value, Binding definedIn) : base(syntax.Identifier.Text, parent)
		{
			Value = value;
			DefinedIn = definedIn;
			MetaType = MetaType.Get((syntax.MetaType as IdentifierNameSyntax)?.Identifier.Text);
		}

		public override void ResolveTypes(UsingState usingState)
		{
			ExpressionType = MetaType;
			if (Value is TypeSyntax t)
				Value = t.GetTypeBinding(usingState, DefinedIn ?? Parent);
			else if (Value is ExpressionSyntax e)
			{
				var eb = Create(e, DefinedIn);
				eb.ResolveTypes(usingState);
			}
		}

		public override object Evaluate()
		{
			if(Value is TypeBinding)
				throw new Exception("A type parameter cannot be used as an expression operand");
			if (Value is ExpressionBinding eb)
				Value = eb.Evaluate();
			return Value;
		}
	}
}