using System;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace OhdlCompiler
{
	public abstract class ExpressionBinding : Binding
	{
		protected ExpressionBinding(string name, Binding parent) : base(name, parent)
		{
		}

		public virtual TypeBinding ExpressionType { get; protected set; }

		public static ExpressionBinding Create(ExpressionSyntax syntax, Binding parent)
		{
			switch (syntax)
			{
				case BinaryExpressionSyntax b:
					return new BinaryExpressionBinding(b, parent);
				case PrefixUnaryExpressionSyntax u:
					return new UnaryExpressionBinding(u, parent);
				case CastExpressionSyntax c:
					return new CastExpressionBinding(c, parent);
				case PostfixUnaryExpressionSyntax pu:
					throw new NotImplementedException();
				case LiteralExpressionSyntax l:
					return new ConstantBinding(l, parent);
				case ParenthesizedExpressionSyntax p:
					return Create(p.Expression, parent);
				case MemberAccessExpressionSyntax m:
					return new MemberAccessBinding(m, parent);
				case AssignmentExpressionSyntax a:
					return new AssignmentBinding(a, parent);
				case SimpleNameSyntax n:
					return (ExpressionBinding) parent.GetChild(n.Identifier.Text) ??
					       throw new Exception($"Unknown name {n.Identifier.Text}!");
				case ConditionalExpressionSyntax con:
					return new ConditionalExpressionBinding(con, parent);
				default:
					throw new Exception("Invalid syntax");
			}
		}

		public override Binding GetChild(string name)
		{
			return Parent?.GetChild(name);
		}

		protected override void AddChild(Binding child)
		{
			// None - expression children are not accessed by name
		}

		public override void ResolveExpressions(UsingState usingState)
		{
			// None
		}

		public override void ResolveInterface()
		{
			ExpressionType?.ResolveInterface();
		}

		public virtual object Evaluate()
		{
			throw new Exception($"An expression {this} cannot be evaluated at compile-time");
		}

		public virtual HardwareExpression ToHardware() => null;
	}


	class ConditionalExpressionBinding : ExpressionBinding
	{
		public ExpressionBinding Condition { get; }
		public ExpressionBinding IfTrue { get; }
		public ExpressionBinding IfFalse { get; }

		public ConditionalExpressionBinding(ConditionalExpressionSyntax syntax, Binding parent) : base(null, parent)
		{
			Condition = Create(syntax.Condition, this);
			IfTrue = Create(syntax.WhenTrue, this);
			IfFalse = Create(syntax.WhenFalse, this);
		}

		public override void ResolveTypes(UsingState usingState)
		{
			Condition.ResolveTypes(usingState);
			IfTrue.ResolveTypes(usingState);
			IfFalse.ResolveTypes(usingState);
			// TODO: Better ternary type resolution
			ExpressionType = IfTrue.ExpressionType;
		}

		public override object Evaluate()
		{
			return Convert.ToBoolean(Condition.Evaluate()) ? IfTrue.Evaluate() : IfFalse.Evaluate();
		}

		public override HardwareExpression ToHardware()
		{
			return new HardwareBinary();
		}
	}

	class MemberAccessBinding : ExpressionBinding
	{
		public ExpressionBinding Expression { get; set; }
		public MemberBinding Member { get; set; }

		private readonly MemberAccessExpressionSyntax syntax;
		public MemberAccessBinding(MemberAccessExpressionSyntax syntax, Binding parent) : base(null, parent)
		{
			this.syntax = syntax;
			Expression = Create(syntax.Expression, this);
			Member = (MemberBinding)Expression.GetChild(syntax.Name.Identifier.Text);
		}

		public override void ResolveTypes(UsingState usingState)
		{
			Expression.ResolveTypes(usingState);
			if (Member == null)
				throw new Exception($"Member {syntax.Name.Identifier} not found in {Expression?.ExpressionType}!");
			ExpressionType = Member.ExpressionType;
		}
	}

	class ParameterBinding : ExpressionBinding
	{
		private readonly ParameterSyntax syntax;

		public ParameterBinding(ParameterSyntax syntax, Binding parent) : base(syntax.Identifier.Text, parent)
		{
			this.syntax = syntax;
		}

		private ParameterBinding(string name, Binding parent) : base(name, parent) { }

		public static ParameterBinding CreateValueParameter(Binding parent)
		{
			return new ParameterBinding("value", parent);
		}

		public override void ResolveTypes(UsingState usingState)
		{
			ExpressionType = ExpressionType ?? syntax.Type.GetTypeBinding(usingState, this);
		}
	}

	class ConstantBinding : ExpressionBinding
	{
		public object Value { get; }

		public ConstantBinding(LiteralExpressionSyntax syntax, Binding parent) : base(null, parent)
		{
			Value = syntax.Token.Value;
		}

		public ConstantBinding(object value, Binding parent) : base(null, parent)
		{
			Value = value;
		}

		public override void ResolveTypes(UsingState usingState)
		{
			switch (Value)
			{
				case float _:
				case double _:
					ExpressionType = MetaType.Get("FLOAT");
					break;
				case int _:
				case long _:
				case uint _:
				case ulong _:
				case short _:
				case ushort _:
				case byte _:
				case sbyte _:
					ExpressionType = MetaType.Get("INT");
					break;
				case string _:
					ExpressionType = MetaType.Get("STRING");
					break;
				case bool _:
					ExpressionType = MetaType.Get("BOOL");
					break;
			}
		}

		public override object Evaluate()
		{
			return Value;
		}
	}

	class CastExpressionBinding : ExpressionBinding
	{
		private readonly CastExpressionSyntax syntax;

		public ExpressionBinding Operand { get; }

		public CastExpressionBinding(CastExpressionSyntax syntax, Binding parent) : base(null, parent)
		{
			this.syntax = syntax;
			Operand = Create(syntax.Expression, this);
		}

		public override void ResolveTypes(UsingState usingState)
		{
			ExpressionType = syntax.Type.GetTypeBinding(usingState, this);
		}

		public override object Evaluate()
		{
			var v = Operand.Evaluate();
			var mt = (ExpressionType as MetaType)?.Id;
			switch (mt)
			{
				case MetaTypeId.Float:
					return Convert.ToDouble(v);
				case MetaTypeId.Int:
					return Convert.ToInt64(v);
				case MetaTypeId.String:
					return v.ToString();
				case MetaTypeId.Type:
					throw new Exception("Can't cast to TYPE");
				case MetaTypeId.Bool:
					return Convert.ToBoolean(v);
				default:
					throw new Exception("Non-meta values cannot be evaluated");
			}
		}
	}

	class AssignmentBinding : ExpressionBinding
	{
		private readonly AssignmentExpressionSyntax syntax;

		public ExpressionBinding Left { get; }
		public ExpressionBinding Right { get; }

		public AssignmentBinding(AssignmentExpressionSyntax syntax, Binding parent) : base(null, parent)
		{
			this.syntax = syntax;
			Left = Create(syntax.Left, this);
			Right = Create(syntax.Right, this);
		}

		public override void ResolveTypes(UsingState usingState)
		{
			Left.ResolveTypes(usingState);
			Right.ResolveTypes(usingState);
			ExpressionType = Left.ExpressionType;
		}
	}

	class BinaryExpressionBinding : ExpressionBinding
	{
		private readonly BinaryExpressionSyntax syntax;

		public BinaryExpressionBinding(BinaryExpressionSyntax syntax, Binding parent) : base(null, parent)
		{
			this.syntax = syntax;
			Left = Create(syntax.Left, this);
			Right = Create(syntax.Right, this);
		}

		public ExpressionBinding Left { get; }
		public ExpressionBinding Right { get; }

		public override void ResolveTypes(UsingState usingState)
		{
			Left.ResolveTypes(usingState);
			Right.ResolveTypes(usingState);
			// TODO: Operator methods
			ExpressionType = Left.ExpressionType;
		}
	}

	class UnaryExpressionBinding : ExpressionBinding
	{
		private readonly PrefixUnaryExpressionSyntax syntax;

		public UnaryExpressionBinding(PrefixUnaryExpressionSyntax syntax, Binding parent) : base(null, parent)
		{
			this.syntax = syntax;
			Operand = Create(syntax.Operand, this);
		}

		public ExpressionBinding Operand { get; }

		public override void ResolveTypes(UsingState usingState)
		{
			Operand.ResolveTypes(usingState);
			// TODO: Operator methods
			ExpressionType = Operand.ExpressionType;
		}
	}
}