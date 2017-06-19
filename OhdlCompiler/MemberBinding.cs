using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace OhdlCompiler
{
	abstract class MemberBinding : ExpressionBinding
	{
		protected MemberBinding(string name, Binding parent) : base(name, parent)
		{
		}

		public virtual ImmutableArray<string>? ExplicitInterface => null;

	}


	class FieldBinding : MemberBinding
	{
		private readonly FieldDeclarationSyntax syntax;

		public FieldBinding(FieldDeclarationSyntax syntax, int index, Binding parent) : base(syntax.Declaration.Variables[index].Identifier.Text, parent)
		{
			this.syntax = syntax;
		}

		public override void ResolveTypes(UsingState usingState)
		{
			ExpressionType = syntax.Declaration.Type.GetTypeBinding(usingState, this);
		}
	}

	class PropertyBinding : MemberBinding
	{
		private readonly PropertyDeclarationSyntax syntax;

		public MethodBindingBase GetAccessor { get; }
		public MethodBindingBase SetAccessor { get; }

		public PropertyBinding(PropertyDeclarationSyntax syntax, Binding parent) : base(syntax.Identifier.Text, parent)
		{
			this.syntax = syntax;
			var getSyntax = syntax.AccessorList.Accessors.FirstOrDefault(a => a.Keyword.Kind() == SyntaxKind.GetKeyword);
			var setSyntax = syntax.AccessorList.Accessors.FirstOrDefault(a => a.Keyword.Kind() == SyntaxKind.SetKeyword);
			GetAccessor = getSyntax == null ? null : new AccessorBinding(getSyntax, this);
			SetAccessor = getSyntax == null ? null : new AccessorBinding(setSyntax, this);
		}

		public override void ResolveTypes(UsingState usingState)
		{
			ExpressionType = syntax.Type.GetTypeBinding(usingState, this);
		}
	}

	class MethodGroupBinding : MemberBinding
	{
		public MethodGroupBinding(string name, Binding parent) : base(name, parent) { }

		protected readonly List<MethodBinding> Methods = new List<MethodBinding>();

		public void AddMethod(BaseMethodDeclarationSyntax syntax)
		{
			Methods.Add(new MethodBinding(this, syntax));
		}

		public override void ResolveTypes(UsingState usingState)
		{
			foreach (var m in Methods)
				m.ResolveTypes(usingState);
		}
	}

	abstract class MethodBindingBase : Binding
	{
		public ExpressionBodyBinding ExpressionBody { get; }
		public StatementBinding Body { get; }

		private readonly Dictionary<string, ParameterBinding> parameters =
			new Dictionary<string, ParameterBinding>();

		public override Binding GetChild(string name)
		{
			parameters.TryGetValue(name, out var ret);
			return ret ?? Parent?.GetChild(name);
		}

		public TypeBinding ReturnType { get; protected set; }

		protected MethodBindingBase(string name, StatementSyntax body, ArrowExpressionClauseSyntax expressionBody, Binding parent) : base(name, parent)
		{
			ExpressionBody = expressionBody == null ? null : new ExpressionBodyBinding(expressionBody, this);
			Body = body == null ? null : StatementBinding.Create(body, this);
		}

		protected override void AddChild(Binding child)
		{
			if (child is ParameterBinding param)
				parameters[param.Name] = param;
		}

		public override void ResolveExpressions(UsingState usingState)
		{
			ExpressionBody?.ResolveTypes(usingState);
			Body?.ResolveTypes(usingState);
		}

		public override void ResolveArraySizes()
		{
			ExpressionBody?.ResolveArraySizes();
			Body?.ResolveArraySizes();
		}
	}

	class AccessorBinding : MethodBindingBase
	{
		private readonly AccessorDeclarationSyntax syntax;

		public bool IsAuto => syntax.Body == null && syntax.ExpressionBody == null;

		public AccessorBinding(AccessorDeclarationSyntax syntax, Binding parent) :
			base(syntax.Keyword.Text, syntax.Body, syntax.ExpressionBody, parent)
		{
			this.syntax = syntax;
		}

		public override void ResolveTypes(UsingState usingState)
		{
			switch (syntax.Keyword.Kind())
			{
				case SyntaxKind.GetKeyword:
					ReturnType = ((PropertyDeclarationSyntax)syntax.Parent.Parent).Type.GetTypeBinding(usingState, this);
					break;
				case SyntaxKind.SetKeyword:
					ParameterBinding.CreateValueParameter(this);
					break;
				default:
					throw new Exception($"Invalid accessor '{syntax.Keyword.Text}'");
			}
		}
	}

	class MethodBinding : MethodBindingBase
	{
		private readonly BaseMethodDeclarationSyntax syntax;

		public MethodBinding(MethodGroupBinding parent, BaseMethodDeclarationSyntax syntax) : base(parent.Name, syntax.Body, syntax.ExpressionBody, parent)
		{
			this.syntax = syntax;
		}

		public override void ResolveTypes(UsingState usingState)
		{
			if (syntax is MethodDeclarationSyntax method)
				ReturnType = method.ReturnType.GetTypeBinding(usingState, this);
			foreach (var p in syntax.ParameterList.Parameters)
				new ParameterBinding(p, this);
		}
	}

	class EventBinding : Binding
	{
		public ExpressionBinding Trigger { get; }
		public StatementBinding Body { get; }

		public EventBinding(EventDeclarationSyntax syntax, Binding parent) : base("$event", parent)
		{
			Trigger = ExpressionBinding.Create(syntax.TriggerExpression, this);
			if (syntax.ExpressionBody != null)
				Body = new ExpressionBodyBinding(syntax.ExpressionBody, this);
			else
				Body = new BlockBinding(syntax.Body, this);
		}

		public override void ResolveTypes(UsingState usingState)
		{
			// None
		}

		public HardwareEvent ToHardware()
		{
			return new HardwareEvent
			{
				Invert = false,
				//Trigger = Trigger.ToHardware(),
				//Statement = Body.ToHardware(),
			};

		}

		public override Binding GetChild(string name)
		{
			return Parent?.GetChild(name);
		}

		protected override void AddChild(Binding child)
		{
			// None
		}

		public override void ResolveExpressions(UsingState usingState)
		{
			Trigger.ResolveTypes(usingState);
			Body.ResolveTypes(usingState);
		}

		public override void ResolveArraySizes()
		{
			Trigger.ResolveArraySizes();
			Body.ResolveArraySizes();
		}
	}

	class IndexExpressionBinding : ExpressionBinding
	{
		public ExpressionBinding Array { get; }
		public ExpressionBinding Index { get; }

		public IndexExpressionBinding(ElementAccessExpressionSyntax syntax, Binding parent) : base(null, parent)
		{
			Array = Create(syntax.Expression, this);
			Index = Create(syntax.ArgumentList.Arguments[0].Expression, this);
		}

		public override void ResolveTypes(UsingState usingState)
		{
			Array.ResolveTypes(usingState);
			Index.ResolveTypes(usingState);
			ExpressionType = Array.ExpressionType.GetElementType(Index.ExpressionType);
		}
	}
}