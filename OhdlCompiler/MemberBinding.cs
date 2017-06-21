using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace OhdlCompiler
{
	public enum MemberAccess
	{
		Private,
		Protected,
		Internal,
		ProtectedInternal,
		Public
	}

	public abstract class MemberBinding : ExpressionBinding
	{
		protected MemberBinding(SyntaxTokenList modifiers, string name, Binding parent) : base(name, parent)
		{
			var set = new HashSet<SyntaxKind>(modifiers.Select(m => m.Kind()));
			if(set.Contains(SyntaxKind.PublicKeyword))
				Access = MemberAccess.Public;
			else if(set.Contains(SyntaxKind.ProtectedKeyword) && set.Contains(SyntaxKind.InternalKeyword))
				Access = MemberAccess.ProtectedInternal;
			else if(set.Contains(SyntaxKind.ProtectedKeyword))
				Access = MemberAccess.Protected;
			else if(set.Contains(SyntaxKind.InternalKeyword))
				Access = MemberAccess.Internal;
			else
				Access = MemberAccess.Private;
			// TODO: Check for multiple/invalid modifiers
		}

		public uint? Offset { get; set; }

		public virtual ImmutableArray<string>? ExplicitInterface => null;

		public virtual MemberAccess Access { get; }

		public virtual IEnumerable<HardwareMember> ToHardwareDeclaration() => null;

		public virtual bool IsReg => false;
	}


	class FieldBinding : MemberBinding
	{
		public override bool IsReg { get; }

		private readonly FieldDeclarationSyntax syntax;

		public FieldBinding(FieldDeclarationSyntax syntax, int index, UserTypeBinding parent) : base(syntax.Modifiers, syntax.Declaration.Variables[index].Identifier.Text, parent)
		{
			this.syntax = syntax;
			var isReg = syntax.Modifiers.Any(m => m.Kind() == SyntaxKind.RegKeyword);
			IsReg = isReg;
			if(isReg && parent.ClassType != ClassType.Class)
				throw new Exception("Only classes can have 'reg' members");
		}

		public override void ResolveTypes(UsingState usingState)
		{
			ExpressionType = syntax.Declaration.Type.GetTypeBinding(usingState, this);
		}

		private HardwareRegister hwreg;
		public override HardwareExpression ToHardware()
		{
			if (ExpressionType.PrimitiveSize.HasValue)
			{
				return hwreg ?? (hwreg = new HardwareRegister
				{
					IsPublic = Access != MemberAccess.Private,
					Name = ImmutableArray.Create(Name),
					Size = ImmutableArray.Create(ExpressionType.PrimitiveSize.Value),
				});
			}
			throw new Exception("Expression operands must not be module instances");
        }
	}

	class PropertyBinding : MemberBinding
	{
		private readonly PropertyDeclarationSyntax syntax;

		public AccessorBinding GetAccessor { get; }
		public AccessorBinding SetAccessor { get; }
		public bool IsAuto => GetAccessor?.IsAuto == true && SetAccessor?.IsAuto != false;

		public bool CanRead => GetAccessor != null;
		public bool CanWrite => SetAccessor != null || GetAccessor?.IsAuto == true;

		public PropertyBinding(PropertyDeclarationSyntax syntax, Binding parent) : base(syntax.Modifiers, syntax.Identifier.Text, parent)
		{
			this.syntax = syntax;
			if (syntax.ExpressionBody != null)
			{
				GetAccessor = new AccessorBinding(syntax.ExpressionBody, this);
			}
			else
			{
				var getSyntax = syntax.AccessorList.Accessors.FirstOrDefault(a => a.Keyword.Kind() == SyntaxKind.GetKeyword);
				var setSyntax = syntax.AccessorList.Accessors.FirstOrDefault(a => a.Keyword.Kind() == SyntaxKind.SetKeyword);
				GetAccessor = getSyntax == null ? null : new AccessorBinding(getSyntax, this);
				SetAccessor = getSyntax == null ? null : new AccessorBinding(setSyntax, this);
            }
		}

		public override void ResolveTypes(UsingState usingState)
		{
			ExpressionType = syntax.Type.GetTypeBinding(usingState, this);
		}
	}

	class MethodGroupBinding : MemberBinding
	{
		public MethodGroupBinding(string name, Binding parent) : base(default(SyntaxTokenList), name, parent) { }

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

		public override void ResolveInterface()
		{
			ExpressionBody?.ResolveInterface();
			Body?.ResolveInterface();
		}
		
	}

	class AccessorBinding : MethodBindingBase
	{
		private readonly AccessorDeclarationSyntax syntax;
		private readonly ExpressionSyntax expressionBody;

		public bool IsAuto => syntax.Body == null && syntax.ExpressionBody == null;

		public AccessorBinding(AccessorDeclarationSyntax syntax, Binding parent) :
			base($"{syntax.Keyword.Text}_{parent.Name}", syntax.Body, syntax.ExpressionBody, parent)
		{
			this.syntax = syntax;
		}

		public AccessorBinding(ArrowExpressionClauseSyntax syntax, Binding parent) :
			base("get", null, syntax, parent)
		{
			expressionBody = syntax.Expression;
		}

		public override void ResolveTypes(UsingState usingState)
		{
			if (expressionBody != null)
			{
				ReturnType = ((PropertyDeclarationSyntax)expressionBody.Parent.Parent).Type.GetTypeBinding(usingState, this);
				return;
			}
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

		public override void ResolveInterface()
		{
			Trigger.ResolveInterface();
			Body.ResolveInterface();
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