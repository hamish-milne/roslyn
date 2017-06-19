using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace OhdlCompiler
{


	enum ClassType
	{
		Class,
		Struct,
		Interface,
		Enum,
	}

    class ClassBinding : TypeBinding, ITypeProvider
	{
		private readonly UsingState usingState;
		private readonly TypeDeclarationSyntax syntax;
		private readonly Binding genericContext;

		private readonly Dictionary<string, Binding> children
			= new Dictionary<string, Binding>();
		public ClassBinding BaseClass { get; private set; }

		public virtual ITypeProvider GetType(TypeName name, UsingState state, bool traverseUp, ImmutableArray<object> genericArgs, Binding context)
		{
			if (name == null)
			{
				if (genericArgs.IsDefault) return this;
				return MakeGeneric(genericArgs, context);
			}
			// Look in aliases (highest priority)
			var alias = state.Aliases.IsDefault ? null : state.Aliases.FirstOrDefault(p => p.Key == name.Name).Value;
			ITypeProvider ret = null;
			if (alias != null)
			{
				var root = (ITypeProvider)GetRoot();
				ret = root.GetType(TypeName.Append(alias, name.Next), state, false, name.GenericParameters, context);
			}
			// Remove aliases now, as if we didn't use them just now there's no point trying to in future
			state.Aliases = state.Aliases.Clear();

			// Look directly
			if (ret != null)
			{
				children.TryGetValue(name.Name, out var direct);
				ret = (direct as ITypeProvider)?.GetType(name.Next, state, false, name.GenericParameters, context);
			}

			// Look in base class
			ret = ret ?? BaseClass?.GetType(name, state, false, genericArgs, context);
            
			// Traverse up, if allowed
			if (ret == null && traverseUp) ret = (Parent as ITypeProvider)?.GetType(name, state, true, genericArgs, context);
			return ret;
		}

		public int OpenGenericArgs { get; }

		public ITypeProvider MakeGeneric(ImmutableArray<object> genericArgs, Binding genericDefinedIn)
		{
			// TODO: Cache these
			if (OpenGenericArgs == 0) throw new Exception("Attempted to add generic args to a concrete generic instance");
			return new ClassBinding(syntax, usingState, Parent, genericArgs, genericDefinedIn);
		}

		protected override void AddChild(Binding child)
		{
			if(OpenGenericArgs > 0) throw new Exception("Cannot add child to open generic class");
			if (child is EventBinding e)
			{
				Events.Add(e);
			}
			else if (child.Name != null)
			{
				if (children.ContainsKey(child.Name))
					throw new Exception($"Type {Name} already contains member {child.Name}");
				children[child.Name] = child;
			}
		}

		public override Binding GetChild(string name)
		{
			children.TryGetValue(name, out var ret);
			return ret ?? BaseClass?.GetChild(name);
		}

		private bool typesResolved, expressionsResolved, arraySizesResolved;

		public override void ResolveTypes(UsingState unused)
		{
			if (typesResolved) return;
			typesResolved = true;

			if (syntax.BaseList != null)
			{
				var baseList = syntax.BaseList.Types;
				if (baseList.Count > 0)
				{
					var baseName = TypeName.Create(baseList[0].Type as NameSyntax);
					if (baseName == null) throw new Exception("Invalid syntax");
					BaseClass = GetType(baseName, usingState, true, default(ImmutableArray<object>), genericContext) as ClassBinding;
					if (BaseClass == null) throw new Exception($"Unable to find base type {baseName}");
					if (BaseClass.ClassType != ClassType) throw new Exception($"Class type of {Name} must be {BaseClass.ClassType}");
					// TODO: Interfaces
				}
			}
			BaseClass?.ResolveTypes(usingState);
			foreach (var m in children.Values)
				m.ResolveTypes(usingState);
		}

		public override void ResolveExpressions(UsingState unused)
		{
			if (expressionsResolved) return;
			expressionsResolved = true;

			BaseClass?.ResolveExpressions(usingState);
			foreach(var m in children.Values)
				m.ResolveExpressions(usingState);
		}

		public override void ResolveArraySizes()
		{
			if (arraySizesResolved) return;
			arraySizesResolved = true;

			BaseClass?.ResolveArraySizes();
			foreach (var m in children.Values)
				m.ResolveArraySizes();
		}

		protected readonly List<EventBinding> Events = new List<EventBinding>();

		public ClassType ClassType { get; }
		public MethodGroupBinding Constructor { get; }

		public ClassBinding(TypeDeclarationSyntax syntax, UsingState usingState, Binding parent, ImmutableArray<object> genericArgs, Binding genericContext) : base(genericArgs.IsDefault ?  syntax.Identifier.Text : null, parent)
		{
			this.syntax = syntax;
			this.genericContext = genericContext;

			if (!genericArgs.IsDefault && genericArgs.Length != syntax.TypeParameterList?.Parameters.Count)
				throw new Exception($"Incorrect number of generic arguments; we need {syntax.TypeParameterList?.Parameters.Count}");

			// Only add members if we are non-generic or a closed generic
			if (!genericArgs.IsDefault || syntax.TypeParameterList == null)
			{
				if (syntax.TypeParameterList != null)
				{
					var genericParams = ImmutableArray.Create(syntax.TypeParameterList.Parameters
						.Select((p, i) => new GenericParameterBinding(p, this, genericArgs[i], genericContext)).ToArray());
				}
				// GenericArgs = genericArgs;
				Constructor = new MethodGroupBinding(".ctor", this);
				this.usingState = usingState;
				ClassType = GetClassType(syntax.Keyword);
				this.syntax = syntax;
				foreach (var m in syntax.Members)
					AddMember(m);
			}
			else
			{
				OpenGenericArgs = syntax.TypeParameterList.Parameters.Count;
			}
		}

		private static ClassType GetClassType(SyntaxToken keyword)
		{
			switch (keyword.Text)
			{
				case "class":
					return ClassType.Class;
				case "struct":
					return ClassType.Struct;
				case "interface":
					return ClassType.Interface;
				case "enum":
					return ClassType.Enum;
				default:
					throw new Exception($"Invalid type declaration keyword {keyword}");
			}
		}

		protected void AddMember(MemberDeclarationSyntax member)
		{
			switch (member)
			{
				case TypeDeclarationSyntax type:
					new ClassBinding(type, usingState, this, default(ImmutableArray<object>), this);
					break;
				case FieldDeclarationSyntax field:
					for (int i = 0, l = field.Declaration.Variables.Count; i < l; i++)
						new FieldBinding(field, i, this);
					break;
				case MethodDeclarationSyntax method:
					var mName = method.Identifier.Text;
					if (!children.TryGetValue(mName, out var methodGroup))
						methodGroup = new MethodGroupBinding(mName, this);
					((MethodGroupBinding)methodGroup).AddMethod(method);
					break;
				case ConstructorDeclarationSyntax ctor:
					Constructor.AddMethod(ctor);
					break;
				case PropertyDeclarationSyntax property:
					new PropertyBinding(property, this);
					break;
				case EventDeclarationSyntax sEvent:
					new EventBinding(sEvent, this);
					break;
				default:
					throw new Exception($"Invalid syntax {member}");
			}
		}

		public override TypeBinding GetElementType(TypeBinding indexer)
		{
			// TODO: Use indexer definition
			throw new NotImplementedException();
		}
	}
}