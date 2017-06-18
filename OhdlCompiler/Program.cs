using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.Immutable;
// ReSharper disable ObjectCreationAsStatement

namespace OhdlCompiler
{
	class Program
	{

		static void Main(string[] args)
		{
			var compiler = new Compiler();
            compiler.LoadFile(@"
namespace Sys {
    [Primitive]
    struct Bit { }
}

using Sys;
class Counter<INT N> {
	public bit[N] count { get; private set; }
	public bit reset;
	
	event (true) => count = reset ? 0 : count + 1;
}

class Main : Counter<8> { }
");

            compiler.ResolveTypes();

		}
	}


    /*
     * Get using contexts, set for members
     * Add type definitions
     * Resolve member types
     * Bind expressions
     * 
     * 
     * 
     * */

	struct UsingState
	{
		public ImmutableArray<TypeName> Usings { get; set; }
        public ImmutableArray<KeyValuePair<string, TypeName>> Aliases { get; set; }
	}

	abstract class Binding
	{
		public Binding Parent { get; }
        public string Name { get; }

		protected abstract void AddChild(Binding child);

		public abstract Binding GetChild(string name);

		public Binding GetRoot()
		{
			var c = this;
			while (c.Parent != null)
				c = c.Parent;
			return c;
		}

		public T GetParent<T>() where T : class
		{
			if ((object)this is T t) return t;
			return Parent?.GetParent<T>();
		}

        private ImmutableArray<string>? fullyQualifiedName;
		public virtual ImmutableArray<string> FullyQualifiedName
		{
			get
			{
				if (fullyQualifiedName.HasValue) return fullyQualifiedName.Value;
				if (Parent == null) fullyQualifiedName = ImmutableArray.Create(Name);
				else fullyQualifiedName = Name == null ? Parent.FullyQualifiedName : Parent.FullyQualifiedName.Add(Name);
				return fullyQualifiedName.Value;
			}
		}

        public abstract void ResolveTypes(UsingState usingState);
		public abstract void ResolveExpressions(UsingState usingState);
		public abstract void ResolveArraySizes();

		protected Binding(string name, Binding parent)
		{
			Name = name;
			Parent = parent;
            parent?.AddChild(this);
		}
    }

	class TypeName : IEquatable<TypeName>
	{
		public TypeName Next { get; }
        public string Name { get; }
        public ImmutableArray<object> GenericParameters { get; }

		private TypeName(SimpleNameSyntax syntax, TypeName next)
		{
			Name = syntax.Identifier.Text;
			if (syntax is GenericNameSyntax g)
			{
				GenericParameters = ImmutableArray.Create(g.TypeArgumentList.Arguments.Select(a =>
					{
						switch (a)
						{
                            case LiteralExpressionSyntax l:
	                            return l.Token.Value;
                            default:
	                            return a;
						}
					}).ToArray());
			}
			Next = next;
		}

		private TypeName(TypeName other, TypeName next)
		{
			Name = other.Name;
			GenericParameters = other.GenericParameters;
			Next = other.Next == null ? next : new TypeName(other.Next, next);
		}

		private TypeName(string name, TypeName next)
		{
			Name = name;
			Next = next;
		}

		public static TypeName Create(NameSyntax syntax, TypeName append = null)
		{
			if (syntax is QualifiedNameSyntax q)
			{
				return Create(q.Left, Create(q.Right, append));
			} else if (syntax is SimpleNameSyntax s)
			{
				return new TypeName(s, append);
			} else
                throw new Exception($"Invalid syntax {syntax}");
		}

		public static TypeName Create(params string[] elements)
		{
			if (elements == null || elements.Length == 0) return null;
			return new TypeName(elements[0], Create(elements.Skip(1).ToArray()));
		}

		public static TypeName Append(TypeName prefix, TypeName suffix)
		{
			if (suffix == null) return prefix;
			return new TypeName(prefix, suffix);
		}

		public bool Equals(TypeName other)
		{
			if (other == null) return false;
			return Name == other.Name && GenericParameters == other.GenericParameters && Next == other.Next;
		}

		public override bool Equals(object obj)
		{
			if (obj is TypeName other)
				return Equals(other);
			return false;
		}

		public override int GetHashCode()
		{
			return Name.GetHashCode() ^ GenericParameters.GetHashCode() ^ Next.GetHashCode();
		}

		public static bool operator ==(TypeName a, TypeName b)
		{
			if (ReferenceEquals(a, null) && ReferenceEquals(b, null)) return true;
			if (ReferenceEquals(a, null) || ReferenceEquals(b, null)) return false;
			return a.Equals(b);
		}

		public static bool operator !=(TypeName a, TypeName b)
		{
			return !(a == b);
		}
	}

	interface ITypeProvider
	{
		ITypeProvider GetType(TypeName name, UsingState state, bool traverseUp, ImmutableArray<object> genericArgs, Binding context);
	}

	enum MetaTypeId
	{
		Int,
        Float,
        String,
        Type,
        Bool,
	}

	class MetaType : TypeBinding
	{
        public MetaTypeId Id { get; }

		private MetaType(MetaTypeId id) : base(id.ToString().ToUpperInvariant(), null)
		{
			Id = id;
		}

        private static readonly Dictionary<string, MetaType> All
            = new Dictionary<string, MetaType>();

		static MetaType()
		{
			foreach (var v in Enum.GetValues(typeof(MetaTypeId)))
			{
				var t = new MetaType((MetaTypeId) v);
				All[t.Name] = t;
			}
		}

		public static MetaType Get(string name)
		{
			All.TryGetValue(name, out var ret);
			return ret;
		}

		protected override void AddChild(Binding child)
		{
			throw new NotImplementedException();
		}

		public override Binding GetChild(string name)
		{
			throw new NotImplementedException();
		}

		public override void ResolveTypes(UsingState usingState)
		{
			throw new NotImplementedException();
		}

		public override void ResolveExpressions(UsingState usingState)
		{
			throw new NotImplementedException();
		}

        public override TypeBinding GetElementType(TypeBinding indexer)
		{
			throw new NotImplementedException();
		}

		public override void ResolveArraySizes()
		{
			// None
		}
	}

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

	class NamespaceBinding : Binding, ITypeProvider
	{
		private readonly Dictionary<string, Binding> children
            = new Dictionary<string, Binding>();

		public override Binding GetChild(string name)
		{
			children.TryGetValue(name, out var ret);
			return ret;
		}

        public NamespaceBinding(string name, Binding parent) : base(name, parent)
		{
		}

		protected override void AddChild(Binding child)
		{
			if (child.Name != null)
			{
				if(children.ContainsKey(child.Name))
                    throw new Exception($"Namespace {Name} already contains member {child.Name}");
				children[child.Name] = child;
			}
		}

		public virtual ITypeProvider GetType(TypeName name, UsingState state, bool traverseUp, ImmutableArray<object> genericArgs, Binding context)
		{
			if (name == null)
			{
				if (!genericArgs.IsDefault) return null;
				return this;
			}
            // Look directly
            children.TryGetValue(name.Name, out var direct);
			var ret = (direct as ITypeProvider)?.GetType(name.Next, state, false, name.GenericParameters, context);

            // Look in using statements if we're the root
            if (ret == null && GetRoot() == this && !state.Usings.IsDefault)
			{
				foreach (var u in state.Usings)
				{
					ret = GetType(TypeName.Append(u, name), state, false, genericArgs, context);
					if (ret != null) break;
				}
			}

			if (ret == null && traverseUp) ret = (Parent as ITypeProvider)?.GetType(name, state, true, genericArgs, context);
			return ret;
		}

		public override void ResolveTypes(UsingState usingState)
		{
			foreach(var c in children.Values)
                c.ResolveTypes(usingState);
		}

		public override void ResolveExpressions(UsingState usingState)
		{
			foreach (var c in children.Values)
				c.ResolveExpressions(usingState);
		}

		public override void ResolveArraySizes()
		{
			foreach (var c in children.Values)
				c.ResolveArraySizes();
		}

        public NamespaceBinding GetChildNamespace(TypeName name)
		{
			if (name == null) return this;
			var child = GetChild(name.Name);
			if (child == null)
				child = new NamespaceBinding(name.Name, this);
			var ns = child as NamespaceBinding;
			if (ns == null)
				throw new Exception($"Type {name.Name} is already a namespace name");
			return ns.GetChildNamespace(name.Next);
		}

        public void Load(SyntaxList<MemberDeclarationSyntax> members, SyntaxList<UsingDirectiveSyntax> usings, UsingState state)
		{
			if (usings.Count > 0)
			{
				var usingsList = new HashSet<TypeName>();
				var aliasList = new Dictionary<string, TypeName>();
                if(!state.Usings.IsDefault)
	                foreach (var u in state.Usings)
		                usingsList.Add(u);
                if(!state.Aliases.IsDefault)
				    foreach (var pair in state.Aliases)
					    aliasList[pair.Key] = pair.Value;

				foreach (var uds in usings)
				{
					var alias = uds.Alias?.Name.Identifier.Text;
					var name = TypeName.Create(uds.Name);
					if (alias == null)
						usingsList.Add(name);
					else
						aliasList[alias] = name;
				}

                // Update state
				state = new UsingState
				{
					Aliases = ImmutableArray.Create(aliasList.ToArray()),
					Usings = ImmutableArray.Create(usingsList.ToArray())
				};
			}

            foreach (var m in members)
			{
				switch (m)
				{
					case NamespaceDeclarationSyntax ns:
						var newContext = GetChildNamespace(TypeName.Create(ns.Name));
						newContext.Load(ns.Members, ns.Usings, state);
						break;
					case TypeDeclarationSyntax type:
						new ClassBinding(type, state, this, default(ImmutableArray<object>), this);
						break;
					default:
						throw new Exception($"Invalid syntax {m}");
				}
			}
		}
    }

	static class PredefinedTypes
	{
		private static readonly Dictionary<string, TypeName> Map = new Dictionary<string, TypeName>
		{
			{"bit", TypeName.Create("Sys", "Bit")},
		};

		public static TypeName Find(PredefinedTypeSyntax syntax)
		{
			return Map[syntax.Keyword.Text];
		}

		public static TypeBinding GetTypeBinding(this TypeSyntax syntax, UsingState usingState, Binding context)
		{
			switch (syntax)
			{
                case PredefinedTypeSyntax predefined:
	                return (TypeBinding)((ITypeProvider)context.GetRoot()).GetType(Map[predefined.Keyword.Text], default(UsingState), false, default(ImmutableArray<object>), context);
                case NameSyntax name:
	                return (TypeBinding)context.GetParent<ITypeProvider>().GetType(TypeName.Create(name), usingState, true, default(ImmutableArray<object>), context);
                case ArrayTypeSyntax array:
                    if(array.RankSpecifiers.Count != 1)
                        throw new Exception("Jagged arrays not supported");
                    // TODO: Multi-D arrays
		            return GetTypeBinding(array.ElementType, usingState, context)?
                        .GetArrayType(ExpressionBinding.Create(array.RankSpecifiers[0].Sizes[0], context));
                default:
                    throw new Exception($"Invalid syntax {syntax}");
			}
		}
	}

	enum ClassType
	{
		Class,
        Struct,
        Interface,
        Enum,
	}

	abstract class ExpressionBinding : Binding
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

		public override void ResolveArraySizes()
		{
			ExpressionType?.ResolveArraySizes();
		}

		public virtual object Evaluate()
		{
			throw new Exception($"An expression {this} cannot be evaluated at compile-time");
		}
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
            foreach(var m in Methods)
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
            if(child is ParameterBinding param)
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
					ReturnType = ((PropertyDeclarationSyntax) syntax.Parent.Parent).Type.GetTypeBinding(usingState, this);
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

	class VariableBinding : ExpressionBinding
	{
        public TypeBinding Type { get; private set; }
        public ExpressionBinding Initializer { get; }

		private readonly int index;
		private readonly VariableDeclarationSyntax syntax;

		public VariableBinding(VariableDeclarationSyntax syntax, int index, Binding parent) : base(syntax.Variables[index].Identifier.Text, parent)
		{
            this.syntax = syntax;
			this.index = index;
			Initializer = Create(syntax.Variables[index].Initializer?.Value, this);
		}

		public override void ResolveTypes(UsingState usingState)
		{
            if(syntax.Type.IsVar)
            {
	            if(Initializer == null) throw new Exception("No initializer");
	            Type = Initializer.ExpressionType;
            } else
			    Type = syntax.Type.GetTypeBinding(usingState, Parent);
		}
	}

	abstract class StatementBinding : Binding
	{

		protected StatementBinding(Binding parent) : base(null, parent)
		{
		}

		public static StatementBinding Create(StatementSyntax syntax, Binding parent)
		{
			switch (syntax)
			{
                case BlockSyntax b:
                    return new BlockBinding(b, parent);
                case ExpressionStatementSyntax e:
                    return new ExpressionStatementBinding(e, parent);
                default:
                    throw new Exception("Invalid syntax");
			}
		}

		public override void ResolveExpressions(UsingState usingState)
		{
			// None
		}
	}

	class ExpressionStatementBinding : StatementBinding
	{
        public ExpressionBinding Expression { get; }

		public ExpressionStatementBinding(ExpressionStatementSyntax syntax, Binding parent) : base(parent)
		{
			Expression = ExpressionBinding.Create(syntax.Expression, this);
		}

		public override void ResolveTypes(UsingState usingState)
		{
			Expression.ResolveTypes(usingState);
		}

		public override Binding GetChild(string name)
		{
			throw new NotImplementedException();
		}

		protected override void AddChild(Binding child)
		{
			// None
		}

		public override void ResolveArraySizes()
		{
			Expression.ResolveArraySizes();
		}
	}

	class VariableStatementBinding : StatementBinding
	{
		private readonly LocalDeclarationStatementSyntax syntax;

        public List<VariableBinding> Variables { get; } = new List<VariableBinding>();

		public VariableStatementBinding(LocalDeclarationStatementSyntax syntax, Binding parent) : base(parent)
		{
			this.syntax = syntax;
            for (var i = 0; i < syntax.Declaration.Variables.Count; i++)
			{
				new VariableBinding(syntax.Declaration, i, this);
				
			}
        }

		public override void ResolveTypes(UsingState usingState)
		{
            // Resolve initializer expressions
			foreach(var v in Variables)
                v.ResolveTypes(usingState);
            // Add variable names to the context
            foreach(var v in Variables)
                (Parent as BlockBinding)?.AddVariable(v);
		}

		public override Binding GetChild(string name)
		{
			return Parent?.GetChild(name);
		}

		protected override void AddChild(Binding child)
		{
            if(child is VariableBinding v)
			    Variables.Add(v);
        }

		public override void ResolveArraySizes()
		{
			foreach(var v in Variables)
                v.ResolveArraySizes();
		}
	}

	class BlockBinding : StatementBinding
	{
        private readonly Dictionary<string, VariableBinding> variables
            = new Dictionary<string, VariableBinding>();

		protected override void AddChild(Binding child)
		{
			if (child is VariableBinding v)
			{
                if(variables.ContainsKey(v.Name))
                    throw new Exception($"Block already contains variable {v.Name}");
				variables[v.Name] = v;
			}
		}

		public void AddVariable(VariableBinding v)
		{
			AddChild(v);
		}

		public List<StatementBinding> Statements { get; } = new List<StatementBinding>();

		public BlockBinding(BlockSyntax syntax, Binding parent) : base(parent)
		{
			foreach (var s in syntax.Statements)
			{
                Statements.Add(Create(s, this));
			}
		}

		public override void ResolveTypes(UsingState usingState)
		{
			foreach(var s in Statements)
                s.ResolveTypes(usingState);
		}

		public override void ResolveArraySizes()
		{
			foreach(var s in Statements)
                s.ResolveArraySizes();
		}

		public override Binding GetChild(string name)
		{
			variables.TryGetValue(name, out var ret);
			return ret ?? Parent?.GetChild(name);
		}
    }

	class ExpressionBodyBinding : StatementBinding
	{
		public ExpressionBinding Expression { get; }

		public ExpressionBodyBinding(ArrowExpressionClauseSyntax syntax, Binding parent) : base(parent)
		{
			Expression = ExpressionBinding.Create(syntax.Expression, this);
		}

		public override void ResolveTypes(UsingState usingState)
		{
			Expression.ResolveTypes(usingState);
		}

		public override Binding GetChild(string name)
		{
			return Parent?.GetChild(name);
		}

		protected override void AddChild(Binding child)
		{
			// None
		}

		public override void ResolveArraySizes()
		{
			Expression.ResolveArraySizes();
		}
	}

	class EventBinding : Binding
	{
        public ExpressionBinding Trigger { get; }
        public StatementBinding Body { get; }

		public EventBinding(EventDeclarationSyntax syntax, Binding parent) : base("$event", parent)
		{
		    Trigger = ExpressionBinding.Create(syntax.TriggerExpression, this);
            if(syntax.ExpressionBody != null)
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

	abstract class TypeBinding : Binding
	{
		protected TypeBinding(string name, Binding parent) : base(name, parent)
		{
		}

		public abstract TypeBinding GetElementType(TypeBinding indexer);

        //public abstract HardwareExpression ToHardware(HardwareBinaryOp binaryOp, TypeBinding operand);

		public TypeBinding ResolveType(ClassBinding classInstance) => this;

		public ArrayTypeBinding GetArrayType(ExpressionBinding size)
		{
			// TODO: Cache these
			return new ArrayTypeBinding(this, size, null); // TODO: what is the parent?
		}
    }

	class ArrayTypeBinding : TypeBinding
	{
        public TypeBinding ElementType { get; }
        public ExpressionBinding Size { get; }
        public uint? EvaluatedSize { get; private set; }

		public override TypeBinding GetElementType(TypeBinding indexer)
		{
			return ElementType;
		}

		public ArrayTypeBinding(TypeBinding element, ExpressionBinding size, Binding parent) : base(null, parent)
		{
			ElementType = element;
			Size = size;
		}

		public override void ResolveTypes(UsingState usingState)
		{
			// None?
		}

		public override void ResolveExpressions(UsingState usingState)
		{
			// None?
		}

        public override Binding GetChild(string name)
		{
            // TODO: Length etc.
			throw new NotImplementedException();
		}

		protected override void AddChild(Binding child)
		{
			throw new NotImplementedException();
		}

		public override void ResolveArraySizes()
		{
			if (!EvaluatedSize.HasValue)
				EvaluatedSize = Convert.ToUInt32(Size.Evaluate());
		}
	}

	class Compiler
	{
		private readonly NamespaceBinding rootContext = new NamespaceBinding(null, null);

        public Compiler()
		{
		}

		public void LoadFile(string text)
		{
			var tree = CSharpSyntaxTree.ParseText(text);
			var root = (CompilationUnitSyntax)tree.GetRoot();
			rootContext.Load(root.Members, root.Usings, default(UsingState));
		}

		public void ResolveTypes()
		{
			rootContext.ResolveTypes(default(UsingState));
            rootContext.ResolveExpressions(default(UsingState));
            rootContext.ResolveArraySizes();
		}


		
	}
}
