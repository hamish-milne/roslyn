using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
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

    abstract class UserTypeBinding : TypeBinding, ITypeProvider
	{
		protected readonly UsingState usingState;
		protected readonly TypeDeclarationSyntax syntax;
		protected readonly Binding genericContext;

		private readonly Dictionary<string, Binding> children
			= new Dictionary<string, Binding>();
		protected readonly List<MemberBinding> members = new List<MemberBinding>();
		public UserTypeBinding BaseClass { get; private set; }

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
			return Create(syntax, usingState, Parent, genericArgs, genericDefinedIn);
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
				if (child is MemberBinding m)
					members.Add(m);
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

		public override void ResolveInterface()
		{
			if (arraySizesResolved) return;
			arraySizesResolved = true;

			BaseClass?.ResolveInterface();
			foreach (var m in children.Values)
				m.ResolveInterface();
		}

		protected void AddMember(MemberDeclarationSyntax member)
		{
			// ReSharper disable ObjectCreationAsStatement
            switch (member)
			{
				case TypeDeclarationSyntax type:
					Create(type, usingState, this, default(ImmutableArray<object>), this);
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
			// ReSharper restore ObjectCreationAsStatement
        }

        protected readonly List<EventBinding> Events = new List<EventBinding>();

		public abstract ClassType ClassType { get; }
		public MethodGroupBinding Constructor { get; }

		protected UserTypeBinding(TypeDeclarationSyntax syntax, UsingState usingState, Binding parent, ImmutableArray<object> genericArgs, Binding genericContext) :
			base(genericArgs.IsDefault ?  syntax.Identifier.Text : null, parent)
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
				this.syntax = syntax;
				foreach (var m in syntax.Members)
					AddMember(m);
			}
			else
			{
				OpenGenericArgs = syntax.TypeParameterList.Parameters.Count;
			}
		}

		public override TypeBinding GetElementType(TypeBinding indexer)
		{
			// TODO: Use indexer definition
			throw new NotImplementedException();
		}

		/*public override IEnumerable<FlatValue> Flatten()
		{
			if (ClassType == ClassType.Struct)
				yield return new FlatValue
				{
					Name = ImmutableArray<string>.Empty,
					Sizes = ImmutableArray.Create((uint) structFieldDecl.Sum(m => m.ExpressionType.PrimitiveSize.Value))
				};
			else if(ClassType == ClassType.Class)
				foreach (var m in structFieldDecl)
				{
					foreach (var v in m.ExpressionType.Flatten())
						yield return new FlatValue { Name = ImmutableArray.Create(m.Name).AddRange(v.Name), Sizes = v.Sizes };
                }
		}*/

		public static UserTypeBinding Create(TypeDeclarationSyntax syntax, UsingState state, Binding parent,
			ImmutableArray<object> genericArgs, Binding genericContext)
		{
			switch (syntax)
			{
                case ClassDeclarationSyntax c:
	                return new ClassBinding(c, state, parent, genericArgs, genericContext);
                case StructDeclarationSyntax s:
					return new StructBinding(s, state, parent, genericArgs, genericContext);
                default:
					throw new NotSupportedException();
			}
		}
	}

	class ClassBinding : UserTypeBinding
	{
		public override ClassType ClassType => ClassType.Class;

		public ImmutableArray<MemberBinding> Interface { get; private set; }

		public override void ResolveInterface()
		{
			base.ResolveInterface();
			Interface = ImmutableArray.Create(members.Where(m => m.IsReg && m.Access != MemberAccess.Private).ToArray());
		}

		public HardwareModule ToHardware()
		{
			return new HardwareModule
			{
				Name = FullyQualifiedName,

			};
		}

		public ClassBinding(ClassDeclarationSyntax syntax, UsingState usingState,
			Binding parent, ImmutableArray<object> genericArgs, Binding genericContext)
			: base(syntax, usingState, parent, genericArgs, genericContext)
		{
		}
	}

	class StructBinding : UserTypeBinding
	{
		public override ClassType ClassType => ClassType.Struct;

		private uint? primitiveSize;
		public override uint? PrimitiveSize => primitiveSize;

		public override void ResolveInterface()
		{
			base.ResolveInterface();
			primitiveSize = 0;
			foreach (var m in members)
			{
				if (!m.IsReg) continue;
				if(m.ExpressionType == null)
					throw new Exception($"Untyped member {m}");
				if (m.ExpressionType.PrimitiveSize.HasValue)
				{
					m.Offset = primitiveSize;
					primitiveSize += m.ExpressionType.PrimitiveSize;
				} else
					throw new Exception("Structs cannot contain class-type members");
			}
		}

		public StructBinding(StructDeclarationSyntax syntax, UsingState usingState,
			Binding parent, ImmutableArray<object> genericArgs, Binding genericContext)
			: base(syntax, usingState, parent, genericArgs, genericContext)
		{
		}
	}
}