using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.Immutable;


namespace Parent1
{
	namespace Child1
	{
		class Type1 { }
    }

	namespace Child2
	{
		class Type2
		{
			private Child1.Type1 foo;
		}
	}

}

namespace OhdlCompiler
{
	class Program
	{

		static void Main(string[] args)
		{
			var compiler = new Compiler();
            compiler.LoadFile(@"
using Foo.Bar.Baz;
class Counter<INT N> {
	public bit[N] count { get; private set; }
	public MyNs.MyType reset;
	
	event (true) => count = reset ? 0 : count + 1;
}
");

            compiler.ResolveTypes();

		}
	}


    /*
     * Get using contexts, set for members
     * Add type definitions
     * Resolve member types
     * HARDWARE MODEL
     * 
     * 
     * */
    

    

	abstract class Binding
	{
		private Binding parent;
		public virtual Binding Parent => parent;
        public virtual string Name { get; }
		public virtual IEnumerable<Binding> Children => null;

		private static void NameToParts(NameSyntax name, List<string> parts)
		{
			if (name == null) return;
			if (name is QualifiedNameSyntax qn)
			{
				NameToParts(qn.Left, parts);
				NameToParts(qn.Right, parts);
			}
			else if (name is SimpleNameSyntax sn)
			{
				parts.Add(sn.Identifier.Text);
			}
			else throw new Exception("Unknown name node " + name);
		}

		public virtual void AddChild(Binding child)
		{
			if (child == null) return;
			child.parent = this;
		}

		public static ImmutableArray<string> GetName(NameSyntax name)
		{
			var list = new List<string>();
			NameToParts(name, list);
			return ImmutableArray.Create(list.ToArray());
		}

		public Binding GetRoot()
		{
			var c = this;
			while (c.Parent != null)
				c = c.Parent;
			return c;
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

        public abstract void ResolveTypes();

		public virtual Binding GetMember(ImmutableArray<string> name, bool isInternal) => null;

		protected Binding(string name)
		{
			Name = name;
		}
    }
	
    abstract class CompilerContext : Binding
    {
	    public override IEnumerable<Binding> Children => ChildrenMap.Values;

	    public virtual void AddChild(CompilerContext child)
	    {
		    if (child == null) return;
		    base.AddChild(child);
		    ChildrenMap[child.Name] = child;
	    }

	    protected readonly Dictionary<string, CompilerContext> ChildrenMap
            = new Dictionary<string, CompilerContext>();

	    protected CompilerContext GetChild(string name)
	    {
		    ChildrenMap.TryGetValue(name, out var d);
		    return d;
	    }

	    protected virtual Binding GetMemberUncached(ImmutableArray<string> name, bool isInternal)
        {
            return GetChild(name[0])?.GetMember(ImmutableArray.Create(name, 1, name.Length - 1), false);

        }

	    public override Binding GetMember(ImmutableArray<string> parts, bool isInternal)
	    {
		    if (parts.Length == 0) return this;
		    var mc = isInternal ? MemberCacheInternal : MemberCache;
		    if (!mc.TryGetValue(parts, out var ret))
		    {
			    mc.Add(parts, ret = GetMemberUncached(parts, isInternal));
		    }
		    return ret;
	    }

        protected readonly Dictionary<ImmutableArray<string>, Binding> MemberCache
            = new Dictionary<ImmutableArray<string>, Binding>();
        protected readonly Dictionary<ImmutableArray<string>, Binding> MemberCacheInternal
            = new Dictionary<ImmutableArray<string>, Binding>();

	    protected CompilerContext(string name) : base(name)
	    {
	    }

	    public override void ResolveTypes()
	    {
		    foreach(var c in Children)
                c.ResolveTypes();
	    }
    }

	class NamespaceContext : CompilerContext
	{
        public NamespaceContext(string name) : base(name) { }

		protected readonly HashSet<ImmutableArray<string>> Usings = new HashSet<ImmutableArray<string>>();
        protected readonly Dictionary<string, ImmutableArray<string>> Aliases = new Dictionary<string, ImmutableArray<string>>();

		public virtual void AddUsing(ImmutableArray<string> name, string alias = null)
		{
			if (alias == null) Usings.Add(name);
			else Aliases[alias] = name;
		}

		public void Load(IEnumerable<MemberDeclarationSyntax> members, IEnumerable<UsingDirectiveSyntax> usings)
		{
			foreach(var uds in usings)
                AddUsing(GetName(uds.Name), uds.Alias?.Name.Identifier.Text);

			foreach (var m in members)
			{
				switch (m)
				{
                    case NamespaceDeclarationSyntax ns:
	                    var newContext = GetChildNamespace(GetName(ns.Name));
                        newContext.Load(ns.Members, ns.Usings);
	                    break;
                    case TypeDeclarationSyntax type:
	                    AddChildType(type);
	                    break;
                    default:
                        throw new Exception($"Invalid syntax {m}");
				}
			}
		}

		public NamespaceContext GetChildNamespace(ImmutableArray<string> name)
		{
			if (name.Length == 0) return this;
			var child = GetChild(name[0]);
            if(child == null)
                AddChild(child = new NamespaceContext(name[0]));
			var ns = child as NamespaceContext;
			if (ns == null)
			    throw new Exception($"Type {name[0]} is already a namespace name");
			return ns.GetChildNamespace(ImmutableArray.Create(name, 1, name.Length - 1));
		}

		public TypeContext AddChildType(TypeDeclarationSyntax type)
		{
			var child = GetChild(type.Identifier.Text);
			if (child == null)
			{
				var tc = new TypeContext(type);
                AddChild(tc);
				return tc;
			}
			throw new Exception($"Type or namespace {type.Identifier.Text} already defined");
		}

        protected override Binding GetMemberUncached(ImmutableArray<string> name, bool isInternal)
        {
	        var root = GetRoot();
            // Try to find a type with aliases
			if (isInternal)
			{
				if (Aliases.TryGetValue(name[0], out var aName))
					return root.GetMember(aName, false) ?? throw new Exception($"The alias {name[0]}={aName} is invalid!");
			}
            // Try to find a type directly with the first part
            if (GetChild(name[0]) is TypeContext type)
                return type.GetMember(ImmutableArray.Create(name, 1, name.Length - 1), false);
            if (isInternal)
			{
				// Find a type in usings
				foreach (var u in Usings)
				{
                    if (root.GetMember(u.Add(name[0]), false) is TypeContext candidateType)
                        return candidateType.GetMember(ImmutableArray.Create(name, 1, name.Length - 1), false);
                }
			}
            // Otherwise, look in child namespaces as normal
            // Failing that, look in the parent namespace
	        return base.GetMemberUncached(name, false) ?? Parent?.GetMember(name, isInternal);
        }
	}

	static class PredefinedTypes
	{
		private static readonly Dictionary<string, ImmutableArray<string>> Map = new Dictionary<string, ImmutableArray<string>>
		{
			{"bit", ImmutableArray.Create("Sys", "Bit")},
		};

		public static ImmutableArray<string> Find(PredefinedTypeSyntax syntax)
		{
			return Map[syntax.Keyword.Text];
		}

		public static TypeContext GetTypeContext(this TypeSyntax syntax, Binding binding)
		{
			switch (syntax)
			{
                case PredefinedTypeSyntax predefined:
	                return (TypeContext)binding.GetRoot().GetMember(Map[predefined.Keyword.Text], false);
                case QualifiedNameSyntax name:
	                return (TypeContext)binding.GetMember(Binding.GetName(name), true);
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
		protected ExpressionBinding(string name) : base(name)
		{
		}

        public virtual TypeContext ExpressionType { get; protected set; }

		public override Binding GetMember(ImmutableArray<string> name, bool isInternal)
		{
			return Parent.GetMember(name, isInternal);
		}
    }

	class ParameterBinding : ExpressionBinding
	{
		private readonly ParameterSyntax syntax;

		public ParameterBinding(ParameterSyntax syntax) : base(syntax.Identifier.Text)
		{
			this.syntax = syntax;
		}

		public override void ResolveTypes()
		{
			ExpressionType = syntax.Type.GetTypeContext(this);
		}
	}

	abstract class MemberBinding : ExpressionBinding
	{
		protected MemberBinding(string name) : base(name)
		{
		}

		public virtual ImmutableArray<string>? ExplicitInterface => null;

    }

	class FieldBinding : MemberBinding
	{
		private readonly FieldDeclarationSyntax syntax;

		public FieldBinding(FieldDeclarationSyntax syntax, int index) : base(syntax.Declaration.Variables[index].Identifier
			.Text)
		{
			this.syntax = syntax;
		}

		public override void ResolveTypes()
		{
			ExpressionType = syntax.Declaration.Type.GetTypeContext(this);
		}
	}

	class PropertyBinding : MemberBinding
	{
		private readonly PropertyDeclarationSyntax syntax;

		public PropertyBinding(PropertyDeclarationSyntax syntax) : base(syntax.Identifier.Text)
		{
			this.syntax = syntax;
		}

		public override void ResolveTypes()
		{
			ExpressionType = syntax.Type.GetTypeContext(this);
		}
	}

	class MethodGroupBinding : MemberBinding
	{
		public MethodGroupBinding(string name) : base(name) { }

		protected readonly List<MethodBinding> Methods = new List<MethodBinding>();

		public override IEnumerable<Binding> Children => Methods;

		public void AddMethod(BaseMethodDeclarationSyntax syntax)
		{
			Methods.Add(new MethodBinding(this, syntax));
		}

		public override void ResolveTypes()
		{
			throw new NotImplementedException();
		}
	}
    

	class MethodBinding : Binding
	{
		public override Binding Parent { get; }

		private readonly BaseMethodDeclarationSyntax syntax;

		public override string Name => ((MethodGroupBinding) Parent).Name;

		public MethodBinding(MethodGroupBinding parent, BaseMethodDeclarationSyntax syntax) : base(null)
		{
            Parent = parent;
			this.syntax = syntax;

		}

        public TypeContext ReturnType { get; private set; }
        public ImmutableArray<ParameterBinding> Parameters { get; private set; }

		public override void ResolveTypes()
		{
			if (syntax is MethodDeclarationSyntax method)
				ReturnType = method.ReturnType.GetTypeContext(this);
			Parameters = ImmutableArray.Create(syntax.ParameterList.Parameters.Select(p => new ParameterBinding(p)).ToArray());
		}
	}

	class EventBinding : Binding
	{
		public EventBinding(EventDeclarationSyntax syntax) : base("$event")
		{
		    
		}

		public override void ResolveTypes()
		{
			
		}
	}

    class TypeContext : CompilerContext
    {
        protected readonly Dictionary<string, MemberBinding> Members = new Dictionary<string, MemberBinding>();
        protected readonly List<EventBinding> Events = new List<EventBinding>();

        public ClassType ClassType { get; }
        public MethodGroupBinding Constructor { get; } = new MethodGroupBinding(".ctor");

	    public TypeContext(TypeDeclarationSyntax syntax) : base(syntax.Identifier.Text)
	    {
		    ClassType = GetClassType(syntax.Keyword);
            foreach(var m in syntax.Members)
                AddMember(m);
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

	    protected void AddMember(MemberBinding member)
	    {
		    if (Members.ContainsKey(member.Name))
		    {
			    throw new Exception($"Member {member.Name} in {Name} already exists!");
		    }
		    Members[member.Name] = member;
	    }

	    protected void AddMember(MemberDeclarationSyntax member)
	    {
		    switch (member)
		    {
                case TypeDeclarationSyntax type:
                    AddChild(new TypeContext(type));
	                break;
                case FieldDeclarationSyntax field:
                    for(int i = 0, l = field.Declaration.Variables.Count; i < l; i++)
                        AddMember(new FieldBinding(field, i));
	                break;
                case MethodDeclarationSyntax method:
	                var mName = method.Identifier.Text;
                    if (!Members.TryGetValue(mName, out var methodGroup))
                        AddMember(methodGroup = new MethodGroupBinding(mName));
                    ((MethodGroupBinding)methodGroup).AddMethod(method);
	                break;
                case ConstructorDeclarationSyntax ctor:
                    Constructor.AddMethod(ctor);
	                break;
                case PropertyDeclarationSyntax property:
                    AddMember(new PropertyBinding(property));
	                break;
                case EventDeclarationSyntax sEvent:
                    Events.Add(new EventBinding(sEvent));
	                break;
                default:
                    throw new Exception($"Invalid syntax {member}");
		    }
	    }

	    public override void ResolveTypes()
	    {
		    base.ResolveTypes();
		    foreach (var m in Members)
			    m.Value.ResolveTypes();
	    }
    }

	class Compiler
	{
		private readonly NamespaceContext rootContext = new NamespaceContext(null);

        public Compiler()
		{
		}

		public void LoadFile(string text)
		{
			var tree = CSharpSyntaxTree.ParseText(text);
			var root = (CompilationUnitSyntax)tree.GetRoot();
			rootContext.Load(root.Members, root.Usings);
		}

		public void ResolveTypes()
		{
			rootContext.ResolveTypes();
		}


		
	}
}
