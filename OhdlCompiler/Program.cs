using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace OhdlCompiler
{
	class Program
	{

		static void Main(string[] args)
		{
			var tree = CSharpSyntaxTree.ParseText(@"
using Foo.Bar.Baz;
class Counter<INT N> {
	public bit[N] count { get; private set; }
	public bit reset;
	
	event (true) => count = reset ? 0 : count + 1;
}
");
			var errors = tree.GetRoot().GetDiagnostics();
			/*var compilation = CSharpCompilation.Create("HelloWorld")
											   .AddReferences(
													MetadataReference.CreateFromFile(
														typeof(object).Assembly.Location))
											   .AddSyntaxTrees(tree);
			var model = compilation.GetSemanticModel(tree);
			var root = (CompilationUnitSyntax)tree.GetRoot();
			var symbol = model.GetTypeInfo(root.Members[0]);*/


		}
	}
	
    abstract class CompilerContext
    {
        public CompilerContext Parent { get; private set; }
	    public virtual IEnumerable<CompilerContext> Children => ChildrenMap.Values;
        public virtual string Name { get; }

	    public virtual void AddChild(CompilerContext child)
	    {
		    if (child == null) return;
		    ChildrenMap[child.Name] = child;
		    child.Parent = this;
	    }

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

	    public static ImmutableArray<string> GetName(NameSyntax name)
	    {
		    var list = new List<string>();
            NameToParts(name, list);
		    return ImmutableArray.Create(list.ToArray());
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

	    protected readonly Dictionary<string, CompilerContext> ChildrenMap
            = new Dictionary<string, CompilerContext>();

	    public CompilerContext GetMember(ImmutableArray<string> parts)
	    {
		    if (parts.Length == 0) return this;
            if (!MemberCache.TryGetValue(parts, out var ret))
		    {
                MemberCache.Add(parts, ret = GetMemberUncached(parts));
		    }
		    return ret;
	    }

	    protected CompilerContext GetMemberDirect(string name)
	    {
		    ChildrenMap.TryGetValue(name, out var d);
		    return d;
	    }

	    protected virtual CompilerContext GetMemberUncached(ImmutableArray<string> name)
        {
            return GetMemberDirect(name[0])?.GetMember(ImmutableArray.Create(name, 1, name.Length - 1));
            
	    }
        protected readonly Dictionary<ImmutableArray<string>, CompilerContext> MemberCache
            = new Dictionary<ImmutableArray<string>, CompilerContext>();

	    protected CompilerContext(string name)
	    {
		    Name = name;
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
                AddUsing(GetName(uds.Name), uds.Alias.Name.Identifier.Text);

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
			var child = GetMemberDirect(name[0]);
            if(child == null)
                AddChild(child = new NamespaceContext(name[0]));
			var ns = child as NamespaceContext;
			if (ns == null)
			{
				// ERROR: Type already declared
				return null;
			}
			return ns.GetChildNamespace(ImmutableArray.Create(name, 1, name.Length - 1));
		}

		public TypeContext AddChildType(TypeDeclarationSyntax type)
		{
			var child = GetMemberDirect(type.Identifier.Text);
			if (child == null)
			{
				var tc = new TypeContext(type);
                AddChild(tc);
				return tc;
			}
			// ERROR: Type or namespace already declared
			return null;
		}

        protected override CompilerContext GetMemberUncached(ImmutableArray<string> name)
		{
            
			return base.GetMemberUncached(name);
		}
	}

	enum ClassType
	{
		Class,
        Struct,
        Interface,
        Enum,
	}

    class TypeContext : CompilerContext
    {
        public ClassType ClassType { get; }

	    public TypeContext(TypeDeclarationSyntax syntax) : base(syntax.Identifier.Text)
	    {
		    ClassType = GetClassType(syntax.Keyword);
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

		
	}
}
