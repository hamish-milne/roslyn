using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
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
