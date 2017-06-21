using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace OhdlCompiler
{

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
					if (array.RankSpecifiers.Count != 1)
						throw new Exception("Jagged arrays not supported");
					// TODO: Multi-D arrays
					return GetTypeBinding(array.ElementType, usingState, context)?
						.GetArrayType(array.RankSpecifiers[0].Sizes.Select(e => ExpressionBinding.Create(e, context)));
				default:
					throw new Exception($"Invalid syntax {syntax}");
			}
		}
	}

	public struct FlatValue
	{
		public ImmutableArray<string> Name;
		public ImmutableArray<uint> Sizes;
	}

    public abstract class TypeBinding : Binding
	{
		protected TypeBinding(string name, Binding parent) : base(name, parent)
		{
		}

		public abstract TypeBinding GetElementType(TypeBinding indexer);

		//public abstract HardwareExpression ToHardware(HardwareBinaryOp binaryOp, TypeBinding operand);

		public ArrayTypeBinding GetArrayType(IEnumerable<ExpressionBinding> sizes)
		{
			// TODO: Cache these
			return new ArrayTypeBinding(this, sizes, null); // TODO: what is the parent?
		}

        public virtual uint? PrimitiveSize => null;
	}

	public class ArrayTypeBinding : TypeBinding
	{
		public TypeBinding ElementType { get; }
		public ImmutableArray<ExpressionBinding> Sizes { get; }
		private uint? primitiveSize;
		public override uint? PrimitiveSize => primitiveSize;

		public override TypeBinding GetElementType(TypeBinding indexer)
		{
			return ElementType;
		}

		public ArrayTypeBinding(TypeBinding element, IEnumerable<ExpressionBinding> sizes, Binding parent) : base(null, parent)
		{
			ElementType = element;
			Sizes = ImmutableArray.Create(sizes.ToArray());
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

		public override void ResolveInterface()
		{
			if (primitiveSize == null)
				primitiveSize = Sizes.Select(e => Convert.ToUInt32(e.Evaluate())).Aggregate((a,b) => a * b);
		}
	}
}