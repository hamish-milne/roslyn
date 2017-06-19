using System;
using System.Collections.Generic;
using System.Collections.Immutable;
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
						.GetArrayType(ExpressionBinding.Create(array.RankSpecifiers[0].Sizes[0], context));
				default:
					throw new Exception($"Invalid syntax {syntax}");
			}
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
}