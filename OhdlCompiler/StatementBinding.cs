using System;
using System.Collections.Generic;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace OhdlCompiler
{
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
			if (syntax.Type.IsVar)
			{
				if (Initializer == null) throw new Exception("No initializer");
				Type = Initializer.ExpressionType;
			}
			else
				Type = syntax.Type.GetTypeBinding(usingState, Parent);
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
			foreach (var v in Variables)
				v.ResolveTypes(usingState);
			// Add variable names to the context
			foreach (var v in Variables)
				(Parent as BlockBinding)?.AddVariable(v);
		}

		public override Binding GetChild(string name)
		{
			return Parent?.GetChild(name);
		}

		protected override void AddChild(Binding child)
		{
			if (child is VariableBinding v)
				Variables.Add(v);
		}

		public override void ResolveArraySizes()
		{
			foreach (var v in Variables)
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
				if (variables.ContainsKey(v.Name))
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
			foreach (var s in Statements)
				s.ResolveTypes(usingState);
		}

		public override void ResolveArraySizes()
		{
			foreach (var s in Statements)
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
}