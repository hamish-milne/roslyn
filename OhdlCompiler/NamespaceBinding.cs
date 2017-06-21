using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace OhdlCompiler
{
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

		public override void ResolveInterface()
		{
			foreach (var c in children.Values)
				c.ResolveInterface();
		}

		public NamespaceBinding GetChildNamespace(TypeName name)
		{
			if (name == null) return this;
			var child = GetChild(name.Name) ?? new NamespaceBinding(name.Name, this);
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
						UserTypeBinding.Create(type, state, this, default(ImmutableArray<object>), this);
						break;
					default:
						throw new Exception($"Invalid syntax {m}");
				}
			}
		}
	}
}