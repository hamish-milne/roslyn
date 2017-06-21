using System.Collections.Generic;
using System.Collections.Immutable;

namespace OhdlCompiler
{
	public struct UsingState
	{
		public ImmutableArray<TypeName> Usings { get; set; }
		public ImmutableArray<KeyValuePair<string, TypeName>> Aliases { get; set; }
	}

	public interface ITypeProvider
	{
		ITypeProvider GetType(TypeName name, UsingState state, bool traverseUp, ImmutableArray<object> genericArgs, Binding context);
	}

    public abstract class Binding
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
		public abstract void ResolveInterface();

		protected Binding(string name, Binding parent)
		{
			Name = name;
			Parent = parent;
			parent?.AddChild(this);
		}
	}
}