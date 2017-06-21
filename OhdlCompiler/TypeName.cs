using System;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace OhdlCompiler
{
	public class TypeName : IEquatable<TypeName>
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
}