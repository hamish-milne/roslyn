using System;
using System.Collections.Generic;

namespace OhdlCompiler
{

	enum MetaTypeId
	{
		Int,
		Float,
		String,
		Type,
		Bool,
	}

    class MetaType : TypeBinding
	{
		public MetaTypeId Id { get; }

		private MetaType(MetaTypeId id) : base(id.ToString().ToUpperInvariant(), null)
		{
			Id = id;
		}

		private static readonly Dictionary<string, MetaType> All
			= new Dictionary<string, MetaType>();

		static MetaType()
		{
			foreach (var v in Enum.GetValues(typeof(MetaTypeId)))
			{
				var t = new MetaType((MetaTypeId) v);
				All[t.Name] = t;
			}
		}

		public static MetaType Get(string name)
		{
			All.TryGetValue(name, out var ret);
			return ret;
		}

		protected override void AddChild(Binding child)
		{
			throw new NotImplementedException();
		}

		public override Binding GetChild(string name)
		{
			throw new NotImplementedException();
		}

		public override void ResolveTypes(UsingState usingState)
		{
			throw new NotImplementedException();
		}

		public override void ResolveExpressions(UsingState usingState)
		{
			throw new NotImplementedException();
		}

		public override TypeBinding GetElementType(TypeBinding indexer)
		{
			throw new NotImplementedException();
		}

		public override void ResolveArraySizes()
		{
			// None
		}
	}
}