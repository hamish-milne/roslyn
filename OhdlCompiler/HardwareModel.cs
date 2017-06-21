using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Text;
using static OhdlCompiler.HardwareBinaryOp;

namespace OhdlCompiler
{
    class HardwareModel
    {
	    public List<HardwareModule> Modules = new List<HardwareModule>();
    }

	public abstract class HardwareExpression
	{
		public abstract void Emit(StringBuilder sb, ref int indent);

		public virtual bool IsStatement => false;
	}

	public class HardwareNamedValue : HardwareExpression
	{
		public ImmutableArray<string> Name { get; set; }

		public override void Emit(StringBuilder sb, ref int indent)
		{
			foreach (var n in Name)
				sb.Append(n).Append('$');
		}
	}

    public abstract class HardwareMember : HardwareNamedValue
	{
		public bool IsPublic { get; set; }

		public virtual void EmitDeclaration(StringBuilder sb, ref int indent)
		{
			sb.Append('\t', indent);
		}
		public virtual void EmitAsOutput(StringBuilder sb, ref int indent)
		{
			sb.Append('\t', indent);
		}
	}

	abstract class HardwareSizedMember : HardwareMember
	{
		public ImmutableArray<uint> Size { get; set; }

		public override void EmitDeclaration(StringBuilder sb, ref int indent)
		{
			foreach (var s in Size)
				sb.Append("[").Append(s).Append(":0] ");
        }
	}

    class HardwareParameter : HardwareNamedValue
	{
        public bool IsOut { get; set; }
        public ImmutableArray<uint> Size { get; set; }

		public virtual void EmitDeclaration(StringBuilder sb, ref int indent)
		{
			sb.Append('\t', indent).Append(IsOut ? "output" : "input").Append(' ');
			foreach (var s in Size)
				sb.Append("[").Append(s).Append(":0] ");
            Emit(sb, ref indent);
			sb.Append(";\n");
		}
	}

	class HardwareConstant : HardwareExpression
	{
		public long Value { get; set; }

		public override void Emit(StringBuilder sb, ref int indent)
		{
			sb.Append(Value);
		}
	}

	class HardwareModuleInstance : HardwareMember
	{
		public HardwareModule Module { get; set; }
        public List<KeyValuePair<string, int>> Parameters { get; } = new List<KeyValuePair<string, int>>();
        public List<HardwareExpression> Arguments { get; } = new List<HardwareExpression>();

		public override void EmitDeclaration(StringBuilder sb, ref int indent)
		{
			sb.Append('\t', indent);
            Emit(sb, ref indent);
			if (Parameters.Count > 0)
			{
				sb.Append(" #( ");
				for (var i = 0; i < Parameters.Count; i++)
				{
					if (i > 0)
						sb.Append(", ");
					var pair = Parameters[i];
					sb.Append('.').Append(pair.Key).Append('(').Append(pair.Value).Append(')');
				}
				sb.Append(")");
            }
			sb.Append(' ');
            Emit(sb, ref indent);

			sb.Append('(');
			for (int i = 0; i < Arguments.Count; i++)
			{
				if (i > 0)
					sb.Append(", ");
                Arguments[i].Emit(sb, ref indent);
			}
            sb.Append(");\n");
		}
	}

	class HardwareModuleParam : HardwareNamedValue
	{
		public int DefaultValue { get; set; }

		public virtual void EmitDeclaration(StringBuilder sb, ref int indent)
		{
			sb.Append('\t', indent).Append("parameter ");
            Emit(sb, ref indent);
			sb.Append(" = ").Append(DefaultValue).Append(";\n");
		}
	}

	class HardwareMethod : HardwareMember
	{
        public List<HardwareParameter> Parameters { get; } = new List<HardwareParameter>();
		public List<HardwareRegister> Variables { get; } = new List<HardwareRegister>();
        public HardwareBlock Block { get; set; }

		public override void EmitDeclaration(StringBuilder sb, ref int indent)
		{
			sb.Append('\t', indent).Append("task ");
            Emit(sb, ref indent);
			sb.Append(";\n");
			indent++;
            foreach (var p in Parameters)
                p.EmitDeclaration(sb, ref indent);
			indent--;
            Block.Emit(sb, ref indent);
		}
	}

	class HardwareEvent
	{
        public bool Invert { get; set; }
        public HardwareExpression Trigger { get; set; }
        public HardwareStatement Statement { get; set; }

		public virtual void EmitDeclaration(StringBuilder sb, ref int indent)
		{
			sb.Append('\t', indent).Append("always @(").Append(Invert ? "negedge" : "posedge").Append(' ');
            Trigger.Emit(sb, ref indent);
			sb.Append(")\n");
            Statement.Emit(sb, ref indent);
		}
	}

	class HardwareInput : HardwareSizedMember
	{
		public override void EmitDeclaration(StringBuilder sb, ref int indent)
		{
			sb.Append('\t', indent).Append("input ");
            base.EmitDeclaration(sb, ref indent);
            sb.Append(";\n");
		}
	}

	class HardwareRegister : HardwareSizedMember
	{

		public override void EmitDeclaration(StringBuilder sb, ref int indent)
		{
			sb.Append('\t', indent).Append("reg ");
            base.EmitDeclaration(sb, ref indent);
            Emit(sb, ref indent);
			sb.Append(';');
		}
	}

	

	class HardwareWire : HardwareSizedMember
	{
        public HardwareExpression Expression { get; set; }

		public override void EmitDeclaration(StringBuilder sb, ref int indent)
		{
			sb.Append('\t', indent).Append("wire ");
			base.EmitDeclaration(sb, ref indent);
            Emit(sb, ref indent);
			sb.Append(";\n").Append("assign ");
            Emit(sb, ref indent);
		}
	}



	enum HardwareUnaryOp
	{
		Plus,
        Minus,
        BitwiseNot,
        BoolNot,

	}

	class HardwareUnary : HardwareExpression
	{
		private static readonly Dictionary<HardwareUnaryOp, char> Map = new Dictionary<HardwareUnaryOp, char>
		{
            {HardwareUnaryOp.Plus, '+' },
            {HardwareUnaryOp.Minus, '-' },
            {HardwareUnaryOp.BitwiseNot, '~' },
            {HardwareUnaryOp.BoolNot, '!' },
		};

		public HardwareUnaryOp Op { get; set; }
        public HardwareExpression RValue { get; set; }

		public override void Emit(StringBuilder sb, ref int indent)
		{
			sb.Append(Map[Op]);
            RValue.Emit(sb, ref indent);
		}
	}

	enum HardwareBinaryOp
	{
		Add,
		Sub,
		Mul,
		Div,
		BitwiseOr,
		BitwiseAnd,
		BitwiseXor,
		BoolOr,
		BoolAnd,
		BoolXor,
		Assign,
		AssignAdd,
		AssignSub,
		AssignMul,
		AssignDiv,
		AssignOr,
		AssignAnd,
		AssignXor,
	}

    class HardwareBinary : HardwareExpression
	{
        
		private static readonly Dictionary<HardwareBinaryOp, string> Map = new Dictionary<HardwareBinaryOp, string>
		{
            { Add, "+" },
            { Sub, "-" },
            { Mul, "*" },
            { Div, "/" },
            { BitwiseOr, "|" },
            { BitwiseAnd, "&" },
            { BitwiseXor, "^" },
            { BoolOr, "||" },
            { BoolAnd, "&&" },
            { BoolXor, "^" },
            { Assign, "=" },
            { AssignAdd, "+=" },
            { AssignSub, "-=" },
            { AssignMul, "*=" },
            { AssignDiv, "/=" },
            { AssignOr, "|=" },
            { AssignAnd, "&=" },
            { AssignXor, "^=" },
		};

		public HardwareBinaryOp Op { get; set; }
        public HardwareExpression RValue { get; set; }
        public HardwareExpression LValue { get; set; }
		public override bool IsStatement => Op >= Assign;

		public override void Emit(StringBuilder sb, ref int indent)
		{
			sb.Append('(');
            LValue.Emit(sb, ref indent);
			sb.Append(Map[Op]);
            RValue.Emit(sb, ref indent);
			sb.Append(')');
		}
	}

	class HardwareTernary : HardwareExpression
	{
		public HardwareExpression Condition { get; set; }
		public HardwareExpression WhenTrue { get; set; }
		public HardwareExpression WhenFalse { get; set; }

		public override void Emit(StringBuilder sb, ref int indent)
		{
			sb.Append('(');
			Condition.Emit(sb, ref indent);
			sb.Append(" ? ");
			WhenTrue.Emit(sb, ref indent);
			sb.Append(" : ");
			WhenFalse.Emit(sb, ref indent);
			sb.Append(')');
		}
	}

	class HardwareCall : HardwareExpression
	{
		public HardwareMethod Method { get; set; }
        public List<HardwareExpression> Arguments { get; } = new List<HardwareExpression>();
		public override bool IsStatement => true;

		public override void Emit(StringBuilder sb, ref int indent)
		{
			Method.Emit(sb, ref indent);
			sb.Append('(');
			for (int i = 0; i < Arguments.Count - 1; i++)
			{
				Arguments[i].Emit(sb, ref indent);
				sb.Append(", ");
			}
			sb.Append(')');
		}
	}

	abstract class HardwareStatement
	{
		public virtual void Emit(StringBuilder sb, ref int indent)
		{
			sb.Append('\t', indent);
		}
	}

	class HardwareStatementSingle : HardwareStatement
	{
        public HardwareExpression Expression { get; set; }

		public override void Emit(StringBuilder sb, ref int indent)
		{
            if(!Expression.IsStatement)
                throw new Exception($"Expression {Expression} is not a valid statement");
			sb.Append('\t', indent);
            Expression.Emit(sb, ref indent);
			sb.Append(";\n");
		}
	}

	class HardwareBlock : HardwareStatement
	{
		public List<HardwareStatement> Statements { get; } = new List<HardwareStatement>();

		public override void Emit(StringBuilder sb, ref int indent)
		{
			sb.Append('\t', indent).Append("begin\n");
			indent++;
			foreach(var m in Statements)
                m.Emit(sb, ref indent);
			indent--;
			sb.Append('\t', indent).Append("end\n");
        }
	}

	class HardwareModule : HardwareMember
	{
        public List<HardwareModuleParam> Parameters { get; } = new List<HardwareModuleParam>();
        public List<HardwareEvent> Events { get; } = new List<HardwareEvent>();
		public List<HardwareMember> Members { get; } = new List<HardwareMember>();

		public override void EmitDeclaration(StringBuilder sb, ref int indent)
		{
			sb.Append('\t', indent).Append("module ");
            Emit(sb, ref indent);
			sb.Append(" (\n");
			indent++;
			foreach (var m in Members)
			{
                if(m.IsPublic)
				    m.EmitAsOutput(sb, ref indent);
			}
			sb.Append(");\n");
			foreach (var m in Members)
			{
                m.EmitDeclaration(sb, ref indent);
			}
            foreach(var e in Events)
                e.EmitDeclaration(sb, ref indent);
			indent--;
            sb.Append('\t', indent).Append("endmodule\n");
        }
	}
}
