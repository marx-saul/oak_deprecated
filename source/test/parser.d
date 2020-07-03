module test.parser;

import std.stdio, std.conv;

unittest {
	writeln("/------- parser test 1 -------/");

	import parser.parse, parser.lexer;
	// show expression
	{
		import parser.expression;
		auto lx  = new Lexer!string("(f n * (n-1), struct(Vector2){x:a![3], y:--a![0]}, [\"hello\" : 3+2], [], [#a])");
		auto expr = expression(lx);
		auto v = new ToStringVisitor();
		expr.accept(v);
		writeln(v.str);
	}

	// show expression
	{
		import parser.expression;
		auto lx  = new Lexer!string(`{
			static let:int a = 0;
			let b:static int, c:int;
			while a <= 10 : {
				writeln a;
				writeln if a % 2 == 0 : "even" else "odd"
				a.inc;
			};
			static immut let a:int = 0, msg:string = "message!";
			for a = 0 ; a <= 0 ; a.inc : {
				writeln if a % 2 == 0 : "even" else "odd"
			};

			label_1 :

			private @pure @safe @trusted immut func factorial:int n:int = if n>0 : n * factorial(n-1) else 1;
			public package @pure @safe immut func fibonacci:int n:int = {
				let i = 1, j = 1,;
				while n > 0 : {
					let t = i+j;
					i = j;
					j = t;
					n.dec;
				};
				i
			};

			func swap x:ref int y:ref int = {
				let t = x;
				x = y;
				y = t;
			};

			writeln app fibonacci app factorial 5
		}`);
		auto v = new ToStringVisitor();
		auto expr = expression(lx);
		expr.accept(v);
		writeln(v.str);
	}

	// show type
	{
		import parser.type;
		auto lx  = new Lexer!string("(a -> b) -> [a] -> [b]");
		auto type = type(lx);
		auto v = new ToStringVisitor();
		type.accept(v);
		writeln(v.str);
	}

}

import visitor.visitor, ast.all;
class ToStringVisitor : PermissiveVisitor {
	size_t depth;
	string str = "";
	void add(string[] ss...) {
		foreach (s; ss) str ~= s;
	}

	// Expression
	override void visit(ASTNode)			{ assert(0); }
	override void visit(Expression)		 	{ assert(0); }
	override void visit(BinaryExpression e) {
		add("(");
		if (e.left)  e.left. accept(this);
		add(" ", e.op.to!string, " ");
		if (e.right) e.right.accept(this);
		add(")");
	}
	override void visit(UnaryExpression e) {
		add(e.op.to!string);
		if (e.expr) e.expr.accept(this);
	}
	override void visit(WhenExpression e) {
		add("(");
		if (e.left)   e.left.  accept(this);
		add(" when ");
		if (e.center) e.center.accept(this);
		add(" else ");
		if (e.right)  e.right. accept(this);
		add(")");
	}
	override void visit(TupleExpression e) {
		add("(");
		foreach (expr; e.exprs) {
			if (expr) expr.accept(this);
			add(", ");
		}
		str = str[0..$-2];
		add(")");
	}
	override void visit(IndexExpression e) {
		add("(");
		if (e.expr)   e.expr.accept(this);
		add("![");
		foreach (expr; e.indices) {
			if (expr) expr.accept(this);
			add(", ");
		}
		str = str[0..$-2];
		add("])");
	}
	override void visit(SliceExpression e) {
		add("(");
		if (e.expr) e.expr.accept(this);
		add("{");
		if (e.from) e.from.accept(this);
		add("..");
		if (e.to)   e.to.  accept(this);
		add("})");
	}
	override void visit(IntegerExpression e) {
		add(e.value.to!string);
	}
	override void visit(RealExpression e) {
		add(e.value.to!string);
	}
	override void visit(StringExpression e) {
		add("`");
		add(e.str);
		add("`");
	}
	override void visit(IdentifierExpression e) {
		add(e.id.name);
	}
	override void visit(DollarExpression e) { visit(cast(IdentifierExpression)e); }
	override void visit(ThisExpression e)   { visit(cast(IdentifierExpression)e); }
	override void visit(SuperExpression e)  { visit(cast(IdentifierExpression)e); }
	override void visit(UnitExpression e)   { add("()"); }
	override void visit(ArrayExpression e)  {
		add("[");
		foreach (expr; e.exprs) {
			if (expr) expr.accept(this);
			add(", ");
		}
		str = str[0..$-2];
		add("]");
	}
	override void visit(AssocArrayExpression e) {
		add("[");
		foreach (i; 0 .. e.keys.length) {
			if (e.keys[i])   e  .keys[i].accept(this);
			add(":");
			if (e.values[i]) e.values[i].accept(this);
			add(", ");
		}
		str = str[0..$-2];
		add("]");
	}
	override void visit(StructExpression e) {
		add("struct (");
		if (e.type) e.type.accept(this);
		add(") {");
		foreach (i; 0 .. e.exprs.length) {
			add(e.members[i], ":");
			if (e.exprs[i]) e.exprs[i].accept(this);
			add(", ");
		}
		str = str[0..$-2];
		add("}");
	}
	override void visit(BlockExpression s) {

		//foreach (i; 0 .. depth) str ~= "	";
		add("{\n");
		++depth;

		foreach (n; s.nodes[0..$-1]) {
			foreach (i; 0 .. depth) str ~= "	";
			if (n) n.accept(this);
			add(";\n");
		}
		foreach (i; 0 .. depth) str ~= "	";
		if (s.nodes[$-1]) s.nodes[$-1].accept(this);
		add("\n");

		--depth;
		foreach (i; 0 .. depth) str ~= "	";
		add("}");
	}
	override void visit(IfElseExpression s) {
		add("if ");
		if (s.cond) s.cond.accept(this);
		add(" then ");
		if (s.if_body) s.if_body.accept(this);
		if (s.else_body) {
			add(" else ");
			s.else_body.accept(this);
		}
	}
	// Statement
	override void visit(WhileStatement s) {
		add("while ");
		if (s.cond) s.cond.accept(this);
		add(" then ");
		if (s.body) s.body.accept(this);
	}
	override void visit(DoWhileStatement s) {
		add("do ");
		if (s.body) s.body.accept(this);
		add("while ");
		if (s.cond) s.cond.accept(this);
		add(";");
	}
	override void visit(ForStatement s) {
		add("for ");
		if (s.init) s.init.accept(this);
		add("; ");
		if (s.cond) s.cond.accept(this);
		add("; ");
		if (s.exec) s.exec.accept(this);
		add(": ");
		if (s.body) s.body.accept(this);
	}
	override void visit(ForeachStatement s) { }
	override void visit(ForeachReverseStatement s) { }
	override void visit(BreakStatement s) {
		add("break");
		if (s.label) add(" ", s.label.name, ";");
	}
	override void visit(ContinueStatement s) {
		add("continue");
		if (s.label) add(" ", s.label.name, ";");
	}
	override void visit(ReturnStatement s) {
		add("return");
		if (s.expr) s.expr.accept(this);
	}

	// Type
	override void visit(FunctionType t) {
		add("(");
		if (t.range)  t.range. accept(this);
		add(" -> ");
		if (t.domain) t.domain.accept(this);
		add(")");
	}
	override void visit(ArrayType t) {
		add("[");
		if (t.type) t.type.accept(this);
		add("]");
	}
	override void visit(AssocArrayType t) {
		add("[");
		if (t.key)   t.key.  accept(this);
		add(":");
		if (t.value) t.value.accept(this);
		add("]");
	}
	override void visit(PointerType t) {
		add("#(");
		if (t.type) t.type.accept(this);
		add(")");
	}
	override void visit(TupleType t) {
		add("(");
		foreach (type; t.types) {
			if (type) type.accept(this);
			add(", ");
		}
		str = str[0..$-2];
		add(")");
	}
	override void visit(IdentifierType t) {
		add(t.id.name);
	}
	override void visit(PrimitiveType t) {
		add(t.tt.to!string);
	}

	// Declaration
	override void visit(LetDeclaration s) {
		add("let ");
		foreach (i; 0 .. s.ids.length) {
			add(s.ids[i].name);
			add(" @", (cast(ulong) s.ids[i].attr).to!string, " ");
			if (s.ids[i].type) { add(":"); s.ids[i].type.accept(this); }
			if (s.exprs[i]) { add(" = "); s.exprs[i].accept(this); }
			add(", ");
		}
		str = str[0 .. $-2];
	}
	override void visit(FunctionDeclaration s) {
		add("@", (cast(ulong)s.attr).to!string, " ");
		add("func ");
		if (s.id) {
			add(s.id.name);
			if (s.id.ret_type) { add(":"); s.id.ret_type.accept(this); add(" "); }
		}
		foreach (i; 0 .. s.args.length) {
			add(s.args[i].name);
			if (s.args[i].type) { add(":"); s.args[i].type.accept(this); }
			add(" ");
		}
		if (s.body) { add("= "); s.body.accept(this); }
	}
	override void visit(LabelDeclaration l) {
		add(l.name, ": ");
	}

	// others
	alias visit = typeof(super).visit;
}
