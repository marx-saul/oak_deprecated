module test.parser;

import std.stdio, std.conv;

unittest {
    writeln("/------- parser test 1 -------/");

    import parser, lexer, ast.astbase;
    // show expression
    {
        auto lx  = new Lexer!string("(f n * (n-1), struct(Vector2){x:a![3], y:--a![0]}, [\"hello\" : 3+2], [], [#a])");
        auto ps = new Parser!(ASTBase, Lexer!string)(lx);
        auto expr = ps.expression();
        auto v = new ToStringVisitor();
        expr.accept(v);
        writeln(v.str);
    }

    // show expression
    {
        auto lx  = new Lexer!string(`{
            let:int a = 0;
            while a <= 10 : {
                writeln a;
                writeln if a % 2 == 0 : "even" else "odd"
                a.inc;
            };
            let a:int = 0, msg = "message!";
            for a = 0 ; a <= 0 ; a.inc : {
                writeln if a % 2 == 0 : "even" else "odd"
            };

            func factorial:int n:int = if n>0 : n * factorial(n-1) else 1;
            func fibonacci:int n:int = {
                let i = 1, j = 1,;
                while n > 0 : {
                    let t = i+j;
                    i = j;
                    j = t;
                    n.dec;
                };
                i
            };

            writeln app fibonacci app factorial 5
        }`);
        auto ps = new Parser!(ASTBase, Lexer!string)(lx);
        auto v = new ToStringVisitor();
        auto expr = ps.expression();
        expr.accept(v);
        writeln(v.str);
    }

    // show type
    {
        auto lx  = new Lexer!string("(a -> b) -> [a] -> [b]");
        auto ps = new Parser!(ASTBase, Lexer!string)(lx);
        auto type = ps.type();
        auto v = new ToStringVisitor();
        type.accept(v);
        writeln(v.str);
    }

}

import visitor, ast.astbase, ast.astnode;
class ToStringVisitor : PermissiveVisitor!ASTBase {
    alias AST = ASTBase;

    size_t depth;
    string str = "";
    void add(string[] ss...) {
        foreach (s; ss) str ~= s;
    }

    // Expression
    override void visit(AST.ASTNode)              { assert(0); }
    override void visit(AST.Expression)           { assert(0); }
    override void visit(AST.BinaryExpression e) {
        add("(");
        if (e.left)  e.left. accept(this);
        add(" ", e.op.to!string, " ");
        if (e.right) e.right.accept(this);
        add(")");
    }
    override void visit(AST.UnaryExpression e) {
        add(e.op.to!string);
        if (e.expr) e.expr.accept(this);
    }
    override void visit(AST.WhenExpression e) {
        add("(");
        if (e.left)   e.left.  accept(this);
        add(" when ");
        if (e.center) e.center.accept(this);
        add(" else ");
        if (e.right)  e.right. accept(this);
        add(")");
    }
    override void visit(AST.TupleExpression e) {
        add("(");
        foreach (expr; e.exprs) {
            if (expr) expr.accept(this);
            add(", ");
        }
        str = str[0..$-2];
        add(")");
    }
    override void visit(AST.IndexExpression e) {
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
    override void visit(AST.SliceExpression e) {
        add("(");
        if (e.expr) e.expr.accept(this);
        add("{");
        if (e.from) e.from.accept(this);
        add("..");
        if (e.to)   e.to.  accept(this);
        add("})");
    }
    override void visit(AST.IntegerExpression e) {
        add(e.value.to!string);
    }
    override void visit(AST.RealExpression e) {
        add(e.value.to!string);
    }
    override void visit(AST.StringExpression e) {
        add("`");
        add(e.str);
        add("`");
    }
    override void visit(AST.IdentifierExpression e) {
        add(e.id.name[0]);
    }
    override void visit(AST.DollarExpression e) { visit(cast(AST.IdentifierExpression)e); }
    override void visit(AST.ThisExpression e)   { visit(cast(AST.IdentifierExpression)e); }
    override void visit(AST.SuperExpression e)  { visit(cast(AST.IdentifierExpression)e); }
    override void visit(AST.UnitExpression e)   { add("()"); }
    override void visit(AST.ArrayExpression e) {
        add("[");
        foreach (expr; e.exprs) {
            if (expr) expr.accept(this);
            add(", ");
        }
        str = str[0..$-2];
        add("]");
    }
    override void visit(AST.AssocArrayExpression e) {
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
    override void visit(AST.StructExpression e) {
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
    override void visit(AST.BlockExpression s) {

        //foreach (i; 0 .. depth) str ~= "    ";
        add("{\n");
        ++depth;

        foreach (n; s.nodes[0..$-1]) {
            foreach (i; 0 .. depth) str ~= "    ";
            if (n) n.accept(this);
            add(";\n");
        }
        foreach (i; 0 .. depth) str ~= "    ";
        if (s.nodes[$-1]) s.nodes[$-1].accept(this);
        add("\n");

        --depth;
        foreach (i; 0 .. depth) str ~= "    ";
        add("}");
    }
    override void visit(AST.IfElseExpression s) {
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
    override void visit(AST.WhileStatement s) {
        add("while ");
        if (s.cond) s.cond.accept(this);
        add(" then ");
        if (s.body) s.body.accept(this);
    }
    override void visit(AST.DoWhileStatement s) {
        add("do ");
        if (s.body) s.body.accept(this);
        add("while ");
        if (s.cond) s.cond.accept(this);
        add(";");
    }
    override void visit(AST.ForStatement s) {
        add("for ");
        if (s.init) s.init.accept(this);
        add("; ");
        if (s.cond) s.cond.accept(this);
        add("; ");
        if (s.exec) s.exec.accept(this);
        add(": ");
        if (s.body) s.body.accept(this);
    }
    override void visit(AST.ForeachStatement s) { }
    override void visit(AST.ForeachReverseStatement s) { }
    override void visit(AST.BreakStatement s) {
        add("break");
        if (s.label) add(" ", s.label.name[0], ";");
    }
    override void visit(AST.ContinueStatement s) {
        add("continue");
        if (s.label) add(" ", s.label.name[0], ";");
    }
    override void visit(AST.ReturnStatement s) {
        add("return");
        if (s.expr) s.expr.accept(this);
    }

    // Type
    override void visit(AST.FunctionType t) {
        add("(");
        if (t.range)  t.range. accept(this);
        add(" -> ");
        if (t.domain) t.domain.accept(this);
        add(")");
    }
    override void visit(AST.ArrayType t) {
        add("[");
        if (t.type) t.type.accept(this);
        add("]");
    }
    override void visit(AST.AssocArrayType t) {
        add("[");
        if (t.key)   t.key.  accept(this);
        add(":");
        if (t.value) t.value.accept(this);
        add("]");
    }
    override void visit(AST.PointerType t) {
        add("#(");
        if (t.type) t.type.accept(this);
        add(")");
    }
    override void visit(AST.TupleType t) {
        add("(");
        foreach (type; t.types) {
            if (type) type.accept(this);
            add(", ");
        }
        str = str[0..$-2];
        add(")");
    }
    override void visit(AST.IdentifierType t) {
        add(t.id.name[0]);
    }
    override void visit(AST.PrimitiveType t) {
        add(t.tt.to!string);
    }

    // Declaration
    override void visit(AST.LetDeclaration s) {
        add("let ");
        foreach (i; 0 .. s.ids.length) {
            add(s.ids[i].name[0]);
            if (s.ids[i].type) { add(":"); s.ids[i].type.accept(this); add(" "); }
            if (s.exprs[i]) { add(" = "); s.exprs[i].accept(this); }
            add(", ");
        }
        str = str[0 .. $-2];
    }
    override void visit(AST.FunctionDeclaration s) {
        add("func ");
        if (s.id) {
            add(s.id.name[0]);
            if (s.id.type) { add(":"); s.id.type.accept(this); add(" "); }
        }
        foreach (i; 0 .. s.args.length) {
            add(s.args[i].name[0]);
            if (s.args[i].type) { add(":"); s.args[i].type.accept(this); }
            add(" ");
        }
        if (s.body) { add("= "); s.body.accept(this); }
    }


    // others
    alias visit = typeof(super).visit;
}
