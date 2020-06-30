module test.parser;

import std.stdio, std.conv;

unittest {
    writeln("/------- parser test 1 -------/");

    import parser, lexer, ast.astbase;
    // show expression
    {
        auto lx  = new Lexer!string("f n * (n-1)");
        auto ps = new Parser!(ASTBase, Lexer!string)(lx);
        auto expr = ps.expression();
        auto v = new ExpressionVisitor();
        expr.accept(v);
        writeln(v.str);
    }

    // show type
    {
        auto lx  = new Lexer!string("(a -> b) -> [a] -> [b]");
        auto ps = new Parser!(ASTBase, Lexer!string)(lx);
        auto type = ps.type();
        auto v = new TypeVisitor();
        type.accept(v);
        writeln(v.str);
    }

    // show statement
    {
        auto lx  = new Lexer!string(`{
            a = 0;
            while a <= 10 {
                writeln a;
                a.inc;
            }
            for a = 0 ; a <= 0 ; a.inc {
                if a % 2 == 0 { writeln "even" }
                else writeln "odd"
            }
        }`);
        auto ps = new Parser!(ASTBase, Lexer!string)(lx);
        auto st = ps.statement();
        auto v = new StatementVisitor();
        st.accept(v);
        writeln(v.str);
    }

}

@property string tostring(ASTBase.Expression e) {
    if (!e) return "";
    auto v = new ExpressionVisitor();
    e.accept(v);
    return v.str;
}
@property string tostring(ASTBase.Type t) {
    if (!t) return "";
    auto v = new TypeVisitor();
    t.accept(v);
    return v.str;
}
@property string tostring(ASTBase.Statement s) {
    if (!s) return "";
    auto v = new StatementVisitor();
    s.accept(v);
    return v.str;
}

import visitor, ast.astbase, ast.astnode;
class ExpressionVisitor : PermissiveVisitor!ASTBase {
    alias AST = ASTBase;

    string str = "";
    void add(string[] ss...) {
        foreach (s; ss) str ~= s;
    }

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
        write(e.op);
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
        add("\b\b)");
    }
    override void visit(AST.IndexExpression e) {
        add("(");
        if (e.expr)   e.expr.accept(this);
        add("{");
        foreach (expr; e.indices) {
            if (expr) expr.accept(this);
            add(", ");
        }
        add("\b\b})");
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

    // others
    alias visit = typeof(super).visit;
}

class TypeVisitor : PermissiveVisitor!ASTBase {
    alias AST = ASTBase;

    string str = "";
    void add(string[] ss...) {
        foreach (s; ss) str ~= s;
    }

    override void visit(AST.ASTNode) { assert(0); }
    override void visit(AST.Type)    { assert(0); }

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
        add("\b\b)");
    }
    override void visit(AST.IdentifierType t) {
        add(t.id.name[0]);
    }
    override void visit(AST.PrimitiveType t) {
        add(t.tt.to!string);
    }

    // others
    alias visit = typeof(super).visit;
}

class StatementVisitor : PermissiveVisitor!ASTBase {
    alias AST = ASTBase;

    size_t depth;
    string str;
    void add(string[] ss...) {
        foreach (s; ss) str ~= s;
    }

    override void visit(AST.ASTNode)   { assert(0); }
    override void visit(AST.Statement) { assert(0); }


    override void visit(AST.ExpressionStatement s) {
        add(s.expr.tostring, ";");
    }
    override void visit(AST.BlockStatement s) {

        foreach (i; 0 .. depth) str ~= "    ";
        add("{\n");
        ++depth;

        foreach (stmt; s.stmts) {
            foreach (i; 0 .. depth) str ~= "    ";
            stmt.accept(this);
            add("\n");
        }

        --depth;
        foreach (i; 0 .. depth) str ~= "    ";
        add("}");
    }
    override void visit(AST.IfElseStatement s) {
        add("if ");
        add(s.cond.tostring, " ");
        if (s.if_body) s.if_body.accept(this);
        if (s.else_body) {
            add(" else ");
            s.else_body.accept(this);
        }
    }
    override void visit(AST.WhileStatement s) {
        add("while ");
        add(s.cond.tostring, "\n");
        if (s.body) s.body.accept(this);
    }
    override void visit(AST.DoWhileStatement s) {
        add("do\n");
        if (s.body) s.body.accept(this);
        add("while ", s.cond.tostring, ";");
    }
    override void visit(AST.ForStatement s) {
        add("for ");
        if (s.init) s.init.accept(this);
        add(s.cond.tostring, "; ");
        add(s.exec.tostring, "\n");
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
        if (s.expr) add(s.expr.tostring);
    }

    // others
    alias visit = typeof(super).visit;
}
