module test.parser;

import std.stdio;

unittest {
    writeln("/------- parser test 1 -------/");

    import parser, lexer, ast.astbase;
    auto lx  = new Lexer!string("f n * (n-1)");
    auto ps = new Parser!(ASTBase, Lexer!string)(lx);
    auto exp = ps.expression();

    auto v = new TestVisitor();
    exp.accept(v);
    writeln();
}

import visitor, ast.astbase, ast.astnode;
class TestVisitor : Visitor!ASTBase {
    alias AST = ASTBase;
    override void visit(AST.ASTNode)              { assert(0); }
    override void visit(AST.Expression)           { assert(0); }
    override void visit(AST.BinaryExpression e) {
        write("(");
        if (e.left)  e.left. accept(this);
        write(" ", e.op, " ");
        if (e.right) e.right.accept(this);
        write(")");
    }
    override void visit(AST.UnaryExpression e) {
        write(e.op);
        if (e.expr) e.expr.accept(this);
    }
    override void visit(AST.WhenExpression e) {
        write("(");
        if (e.left)   e.left.  accept(this);
        write(" when ");
        if (e.center) e.center.accept(this);
        write(" else ");
        if (e.right)  e.right. accept(this);
        write(")");
    }
    override void visit(AST.TupleExpression e) {
        write("(");
        foreach (expr; e.exprs) {
            if (expr) expr.accept(this);
            write(", ");
        }
        write(")");
    }
    override void visit(AST.IndexExpression e) {
        write("(");
        if (e.expr)   e.expr.accept(this);
        write("{");
        foreach (expr; e.indices) {
            if (expr) expr.accept(this);
            write(", ");
        }
        write("})");
    }
    override void visit(AST.SliceExpression e) {
        write("(");
        if (e.expr) e.expr.accept(this);
        write("{");
        if (e.from) e.from.accept(this);
        write("..");
        if (e.to)   e.to.  accept(this);
        write("})");
    }
    override void visit(AST.IntegerExpression e) {
        write(e.value);
    }
    override void visit(AST.RealExpression e) {
        write(e.value);
    }
    override void visit(AST.StringExpression e) {
        write("`");
        write(e.str);
        write("`");
    }
    override void visit(AST.IdentifierExpression e) {
        write(e.name);
    }
    override void visit(AST.DollarExpression e) { visit(cast(AST.IdentifierExpression)e); }
    override void visit(AST.ThisExpression e)   { visit(cast(AST.IdentifierExpression)e); }
    override void visit(AST.SuperExpression e)  { visit(cast(AST.IdentifierExpression)e); }
}
