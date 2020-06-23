// definition of AST that parser generates.
//
module ast.astbase;

import std.algorithm, std.array;

struct ASTBase {
    import lexer;
    import ast.astnode;
    import visitor;

    alias Vis = Visitor!ASTBase;
    abstract class ASTNode {
        abstract void accept(Vis v);
    }

    /* ************* *
        exprs
     * ************* */
    abstract class Expression:ASTNode {
        Location loc;

        this (Location l) {
            loc = l;
        }
        // shallow copy
        abstract @property Expression dup();
        // deep copy
        abstract @property Expression copy();

        override void accept(Vis v) { v.visit(this); }
    }

    final class BinaryExpression : Expression {
        Expression left, right;
        TokenType op;

        this (Location loc, TokenType o, Expression l, Expression r) {
            super(loc);
            op    = o;
            left  = l;
            right = r;
        }

        override @property BinaryExpression dup() {
            return new BinaryExpression(loc, op, left, right);
        }
        override @property BinaryExpression copy() {
            return new BinaryExpression(loc, op, left.copy, right.copy);
        }
        override void accept(Vis v) { v.visit(this); }
    }

    final class UnaryExpression : Expression {
        Expression expr;
        TokenType op;

        this (Location loc, TokenType o, Expression e) {
            super(loc);
            op    = o;
            expr  = e;
        }

        override @property UnaryExpression dup() {
            return new UnaryExpression(loc, op, expr);
        }
        override @property UnaryExpression copy() {
            return new UnaryExpression(loc, op, expr.copy);
        }
        override void accept(Vis v) { v.visit(this); }
    }

    final class WhenExpression : Expression {
        Expression left, center, right;

        this (Location loc, Expression l, Expression c, Expression r) {
            super(loc);
            left   = l;
            center = c;
            right  = r;
        }

        override @property WhenExpression dup() {
            return new WhenExpression(loc, left, center, right);
        }
        override @property WhenExpression copy() {
            return new WhenExpression(loc, left.copy, center.copy, right.copy);
        }
        override void accept(Vis v) { v.visit(this); }
    }

    final class TupleExpression : Expression {
        Expression[] exprs;

        this (Location loc, Expression[] es) {
            super(loc);
            exprs = es;
        }
        override @property TupleExpression dup() {
            return new TupleExpression(loc, exprs);
        }
        override @property TupleExpression copy() {
            auto es = new Expression[exprs.length];
            foreach (i, expr; exprs) { es[i] = expr.copy; }
            return new TupleExpression(loc, es);
        }
        override void accept(Vis v) { v.visit(this); }
    }

    final class IndexExpression : Expression {
        Expression   expr;
        Expression[] indices;

        this (Location loc, Expression e, Expression[] es) {
            super(loc);
               expr = e;
            indices = es;
        }
        override @property IndexExpression dup() {
            return new IndexExpression(loc, expr, indices);
        }
        override @property IndexExpression copy() {
            auto es = new Expression[indices.length];
            foreach (i, expr; indices) { es[i] = expr.copy; }
            return new IndexExpression(loc, expr.copy, es);
        }
        override void accept(Vis v) { v.visit(this); }
    }

    final class SliceExpression : Expression {
        Expression expr, from, to;

        this (Location loc, Expression e, Expression f, Expression t) {
            super(loc);
            expr = e;
            from = f;
              to = t;
        }
        override @property SliceExpression dup() {
            return new SliceExpression(loc, expr, from, to);
        }
        override @property SliceExpression copy() {
            return new SliceExpression(loc, expr.copy, from.copy, to.copy);
        }
        override void accept(Vis v) { v.visit(this); }
    }

    final class IntegerExpression : Expression {
        long value;
        this (Location loc, long v) {
            super(loc);
            value = v;
        }
        override @property IntegerExpression dup() {
            return new IntegerExpression(loc, value);
        }
        override @property IntegerExpression copy() {
            return new IntegerExpression(loc, value);
        }
        override void accept(Vis v) { v.visit(this); }
    }

    final class RealExpression : Expression {
        double value;
        this (Location loc, double v) {
            super(loc);
            value = v;
        }
        override @property RealExpression dup() {
            return new RealExpression(loc, value);
        }
        override @property RealExpression copy() {
            return new RealExpression(loc, value);
        }
        override void accept(Vis v) { v.visit(this); }
    }

    final class StringExpression : Expression {
        string str;
        this (Location loc, string str) {
            super(loc);
            this.str = str;
        }
        override @property StringExpression dup() {
            return new StringExpression(loc, str);
        }
        override @property StringExpression copy() {
            return new StringExpression(loc, str);
        }
        override void accept(Vis v) { v.visit(this); }
    }

    class IdentifierExpression : Expression {
        string name;
        TokenType type;
        this (Location loc, string n) {
            super(loc);
            name = n;
            type = TokenType.identifier;
        }
        override @property IdentifierExpression dup() {
            return new IdentifierExpression(loc, name);
        }
        override @property IdentifierExpression copy() {
            return new IdentifierExpression(loc, name);
        }
        override void accept(Vis v) { v.visit(this); }
    }

    final class DollarExpression : IdentifierExpression {
        this (Location loc) {
            super(loc, "$");
            type = TokenType.dollar;
        }
        override @property DollarExpression dup() {
            return new DollarExpression(loc);
        }
        override @property DollarExpression copy() {
            return new DollarExpression(loc);
        }
        override void accept(Vis v) { v.visit(this); }
    }

    final class ThisExpression : IdentifierExpression {
        this (Location loc) {
            super(loc, "this");
            type = TokenType.this_;
        }
        override @property ThisExpression dup() {
            return new ThisExpression(loc);
        }
        override @property ThisExpression copy() {
            return new ThisExpression(loc);
        }
        override void accept(Vis v) { v.visit(this); }
    }

    final class SuperExpression : IdentifierExpression {
        this (Location loc) {
            super(loc, "super");
            type = TokenType.super_;
        }
        override @property SuperExpression dup() {
            return new SuperExpression(loc);
        }
        override @property SuperExpression copy() {
            return new SuperExpression(loc);
        }
        override void accept(Vis v) { v.visit(this); }
    }

    // class



    /* ************* *
           Types
     * ************* */

    abstract class Type : ASTNode {

    }

    /*
    class StructLiteral : Expression {
        Identifier[] identifiers;
        Expression[] exprs;

        this (Token t, Identifier[] ids, Expression[] es) {
            super(t);
            identifiers = ids;
            exprs = es
        }
    }
    */

}


