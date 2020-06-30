// definition of AST that parser generates.
//
module ast.astbase;

import std.algorithm, std.array;

struct ASTBase {
    import ast.astnode, ast.symbol;
    import lexer;
    import visitor;

    enum Attrbute : ulong {
        dummy      = 0UL,

        shadow     = 1UL << 0,
        ref_       = 1UL << 1,

        private_   = 1UL << 2,
        protected_ = 1UL << 3,
        package_   = 1UL << 4,
        public_    = 1UL << 5,
        export_    = 1UL << 6,
        abstract_  = 1UL << 7,
        override_  = 1UL << 8,
    }

    alias Vis = Visitor!ASTBase;
    abstract class ASTNode {
        abstract void accept(Vis v);
    }

    /* *************************************** *
                     Expression
     * *************************************** */
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
        Identifier id;
        TokenType type;
        this (Location loc, Identifier i) {
            super(loc);
            id = i;
            type = TokenType.identifier;
        }
        override @property IdentifierExpression dup() {
            return new IdentifierExpression(loc, id);
        }
        override @property IdentifierExpression copy() {
            return new IdentifierExpression(loc, id);
        }
        override void accept(Vis v) { v.visit(this); }
    }

    final class DollarExpression : IdentifierExpression {
        this (Location loc, Identifier i) {
            super(loc, i);
            type = TokenType.dollar;
        }
        override @property DollarExpression dup() {
            return new DollarExpression(loc, id);
        }
        override @property DollarExpression copy() {
            return new DollarExpression(loc, id);
        }
        override void accept(Vis v) { v.visit(this); }
    }

    final class ThisExpression : IdentifierExpression {
        this (Location loc, Identifier i) {
            super(loc, i);
            type = TokenType.this_;
        }
        override @property ThisExpression dup() {
            return new ThisExpression(loc, id);
        }
        override @property ThisExpression copy() {
            return new ThisExpression(loc, id);
        }
        override void accept(Vis v) { v.visit(this); }
    }

    final class SuperExpression : IdentifierExpression {
        this (Location loc, Identifier i) {
            super(loc, i);
            type = TokenType.super_;
        }
        override @property SuperExpression dup() {
            return new SuperExpression(loc, id);
        }
        override @property SuperExpression copy() {
            return new SuperExpression(loc, id);
        }
        override void accept(Vis v) { v.visit(this); }
    }

    final class UnitExpression : Expression {
        this (Location l) {
            super(l);
        }
        override @property UnitExpression dup() {
            return new UnitExpression(loc);
        }
        override @property UnitExpression copy() {
            return new UnitExpression(loc);
        }
        override void accept(Vis v) { v.visit(this); }
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
    /*
    class LambdaLiteral : Expression {
    }
    (/
    /* *************************************** *
                       Type
     * *************************************** */
    abstract class Type : ASTNode {
        Location loc;

        this (Location l) {
            loc = l;
        }
        // shallow copy
        abstract @property Type dup();
        // deep copy
        abstract @property Type copy();

        override void accept(Vis v) { v.visit(this); }
    }

    final class FunctionType : Type {
        Type range;
        Type domain;

        this (Location l, Type r, Type d) {
            super(l);
            range  = r;
            domain = d;
        }
        override @property FunctionType dup() {
            return new FunctionType(loc, range, domain);
        }
        override @property FunctionType copy() {
            return new FunctionType(loc, range.copy, domain.copy);
        }
        override void accept(Vis v) { v.visit(this); }
    }

    final class ArrayType : Type {
        Type type;

        this (Location l, Type t) {
            super(l);
            type = t;
        }
        override @property ArrayType dup() {
            return new ArrayType(loc, type);
        }
        override @property ArrayType copy() {
            return new ArrayType(loc, type.copy);
        }
        override void accept(Vis v) { v.visit(this); }
    }

    final class AssocArrayType : Type {
        Type key;
        Type value;

        this (Location l, Type k, Type v) {
            super(l);
            key = k;
            value = v;
        }
        override @property AssocArrayType dup() {
            return new AssocArrayType(loc, key, value);
        }
        override @property AssocArrayType copy() {
            return new AssocArrayType(loc, key.copy, value.copy);
        }
        override void accept(Vis v) { v.visit(this); }
    }

    final class PointerType : Type {
        Type type;

        this (Location l, Type t) {
            super(l);
            type = t;
        }
        override @property Type dup() {
            return new PointerType(loc, type);
        }
        override @property Type copy() {
            return new PointerType(loc, type.copy);
        }
        override void accept(Vis v) { v.visit(this); }
    }

    final class TupleType : Type {
        Type[] types;

        this (Location l, Type[] ts) {
            super(l);
            types = ts;
        }
        override @property TupleType dup() {
            return new TupleType(loc, types);
        }
        override @property TupleType copy() {
            auto ts = new Type[types.length];
            foreach (i, t; types) { ts[i] = types[i].copy; }
            return new TupleType(loc, ts);
        }
        override void accept(Vis v) { v.visit(this); }
    }

    // int, real, bool, ...
    final class IdentifierType : Type {
        Identifier id;
        this (Location l, Identifier i) {
            super(l);
            id = i;
        }
        override @property IdentifierType dup() {
            return new IdentifierType(loc, id);
        }
        override @property IdentifierType copy() {
            return new IdentifierType(loc, id);
        }
        override void accept(Vis v) { v.visit(this); }
    }

    // int, real, bool, ...
    final class PrimitiveType : Type {
        TokenType tt;
        this (Location l, TokenType t) {
            super(l);
            tt = t;
        }
        override @property PrimitiveType dup() {
            return new PrimitiveType(loc, tt);
        }
        override @property PrimitiveType copy() {
            return new PrimitiveType(loc, tt);
        }
        override void accept(Vis v) { v.visit(this); }
    }

    /* *************************************** *
                       Statement
     * *************************************** */
    enum Stmt {
        error,

        block,

        expression,

        if_,
        do_,
        while_,
        for_,
        foreach_,
        foreach_reverse_,
        break_,
        continue_,
        return_,
    }

    abstract class Statement : ASTNode {
        Location loc;
        Stmt stmt;
        this (Location l, Stmt s) {
            loc = l;
            stmt = s;
        }
        // shallow copy
        abstract @property Statement dup();
        // deep copy
        abstract @property Statement copy();

        override void accept(Vis v) { v.visit(this); }
    }

    final class ExpressionStatement : Statement {
        Expression expr;
        this (Location l, Expression e) {
            super(l, Stmt.expression);
            expr = e;
        }

        override @property ExpressionStatement dup() {
            return new ExpressionStatement(loc, expr);
        }
        override @property ExpressionStatement copy() {
            return new ExpressionStatement(loc, expr.copy);
        }

        override void accept(Vis v) { v.visit(this); }
    }

    final class BlockStatement : Statement {
        Statement[] stmts;
        this (Location l, Statement[] ss) {
            super(l, Stmt.block);
            stmts = ss;
        }

        override @property BlockStatement dup() {
            return new BlockStatement(loc, stmts);
        }
        override @property BlockStatement copy() {
            auto ss = new Statement[stmts.length];
            foreach (i, s; ss) { ss[i] = s.copy; }
            return new BlockStatement(loc, ss);
        }

        override void accept(Vis v) { v.visit(this); }
    }

    final class IfElseStatement : Statement {
        Expression cond;
        Statement if_body;
        Statement else_body;
        this (Location l, Expression c, Statement i, Statement e) {
            super(l, Stmt.if_);
            cond = c;
            if_body = i;
            else_body = e;
        }

        override @property IfElseStatement dup() {
            return new IfElseStatement(loc, cond, if_body, else_body);
        }
        override @property IfElseStatement copy() {
            return new IfElseStatement(loc, cond.copy, if_body.copy, else_body.copy);
        }

        override void accept(Vis v) { v.visit(this); }
    }

    final class WhileStatement : Statement {
        Expression cond;
        Statement body;
        this (Location l, Expression c, Statement b) {
            super(l, Stmt.if_);
            cond = c;
            body = b;
        }

        override @property WhileStatement dup() {
            return new WhileStatement(loc, cond, body);
        }
        override @property WhileStatement copy() {
            return new WhileStatement(loc, cond, body.copy);
        }

        override void accept(Vis v) { v.visit(this); }
    }

    final class DoWhileStatement : Statement {
        Statement body;
        Expression cond;
        this (Location l, Statement b, Expression c) {
            super(l, Stmt.if_);
            body = b;
            cond = c;
        }

        override @property DoWhileStatement dup() {
            return new DoWhileStatement(loc, body, cond);
        }
        override @property DoWhileStatement copy() {
            return new DoWhileStatement(loc, body.copy, cond.copy);
        }

        override void accept(Vis v) { v.visit(this); }
    }

    final class ForStatement : Statement {
        Statement init;
        Expression cond;
        Expression exec;
        Statement body;
        this (Location l, Statement i, Expression c, Expression e, Statement b) {
            super(l, Stmt.for_);
            init = i;
            cond = c;
            exec = e;
            body = b;
        }

        override @property ForStatement dup() {
            return new ForStatement(loc, init, cond, exec, body);
        }
        override @property ForStatement copy() {
            return new ForStatement(loc, init.copy, cond.copy, exec.copy, body.copy);
        }

        override void accept(Vis v) { v.visit(this); }
    }

    final class ForeachStatement : Statement {
        TypedIdentifier[] t_ids;
        Expression expr;
        Statement body;
        this (Location l, TypedIdentifier[] t, Expression e, Statement b) {
            super(l, Stmt.foreach_);
            t_ids = t;
            expr = e;
            body = b;
        }

        override @property ForeachStatement dup() {
            return new ForeachStatement(loc, t_ids, expr, body);
        }
        override @property ForeachStatement copy() {
            return new ForeachStatement(loc, t_ids, expr.copy, body.copy);
        }

        override void accept(Vis v) { v.visit(this); }
    }

    final class ForeachReverseStatement : Statement {
        TypedIdentifier[] t_ids;
        Expression expr;
        Statement body;
        this (Location l, TypedIdentifier[] t, Expression e, Statement b) {
            super(l, Stmt.foreach_reverse_);
            t_ids = t;
            expr = e;
            body = b;
        }

        override @property ForeachReverseStatement dup() {
            return new ForeachReverseStatement(loc, t_ids, expr, body);
        }
        override @property ForeachReverseStatement copy() {
            return new ForeachReverseStatement(loc, t_ids, expr.copy, body.copy);
        }

        override void accept(Vis v) { v.visit(this); }
    }

    final class BreakStatement : Statement {
        Identifier label;
        this (Location l, Identifier lbl) {
            super(l, Stmt.break_);
            label = lbl;
        }

        override @property BreakStatement dup() {
            return new BreakStatement(loc, label);
        }
        override @property BreakStatement copy() {
            return new BreakStatement(loc, label);
        }

        override void accept(Vis v) { v.visit(this); }
    }

    final class ContinueStatement : Statement {
        Identifier label;
        this (Location l, Identifier lbl) {
            super(l, Stmt.continue_);
            label = lbl;
        }

        override @property ContinueStatement dup() {
            return new ContinueStatement(loc, label);
        }
        override @property ContinueStatement copy() {
            return new ContinueStatement(loc, label);
        }

        override void accept(Vis v) { v.visit(this); }
    }

    final class ReturnStatement : Statement {
        Expression expr;
        this (Location l, Expression e) {
            super(l, Stmt.return_);
            expr = e;
        }

        override @property ReturnStatement dup() {
            return new ReturnStatement(loc, expr);
        }
        override @property ReturnStatement copy() {
            return new ReturnStatement(loc, expr.copy);
        }

        override void accept(Vis v) { v.visit(this); }
    }

}


