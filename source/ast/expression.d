module ast.expression;

import parser.lexer, ast.astnode, ast.symbol, ast.type, visitor.visitor;

abstract class Expression: ASTNode {
    Location loc;

    this(Location l) {
        loc = l;
    }

    // shallow copy
    override @property Expression dup() {
        return this;
    }
    // deep copy
    override @property Expression copy() {
        return this;
    }
    override void accept(Visitor v) {
        v.visit(this);
    }
}

final class BinaryExpression: Expression {
    Expression left, right;
    TokenType op;

    this(Location loc, TokenType o, Expression l, Expression r) {
        super(loc);
        op = o;
        left = l;
        right = r;
    }

    override @property BinaryExpression dup() {
        return new BinaryExpression(loc, op, left, right);
    }
    override @property BinaryExpression copy() {
        return new BinaryExpression(loc, op, left.copy, right.copy);
    }
    override void accept(Visitor v) {
        v.visit(this);
    }
}

final class UnaryExpression: Expression {
    Expression expr;
    TokenType op;

    this(Location loc, TokenType o, Expression e) {
        super(loc);
        op = o;
        expr = e;
    }

    override @property UnaryExpression dup() {
        return new UnaryExpression(loc, op, expr);
    }
    override @property UnaryExpression copy() {
        return new UnaryExpression(loc, op, expr.copy);
    }
    override void accept(Visitor v) {
        v.visit(this);
    }
}

final class WhenExpression: Expression {
    Expression left, center, right;

    this(Location loc, Expression l, Expression c, Expression r) {
        super(loc);
        left = l;
        center = c;
        right = r;
    }

    override @property WhenExpression dup() {
        return new WhenExpression(loc, left, center, right);
    }
    override @property WhenExpression copy() {
        return new WhenExpression(loc, left.copy, center.copy, right.copy);
    }
    override void accept(Visitor v) {
        v.visit(this);
    }
}

final class TupleExpression: Expression {
    Expression[] exprs;

    this(Location loc, Expression[] es) {
        super(loc);
        exprs = es;
    }
    override @property TupleExpression dup() {
        return new TupleExpression(loc, exprs);
    }
    override @property TupleExpression copy() {
        auto es = new Expression[exprs.length];
        foreach(i, expr; exprs) {
            es[i] = expr.copy;
        }
        return new TupleExpression(loc, es);
    }
    override void accept(Visitor v) {
        v.visit(this);
    }
}

final class IndexExpression: Expression {
    Expression expr;
    Expression[] indices;

    this(Location loc, Expression e, Expression[] es) {
        super(loc);
        expr = e;
        indices = es;
    }
    override @property IndexExpression dup() {
        return new IndexExpression(loc, expr, indices);
    }
    override @property IndexExpression copy() {
        auto es = new Expression[indices.length];
        foreach(i, expr; indices) {
            es[i] = expr.copy;
        }
        return new IndexExpression(loc, expr.copy, es);
    }
    override void accept(Visitor v) {
        v.visit(this);
    }
}

final class SliceExpression: Expression {
    Expression expr, from, to;

    this(Location loc, Expression e, Expression f, Expression t) {
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
    override void accept(Visitor v) {
        v.visit(this);
    }
}

final class IntegerExpression: Expression {
    long value;
    this(Location loc, long v) {
        super(loc);
        value = v;
    }
    override @property IntegerExpression dup() {
        return new IntegerExpression(loc, value);
    }
    override @property IntegerExpression copy() {
        return new IntegerExpression(loc, value);
    }
    override void accept(Visitor v) {
        v.visit(this);
    }
}

final class RealExpression: Expression {
    double value;
    this(Location loc, double v) {
        super(loc);
        value = v;
    }
    override @property RealExpression dup() {
        return new RealExpression(loc, value);
    }
    override @property RealExpression copy() {
        return new RealExpression(loc, value);
    }
    override void accept(Visitor v) {
        v.visit(this);
    }
}

final class StringExpression: Expression {
    string str;
    this(Location loc, string str) {
        super(loc);
        this.str = str;
    }
    override @property StringExpression dup() {
        return new StringExpression(loc, str);
    }
    override @property StringExpression copy() {
        return new StringExpression(loc, str);
    }
    override void accept(Visitor v) {
        v.visit(this);
    }
}

class IdentifierExpression: Expression {
    Symbol.Identifier id;
    TokenType type;
    this(Location loc, Symbol.Identifier i) {
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
    override void accept(Visitor v) {
        v.visit(this);
    }
}

final class DollarExpression: IdentifierExpression {
    this(Location loc, Symbol.Identifier i) {
        super(loc, i);
        type = TokenType.dollar;
    }
    override @property DollarExpression dup() {
        return new DollarExpression(loc, id);
    }
    override @property DollarExpression copy() {
        return new DollarExpression(loc, id);
    }
    override void accept(Visitor v) {
        v.visit(this);
    }
}

final class ThisExpression: IdentifierExpression {
    this(Location loc, Symbol.Identifier i) {
        super(loc, i);
        type = TokenType.this_;
    }
    override @property ThisExpression dup() {
        return new ThisExpression(loc, id);
    }
    override @property ThisExpression copy() {
        return new ThisExpression(loc, id);
    }
    override void accept(Visitor v) {
        v.visit(this);
    }
}

final class SuperExpression: IdentifierExpression {
    this(Location loc, Symbol.Identifier i) {
        super(loc, i);
        type = TokenType.super_;
    }
    override @property SuperExpression dup() {
        return new SuperExpression(loc, id);
    }
    override @property SuperExpression copy() {
        return new SuperExpression(loc, id);
    }
    override void accept(Visitor v) {
        v.visit(this);
    }
}

final class UnitExpression: Expression {
    this(Location l) {
        super(l);
    }
    override @property UnitExpression dup() {
        return new UnitExpression(loc);
    }
    override @property UnitExpression copy() {
        return new UnitExpression(loc);
    }
    override void accept(Visitor v) {
        v.visit(this);
    }
}

final class ArrayExpression: Expression {
    Expression[] exprs;

    this(Location loc, Expression[] es) {
        super(loc);
        exprs = es;
    }
    override @property ArrayExpression dup() {
        return new ArrayExpression(loc, exprs);
    }
    override @property ArrayExpression copy() {
        auto es = new Expression[exprs.length];
        foreach(i, expr; exprs) {
            es[i] = expr.copy;
        }
        return new ArrayExpression(loc, es);
    }
    override void accept(Visitor v) {
        v.visit(this);
    }
}

final class AssocArrayExpression: Expression {
    Expression[] keys;
    Expression[] values;

    this(Location loc, Expression[] ks, Expression[] vs) {
        super(loc);
        keys = ks;
        values = vs;
    }
    override @property AssocArrayExpression dup() {
        return new AssocArrayExpression(loc, keys, values);
    }
    override @property AssocArrayExpression copy() {
        auto ks = new Expression[keys.length];
        auto vs = new Expression[values.length];
        foreach(i, key; keys) {
            ks[i] = key.copy;
        }
        foreach(i, value; keys) {
            vs[i] = value.copy;
        }
        return new AssocArrayExpression(loc, ks, vs);
    }
    override void accept(Visitor v) {
        v.visit(this);
    }
}

final class StructExpression: Expression {
    Type type;
    string[] members;
    Expression[] exprs;

    this(Location loc, Type t, string[] ms, Expression[] es) {
        super(loc);
        type = t;
        members = ms;
        exprs = es;
    }
    override @property StructExpression dup() {
        return new StructExpression(loc, type, members, exprs);
    }
    override @property StructExpression copy() {
        auto es = new Expression[exprs.length];
        foreach(i, expr; exprs) {
            es[i] = expr.copy;
        }
        return new StructExpression(loc, type.copy, members.dup, es);
    }
    override void accept(Visitor v) {
        v.visit(this);
    }
}
/*
final class LambdaLiteral : Expression {

}
*/

final class IfElseExpression: Expression {
    Expression cond;
    Expression if_body;
    Expression else_body;
    this(Location l, Expression c, Expression i, Expression e) {
        super(l);
        cond = c;
        if_body = i;
        else_body = e;
    }

    override @property IfElseExpression dup() {
        return new IfElseExpression(loc, cond, if_body, else_body);
    }
    override @property IfElseExpression copy() {
        return new IfElseExpression(loc, cond.copy, if_body.copy, else_body.copy);
    }

    override void accept(Visitor v) {
        v.visit(this);
    }
}

final class BlockExpression: Expression {
    ASTNode[] nodes; // ; separated expressions, delcarations, etc...
    this(Location l, ASTNode[] ns) {
        super(l);
        nodes = ns;
    }

    override @property BlockExpression dup() {
        return new BlockExpression(loc, nodes);
    }
    override @property BlockExpression copy() {
        auto ns = new ASTNode[nodes.length];
        foreach(i, n; nodes) {
            ns[i] = n.copy;
        }
        return new BlockExpression(loc, ns);
    }

    override void accept(Visitor v) {
        v.visit(this);
    }
}
