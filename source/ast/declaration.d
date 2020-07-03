module ast.declaration;

import parser.lexer, ast.all, visitor.visitor;

final class LetDeclaration: ASTNode {
    Location loc;
    Symbol.Variable[] ids;
    Expression[] exprs;

    this(Location l, Symbol.Variable[] i_s, Expression[] es) {
        loc = l;
        ids = i_s;
        exprs = es;
    }

    override @property LetDeclaration dup() {
        return new LetDeclaration(loc, ids, exprs);
    }
    override @property LetDeclaration copy() {
        auto es = new Expression[exprs.length];
        foreach(i, e; exprs) {
            es[i] = e;
        }
        return new LetDeclaration(loc, ids, es);
    }

    override void accept(Visitor v) {
        v.visit(this);
    }
}

final class FunctionDeclaration: ASTNode {
    Location loc;
    Attribute attr;
    Symbol.Function id;
    Symbol.Variable[] args;
    Expression body;

    this(Location l, Attribute a, Symbol.Function i, Symbol.Variable[] as, Expression b) {
        loc = l;
        attr = a;
        id = i;
        args = as;
        body = b;
    }

    override @property FunctionDeclaration dup() {
        return new FunctionDeclaration(loc, attr, id, args, body);
    }
    override @property FunctionDeclaration copy() {
        return new FunctionDeclaration(loc, attr, id, args, body.copy);
    }

    override void accept(Visitor v) {
        v.visit(this);
    }
}

final class LabelDeclaration: ASTNode {
    Location loc;
    string name;

    this(Location l, string n) {
        loc = l;
        name = n;
    }

    override @property LabelDeclaration dup() {
        return new LabelDeclaration(loc, name);
    }
    override @property LabelDeclaration copy() {
        return new LabelDeclaration(loc, name);
    }

    override void accept(Visitor v) {
        v.visit(this);
    }
}

final class StructDeclaration: ASTNode {
    Location loc;
    string name;
    ASTNode[] members; // declaration of members

    this(Location l, string n, ASTNode[] ms) {
        loc = l;
        name = n;
        members = ms;
    }

    override @property StructDeclaration dup() {
        return new StructDeclaration(loc, name, members);
    }
    override @property StructDeclaration copy() {
        auto ms = new ASTNode[members.length];
        foreach(i, m; members) {
            ms[i] = m.copy;
        }
        return new StructDeclaration(loc, name, ms);
    }

    override void accept(Visitor v) {
        v.visit(this);
    }
}
