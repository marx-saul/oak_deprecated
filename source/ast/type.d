module ast.type;

import parser.lexer, ast.all, visitor.visitor;

abstract class Type: ASTNode {
    Location loc;
    Attribute attr;

    this(Location l, Attribute a = Attribute.none) {
        loc = l;
        attr = a;
    }

    // shallow copy
    override @property Type dup() {
        return this;
    }
    // deep copy
    override @property Type copy() {
        return this;
    }
    override void accept(Visitor v) {
        v.visit(this);
    }
}

final class FunctionType: Type {
    Type range;
    Type domain;

    this(Location l, Type r, Type d) {
        super(l);
        range = r;
        domain = d;
    }
    override @property FunctionType dup() {
        return new FunctionType(loc, range, domain);
    }
    override @property FunctionType copy() {
        return new FunctionType(loc, range.copy, domain.copy);
    }
    override void accept(Visitor v) {
        v.visit(this);
    }
}

final class ArrayType: Type {
    Type type;

    this(Location l, Type t) {
        super(l);
        type = t;
    }
    override @property ArrayType dup() {
        return new ArrayType(loc, type);
    }
    override @property ArrayType copy() {
        return new ArrayType(loc, type.copy);
    }
    override void accept(Visitor v) {
        v.visit(this);
    }
}

final class AssocArrayType: Type {
    Type key;
    Type value;

    this(Location l, Type k, Type v) {
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
    override void accept(Visitor v) {
        v.visit(this);
    }
}

final class PointerType: Type {
    Type type;

    this(Location l, Type t) {
        super(l);
        type = t;
    }
    override @property Type dup() {
        return new PointerType(loc, type);
    }
    override @property Type copy() {
        return new PointerType(loc, type.copy);
    }
    override void accept(Visitor v) {
        v.visit(this);
    }
}

final class DotType: Type {
    Type left;
    Type right;

    this(Location l, Type le, Type r) {
        super(l);
        left = le;
        right = r;
    }
    override @property FunctionType dup() {
        return new FunctionType(loc, left, right);
    }
    override @property FunctionType copy() {
        return new FunctionType(loc, left.copy, right.copy);
    }
    override void accept(Visitor v) {
        v.visit(this);
    }
}

final class TupleType: Type {
    Type[] types;

    this(Location l, Type[] ts) {
        super(l);
        types = ts;
    }
    override @property TupleType dup() {
        return new TupleType(loc, types);
    }
    override @property TupleType copy() {
        auto ts = new Type[types.length];
        foreach(i, t; types) {
            ts[i] = types[i].copy;
        }
        return new TupleType(loc, ts);
    }
    override void accept(Visitor v) {
        v.visit(this);
    }
}

// user defined type
final class IdentifierType: Type {
    Symbol.Identifier id;
    this(Location l, Symbol.Identifier i) {
        super(l);
        id = i;
    }
    override @property IdentifierType dup() {
        return new IdentifierType(loc, id);
    }
    override @property IdentifierType copy() {
        return new IdentifierType(loc, id);
    }
    override void accept(Visitor v) {
        v.visit(this);
    }
}

// int, real, bool, ...
final class PrimitiveType: Type {
    TokenType tt;
    this(Location l, TokenType t) {
        super(l);
        tt = t;
    }
    override @property PrimitiveType dup() {
        return new PrimitiveType(loc, tt);
    }
    override @property PrimitiveType copy() {
        return new PrimitiveType(loc, tt);
    }
    override void accept(Visitor v) {
        v.visit(this);
    }
}

/+
import lexer, attribute, ast.astnode, visitor;
import ast.astcodegen: ASTCodeGen;
alias Visitor = Visitoritor!ASTCodeGen;

enum PT {
    error,

    int32, uint32, int64, uint64, int128, uint128,
    float32, float64, float128,
    bool_, unit,

    struct_, union_, class_,

    func,
    array,
    assoc_array,
    pointer,
    tuple,
}

abstract class Type : ASTNode {
    Attribute attr;
    PT pt;
    this (Attribute a, PT p) {
        attr = a;
        pt = t;
    }

    private {
        Type ito;   // immut
        Type cto;   // const
        Type wto;   // inout
        Type wcto;  // inout const
    }

    final Type immutOf() @property {
        if (ito) return ito;
        else return ito = makeImmut();
    }
    final Type constOf() @property {
        if (cto) return cto;
        else return cto = makeConst();
    }
    final Type inoutOf() @property {
        if (wto) return wto;
        else return wto = makeInout();
    }
    final Type inoutConstOf() @property {
        if (wcto) return wcto;
        else return wcto = makeInoutConst();
    }
    private abstract {
        Type makeImmut();
        Type makeConst();
        Type makeInout();
        Type makeInoutConst();
    }
}

final class FunctionType : Type {
    Type range;
    Type domain;
    this (Attribute a, Type r, Type d) {
        super(a, PT.func);
        range = r;
        domain = d;
    }

    override FunctionType makeImmut() {
        return new FunctionType(attr | Attribute.immut, range, domain);
    }
    override FunctionType makeConst() {
        return new FunctionType(attr | Attribute.const_, range, domain);
    }
    override FunctionType makeInout() {
        return new FunctionType(attr | Attribute.inout_, range, domain);
    }
    override FunctionType makeInoutConst() {
        return new FunctionType(attr | Attribute.inout_ | Attribute.const_, range, domain);
    }
    override accept(Visitor v) { return v.accept(this); }
}

final class ArrayType : Type {
    Type type;
    this (Attribute a, Type t) {
        super(a, PT.array);
        type = t;
    }

    override ArrayType makeImmut() {
        return new ArrayType(attr | Attribute.immut, type);
    }
    override ArrayType makeConst() {
        return new ArrayType(attr | Attribute.const_, type);
    }
    override ArrayType makeInout() {
        return new ArrayType(attr | Attribute.inout_, type);
    }
    override ArrayType makeInoutConst() {
        return new ArrayType(attr | Attribute.inout_ | Attribute.const_, type);
    }
    override accept(Visitor v) { return v.accept(this); }
}

final class AssocArrayType : Type {
    Type key;
    Type value;
    this (Attribute a, Type k, Type v) {
        super(a, PT.assoc_array);
        key = k;
        value = v;
    }

    override AssocArrayType makeImmut() {
        return new AssocArrayType(attr | Attribute.immut, key, value);
    }
    override AssocArrayType makeConst() {
        return new AssocArrayType(attr | Attribute.const_, key, value);
    }
    override AssocArrayType makeInout() {
        return new AssocArrayType(attr | Attribute.inout_, key, value);
    }
    override AssocArrayType makeInoutConst() {
        return new AssocArrayType(attr | Attribute.inout_ | Attribute.const_, key, value);
    }
    override accept(Visitor v) { return v.accept(this); }
}

final class PrimitiveType : Type {
    this (Attribute a, PT p) {
        super(a, p);
    }
    override accept(Visitor v) { return v.accept(this); }
}
+/

