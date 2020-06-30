module ast.symbol;

import lexer, type;

class Identifier {
    string[] name;
    this (string[] n) {
        name = n;
    }
    this (string n) {
        this([n]);
    }
}

// identifier : Type
class TypedIdentifier : Identifier {
    Type type;
    this (string[] n, Type t = null) {
        super(n);
        type = t;
    }
    this (string n, Type t = null) {
        super(n);
        type = t;
    }
}
