module ast.symbol;

import lexer, type, attribute;

// a.b.c is ["c", "b", "a"]
class Identifier {
    string[] name;
    this (string[] n) {
        name = n;
    }
    this (string n) {
        this([n]);
    }
}

