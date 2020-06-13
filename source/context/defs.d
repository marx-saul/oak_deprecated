module context.defs;
import parser.defs, parser.lexer;
/+
enum SymbolType {
    module_,
    variable,
    func,
    proc,
    meta,       // meta variable for templates

    // user defined types
    struct_, class_, interface_,
}
class Symbol {
    SymbolType symbol_type;
    AST address;           // the AST the variable was declared
    bool is_argument;
    this (SymbolType t, AST a, bool i) {
        symbol_type = t, address = a, is_argument = i;
    }
}
alias SymbolTable = Symbol[string];
alias SymbolOfNode = Symbol[AST];

class SymbolInfo {
    SymbolTable table;
    SymbolOfNode symbol_of;
    this () {}
}+/
/+
enum TypeType {
    dummy,
    unknown,       // for template instancings, identifiers that are undetermined to be
    primitive,     // int, bool, etc.
    list, pointer, // [a], *a
    var,           // var a
    func,          // func
    proc,          // proc
    meta,          // meta types
    struct_,       // struct
    //class_, interface_,
}
class Type {
    Token token;
    string name;    // used for struct names, alias of types, etc...
    Node node;      // for template instances
    Type left, right; // for function type
    //this () {}
    this (Token t, Type l = null, Type r = null) {
        token = t, left = l, right = r;
    }
}
// attributes for each nodes indicating a variable
alias TypeTable = Type[Node];
+/
