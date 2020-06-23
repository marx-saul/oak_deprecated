module context.defs;
/+import aatree, std.typecons, parser.defs, parser.lexer;

template Attribute(T) {
    alias Attribute = T[AST];
}

// the block where the statement belong to.
// here, block means BlockStatement, StructDeclaration, ...
alias BlockBelongTo = Attribute!AST;

/* ********************************** */
// Symbols
enum SymbolType {
    dummy, // unknown symbols
    module_,
    variable,
    func,
    //proc,
    meta,       // meta variable for templates

    // user defined types
    struct_, class_, interface_,
}
static const tokenType_to_symbolType = new AATree!(TokenType, (a,b) => a<b, SymbolType) (
    tuple(TokenType.identifier,  SymbolType.variable),
    tuple(TokenType.func,        SymbolType.func),
    tuple(TokenType.proc,        SymbolType.func),
    tuple(TokenType.struct_,     SymbolType.struct_),
    tuple(TokenType.class_,      SymbolType.class_),
    tuple(TokenType.interface_,  SymbolType.interface_),
);
class Symbol {
    SymbolType symbol_type;
    // the address of the symbol token; for struct/class... address is the whole AST of the definition.
    Identifier address;
    // how many symbols were declared within the same scope.
    size_t index;
    this (SymbolType t, Identifier a, size_t i) {
        symbol_type = t, address = a, index = i;
    }
}
// for a block B, scope_symbols[B].values contains all symbols declared in B
// `scope_symbols[B].keys` contains the strings of the symbols. That is,
// `scope_symbols[B][x].address.name.str = x` always holds for any string x.
// `scope_symbols[B][x].index = n` means that `x` is the n-th symbol in the block B
// Note: a function argument is defined to belong to the block statement of the function.
alias ScopeSymbols = Attribute!(Symbol[string]);
// for an AST a in the block B (= block_belong_to[a]).
// Then a can access the symbol s if and only if `scope_symbols_indices[a] >= s.index`.
alias ScopeSymbolsIndices = Attribute!size_t;

/* ********************************** */
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


/+ template of an L-attribute evaluation
void dfs(AST node, AST belong_to, Attribute!T att) {
    if (node is null) return;

    with (ASTType) switch (node.ast_type) {
    case dummy:
    //assert(0, "Dummy AST found");
        writeln("dummy AST found");
    break;

    case expr:
        with (TokenType)
        if (node.token.type == when) {
            auto when_expr = cast (WhenExpression) node;
        }
        else if (node.token.type == comma) {
            auto tuple_expr = cast (TupleExpression) node;
        }
        else if (node.token.type == struct_) {
            auto struct_literal = cast (StructLiteral) node;
        }
        else if (node.token.type == lBrack) {
            auto array_literal = cast (ArrayLiteral) node;
        }
        else if (node.token.type == colon) {
            auto assoc_array_literal = cast (AssocArrayLiteral) node;
        }
        else if (node.token.type == unary_op) {
            auto expression = cast (BinaryExpression) node;
        }
        else if (node.token.type.among!(
            assign, cat_assign, add_assign, sub_assign, mul_assign, div_assign, mod_assign,
            sharp,  pipeline,  or,  and,  ls, leq, gt, geq, eq, neq,
            indexing,  add, sub, mul, div, mod,  pow,  app,  composition,  dot,
            template_instance_type, template_instance_expr,
        )) {
            auto expression = cast (BinaryExpression) node;
        }
        else if (node.token.type == identifier) {
        }
    break;

    case type:
        with (TokenType)
        if (node.token.type == comma) {
            auto tuple_type = cast (TupleType) node;
        }
        else if (node.token.type.among!(
            template_instance_type, template_instance_expr,
            right_arrow,
            var,
            lBrack, mul,
        )) {
            auto type = cast (BinaryExpression) node;
        }
        else if (node.token.type == identifier) {
        }
    break;

    case identifier:
        with (TokenType)
        if (node.token.type.among!(func, proc)) {
            auto func_node = cast (Function) node;

        }
        else if (node.token.type.among!(struct_, class_, interface_)) {
            auto struct_node = cast (Struct) node;

        }
        else {
            auto id_node = cast (Identifier) node;
        }
    break;

    case let:
        auto let_node = cast (LetDeclaration) node;
    break;

    case if_:
        auto if_node = cast (IfElse) node;
    break;

    case while_:
        auto while_node = cast (While) node;
    break;

    case return_:
        auto return_node = cast (Return) node;
    break;

    case block:
        auto block_node = cast (Block) node;
    break;

    default:
        writeln("AST type ", node.ast_type, " has not implemented yet.");
    assert(0);
    }
}
+/+/
