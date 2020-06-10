module parser.statement;

import std.stdio, std.typecons, std.algorithm;
import parser.lexer, parser.defs, parser.expression, parser.declaration;

/*
Statement:
    LetVarDeclaration               // declaration
    FunctionProcedureDeclaration    // declaration
    StructDeclaration             // declaration
    Expression ;                    // expression
    // Note: StructDeclaration and Expression both start with the token struct, but a struct literal
    // is of the form struct(S) {a:b, ...}, we can decide these by look-ahead.
    IfElseStatement
    WhileStatement
    //ForeachForeach_reverseStatement
    BlockStatement
Statements:
    Statement Statements
    empty
BlockStatement:
    { Statements }

IfElseStatement:
    if Expression BlockStatement
    if Expression BlockStatement  else BlockStatement
    if Expression BlockStatement  else IfElseStatement

WhileStatement:
    while Expression BlockStatement

ForeachForeach_reverseStatement:
    foreach         ForeachForeach_reverseIdentifierDeclarations ; Expression BlockStatement
    foreach_reverse ForeachForeach_reverseIdentifierDeclarations ; Expression BlockStatement
    foreach         ForeachForeach_reverseIdentifierDeclarations ; Expression .. Expression BlockStatement
    foreach_reverse ForeachForeach_reverseIdentifierDeclarations ; Expression .. Expression BlockStatement
ForeachForeach_reverseIdentifierDeclarations:
    Identifier
    Identifier: Type
    Identifier , ForeachForeach_reverseIdentifierDeclarations
    Identifier: Type , ForeachForeach_reverseIdentifierDeclarations
*/

/*
if E S1 else S2 has AST
if ---> E, S1, S2
if E S1 has AST
if ---> E, S1, null
*/

unittest {
    writeln("\n#### parse/statement.d unittest1");
    import parser.defs : TokenRange;

    auto token_pusher = new TokenRange!string(`{
        func:int f n:int(n>0) = n * f(n-1);
        func f 0 = 1;
        if n > 0 { writeln "positive"; } else if n < 0 { writeln "negative"; } else { writeln "0"; }
        var counter = 0;
        while counter < 10 { writeln counter "-th visit"; counter.inc; }
        struct S {
            let a: var int, b: [var int];
            func: var int f = b !! a^^2;
        }
    }`);
    auto node = Statement(token_pusher);
    node.stringofNode().writeln();
}

bool isFirstOfStatement(TokenType t) {
    with (TokenType) return
        t.among!(let, var, func, proc, if_, while_, lBrace) || t.isFirstOfExpression();
}

Node Statement(Range)(ref Range input)
    if (isTokenRange!Range)
{
    with (TokenType)
    switch (input.front.type) {
        case let:   case var:  return LetVarDeclaration(input);
        case func:  case proc: return FunctionProcedureDeclaration(input);
        //case struct_:
        //case class_:
        //case interface_:
        // case Expression
        case if_:              return IfElseStatement(input);
        case while_:           return WhileStatement(input);
        //case foreach_: case foreach_reverse_:
        case lBrace:           return BlockStatement(input);

        default: break;
    }
    with (TokenType)
    // StructDeclaration
    if      (input.front.type == TokenType.struct_ && input.lookahead.type != lPar) {
        return StructDeclaration(input);
    }
    // Expression
    else if (input.front.type.isFirstOfExpression()) {
        auto expr_node = Expression(input);
        if (input.front.type != semicolon) {
            writeln("';' was expected after an expression, not " ~ input.front.str);
        }
        else input.popFront();  // get rid of ;
        return expr_node;
    }
    // error
    else {
        writeln("Invalid statement, starting from " ~ input.front.str);
        return null;
    }
}
Node[] Statements(Range)(ref Range input)
    if (isTokenRange!Range)
{
    Node[] result;
    while (isFirstOfStatement(input.front.type)) {
        result ~= Statement(input);
    }
    //writeln("statements end with " ~ input.front.str);
    return result;
}
Node BlockStatement(Range)(ref Range input)
    if (isTokenRange!Range)
{
    auto lBrace_token = input.front;
    input.popFront();   // get rid of {
    auto result = new Node(NodeType.block, lBrace_token);
    result.child = Statements(input);
    // error
    if (input.front.type != TokenType.rBrace) {
        writeln("Enclosure '}' of the block was expected, not " ~ input.front.str);
    }
    input.popFront();   // get rid of }
    return result;
}

string stringofBlockStatement(Node node) {
    import parser.defs: stringofNode;
    import std.conv:to;
    string result = /*node.child.length.to!string ~ */"{\n";
    foreach (stmt; node.child) {
        result ~= stringofNode(stmt);
        if (stmt is null) { result ~= " NULL\n"; break; }
        if (stmt.type == NodeType.expr) result ~= ";";
        result ~= "\n";
    }
    return result ~= "}";
}

Node IfElseStatement(Range)(ref Range input)
    if (isTokenRange!Range)
{
    auto if_token = input.front;
    input.popFront();   // get rid of 'if'
    auto expr_node = Expression(input);

    Node statement1, statement2;
    with (TokenType)
    // if Expression BlockStatement
    if      (input.front.type == lBrace) {
        statement1 = BlockStatement(input);
    }
    // error
    else {
        writeln("A block { ... } was expected as the body of 'if', not " ~ input.front.str);
    }

    with (TokenType)
    if (input.front.type == else_) {
        input.popFront();   // get rid of else
        // if Expression else BlockStatement
        if      (input.front.type == lBrace) {
            statement2 = BlockStatement(input);
        }
        // if Expression else IfElseStatement
        else if (input.front.type == if_) {
            statement2 = IfElseStatement(input);
        }
        // error
        else {
            writeln("A block { ... } or 'if' was expected after 'else', not " ~ input.front.str);
        }
    }

    auto if_else_node = new Node(NodeType.if_, if_token);
    if_else_node.child = [expr_node, statement1, statement2];
    return if_else_node;
}

string stringofIfElseStatement(Node node) {
    import parser.defs: stringofNode;
    string result = "if" ~ stringofNode(node.child[0]) ~ stringofNode(node.child[1]);
    if (node.child[2] !is null) result ~= " else " ~ stringofNode(node.child[2]);
    return result;
}

Node WhileStatement(Range)(ref Range input)
    if (isTokenRange!Range)
{
    auto while_token = input.front;
    input.popFront();   // get rid of 'while'
    auto expr_node = Expression(input);

    Node block_statement;
    with (TokenType)
    // while Expression BlockStatement
    if (input.front.type == lBrace) {
        block_statement = BlockStatement(input);
    }
    // error
    else {
        writeln("A block { ... } was expected as the body of 'while', not " ~ input.front.str);
    }

    auto while_node = new Node(NodeType.while_, while_token);
    while_node.child = [expr_node, block_statement];
    return while_node;
}

string stringofWhileStatement(Node node) {
    import parser.defs: stringofNode;
    string result = "while" ~ stringofNode(node.child[0]) ~ stringofNode(node.child[1]);
    return result;
}
