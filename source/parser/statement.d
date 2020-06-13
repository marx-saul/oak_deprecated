module parser.statement;

import std.stdio, std.typecons, std.algorithm;
import parser.lexer, parser.defs, parser.expression, parser.declaration;

/*
Statement:
    LetVarDeclaration               // declaration
    FunctionProcedureDeclaration    // declaration
    StructDeclaration               // declaration
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

unittest {
    writeln("\n#### parse/statement.d unittest1");
    import parser.defs : TokenRange;

    auto token_pusher = new TokenRange!string(`{
        func:int f n:int(n>0) = -n * f(n-1);
        func f 0 = 1;
        if n > 0 { writeln "positive"; } else if n < 0 { writeln "negative"; } else { writeln "0"; }
        var counter = 0;
        while counter < 10 { writeln counter "-th visit"; counter.inc; }
        struct S {
            let a: var int, b: [var int];
            func: var int f = b !! a^^2;
        }
    }`);
    auto node = statement(token_pusher);
    node.stringof.writeln();
}

bool isFirstOfStatement(TokenType t) {
    with (TokenType) return
        t.among!(let, var, func, proc, if_, while_, lBrace) || t.isFirstOfExpression();
}

AST statement(Range)(ref Range input)
    if (isTokenRange!Range)
{
    with (TokenType)
    switch (input.front.type) {
        case let:   case var:  return letVarDeclaration(input);
        case func:  case proc: return functionProcedureDeclaration(input);
        //case struct_:
        //case class_:
        //case interface_:
        // case Expression
        case if_:              return ifElseStatement(input);
        case while_:           return whileStatement(input);
        //case foreach_: case foreach_reverse_:
        case lBrace:           return blockStatement(input);

        default: break;
    }
    with (TokenType)
    // StructDeclaration
    if      (input.front.type == TokenType.struct_ && input.lookahead.type != lPar) {
        return structDeclaration(input);
    }
    // Expression
    else if (input.front.type.isFirstOfExpression()) {
        auto expr_node = expression(input);
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
AST[] statements(Range)(ref Range input)
    if (isTokenRange!Range)
{
    AST[] result;
    while (isFirstOfStatement(input.front.type)) {
        result ~= statement(input);
    }
    //writeln("statements end with " ~ input.front.str);
    return result;
}
Block blockStatement(Range)(ref Range input)
    if (isTokenRange!Range)
{
    auto lBrace_token = input.front;
    input.popFront();   // get rid of {
    auto result = new Block(lBrace_token);
    result.statements = statements(input);
    // error
    if (input.front.type != TokenType.rBrace) {
        writeln("Enclosure '}' of the block was expected, not " ~ input.front.str);
    }
    input.popFront();   // get rid of }
    return result;
}

IfElse ifElseStatement(Range)(ref Range input)
    if (isTokenRange!Range)
{
    auto if_token = input.front;
    input.popFront();   // get rid of 'if'
    AST expr_node;
    if (!input.front.type.isFirstOfExpression()) {
        writeln("An expression expected after 'if', not " ~ input.front.str);
    }
    else expr_node = expression(input);

    AST statement1, statement2;
    with (TokenType)
    // if Expression BlockStatement
    if      (input.front.type == lBrace) {
        statement1 = blockStatement(input);
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
            statement2 = blockStatement(input);
        }
        // if Expression else IfElseStatement
        else if (input.front.type == if_) {
            statement2 = ifElseStatement(input);
        }
        // error
        else {
            writeln("A block { ... } or 'if' was expected after 'else', not " ~ input.front.str);
        }
    }

    auto if_else_node = new IfElse(if_token);
    if_else_node.condition = expr_node,
    if_else_node.if_block = statement1,
    if_else_node.else_block = statement2;
    return if_else_node;
}

While whileStatement(Range)(ref Range input)
    if (isTokenRange!Range)
{
    auto while_token = input.front;
    input.popFront();   // get rid of 'while'
    AST expr_node;
    if (!input.front.type.isFirstOfExpression()) {
        writeln("An expression expected after 'while', not " ~ input.front.str);
    }
    else expr_node = expression(input);

    Block block_statement;
    with (TokenType)
    // while Expression BlockStatement
    if (input.front.type == lBrace) {
        block_statement = blockStatement(input);
    }
    // error
    else {
        writeln("A block { ... } was expected as the body of 'while', not " ~ input.front.str);
    }

    auto while_node = new While(while_token);
    while_node.condition = expr_node,
    while_node.block = block_statement;
    return while_node;
}
