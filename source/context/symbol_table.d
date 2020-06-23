module context.symbol_table;
/+
import std.stdio, std.typecons, std.algorithm, std.array;
import parser.lexer, parser.defs, context.defs;

debug(symbol_table) unittest {
    writeln("\n#### parse/symbol_table.d unittest1");
    import parser.statement;


    auto token_pusher = new TokenRange!string(`{
        struct S {
            let a: var int = 32, b: [var int];
            func: var int f n1 = b !! a^n1;
        }
        { abcdefghijklmn; }

        func:int factorial n2:int(n>0) = n2 * f(n2-1);
        func factorial 0 = 1;

        if n3 > 102_000
            { writeln "yes"; }
        else if n3 < 102_000
            { writeln "no"; }
        else
            { writeln "equal"; }

        {
            let a1 = 10;
            {
                writeln;
            }
        }
        // calculate fibonacci number
        func fibonacci n:int
        {
            var counter = 0;
            var previous = 0, current = 1;
            while counter < n {
                let tmp = current;
                current += previous;
                previous = tmp;
                counter.inc;
            }
            writeln current;
        }
    }`);
    auto node = statement(token_pusher);
    BlockBelongTo bbt;
    blockStructure(node, null, bbt);
    //node.stringof.writeln();
}

// note that block are introduced by not only block statement but also func/proc, struct/class....
void blockStructure(AST node, AST belong_to, BlockBelongTo bbt) {
    if (node is null) return;

    void set(AST ast = node) {
        bbt[ast] = belong_to;
        debug(symbol_table) {
            if (ast.ast_type == ASTType.identifier) {
                auto id_node = cast(Identifier) ast;
                writeln(id_node.name.str, " ", id_node.name.type, " No.", id_node.name.line_num, "\t\tbelong to line No.", belong_to ? belong_to.token.line_num : -1);
            }
            else {
                writeln(ast.token.str, " ", ast.token.type, " No.", ast.token.line_num, "\t\tbelong to line No.", belong_to ? belong_to.token.line_num : -1);
            }
        }
    }

    debug(symbol_table) {
        if (node.ast_type == ASTType.identifier) {
            auto id_node = cast(Identifier) node;
            writeln(id_node.name.str, " ", id_node.name.type, " No.", id_node.name.line_num, "\t\tbelong to line No.", belong_to ? belong_to.token.line_num : -1);
        }
        else {
            writeln(node.token.str, " ", node.token.type, " No.", node.token.line_num, "\t\tbelong to line No.", belong_to ? belong_to.token.line_num : -1);
        }
    }

    with (ASTType) switch (node.ast_type) {
    case dummy:
    //assert(0, "Dummy AST found");
        writeln("dummy AST found");
    break;

    case expr:
        set();
        with (TokenType)
        if (node.token.type == when) {
            auto when_expr = cast (WhenExpression) node;
            blockStructure(when_expr.left,   belong_to, bbt);
            blockStructure(when_expr.center, belong_to, bbt);
            blockStructure(when_expr.right,  belong_to, bbt);
        }
        else if (node.token.type == comma) {
            set();
            auto tuple_expr = cast (TupleExpression) node;
            foreach (sub_expr; tuple_expr.exprs) {
                blockStructure(sub_expr, belong_to, bbt);
            }
        }
        else if (node.token.type == struct_) {
            auto struct_literal = cast (StructLiteral) node;
            foreach (val_expr; struct_literal.exprs) {
                blockStructure(val_expr, belong_to, bbt);
            }
        }
        else if (node.token.type == lBrack) {
            auto array_literal = cast (ArrayLiteral) node;
            foreach (val_expr; array_literal.exprs) {
                blockStructure(val_expr, belong_to, bbt);
            }
        }
        else if (node.token.type == colon) {
            auto assoc_array_literal = cast (AssocArrayLiteral) node;
            foreach (val_expr; assoc_array_literal.keys) {
                blockStructure(val_expr, belong_to, bbt);
            }
            foreach (val_expr; assoc_array_literal.values) {
                blockStructure(val_expr, belong_to, bbt);
            }
        }
        else if (node.token.type == unary_op) {
            auto expression = cast (BinaryExpression) node;
            blockStructure(expression.right, belong_to, bbt);
        }
        else if (node.token.type.among!(
            assign, cat_assign, add_assign, sub_assign, mul_assign, div_assign, mod_assign,
            sharp,  pipeline,  or,  and,
            ls, leq, gt, geq, eq, neq,
            indexing,
            add, sub, mul, div, mod,
            pow,  app,  composition,  dot,
            template_instance_type, template_instance_expr,
        )) {
            auto expression = cast (BinaryExpression) node;
            blockStructure(expression.left,  belong_to, bbt);
            blockStructure(expression.right, belong_to, bbt);
        }
        else if (node.token.type == identifier) {

        }
    break;

    case type:
        with (TokenType)
        if (node.token.type == comma) {
            set();
            auto tuple_type = cast (TupleType) node;
            foreach (sub_type; tuple_type.types) {
                blockStructure(sub_type, belong_to, bbt);
            }
        }
        else if (node.token.type.among!(
            template_instance_type, template_instance_expr,
            right_arrow,
            var,
            lBrack, mul,
        )) {
            set();
            auto type = cast (BinaryExpression) node;
            blockStructure(type.left,  belong_to, bbt);
            blockStructure(type.right, belong_to, bbt);
        }
        else if (node.token.type == identifier) {
            set();
        }
    break;

    case identifier:
        set();
        with (TokenType)
        if (node.token.type.among!(func, proc)) {
            auto func_node = cast (Function) node;
            foreach (arg; func_node.arguments) {
                blockStructure(arg, func_node.body, bbt);         // new block
            }
            foreach (condition; func_node.conditions) {
                blockStructure(condition, func_node.body, bbt);   // new block
            }
            blockStructure(func_node.body, belong_to, bbt);  // new block
        }
        else if (node.token.type.among!(struct_, class_, interface_)) {
            auto struct_node = cast (Struct) node;
            foreach (member; struct_node.members) {
                blockStructure(member, node, bbt);      // new block
            }
        }
        // let declaration in struct/class/interface
        else {
            auto id_node = cast (Identifier) node;
            blockStructure(id_node.body, belong_to, bbt);
        }
    break;

    case let:
        set();
        auto let_node = cast (LetDeclaration) node;
        foreach (id_node; let_node.identifiers) {
            blockStructure(id_node, belong_to, bbt);
        }
        blockStructure(let_node.body, belong_to, bbt);
    break;

    case if_:
        set();
        auto if_node = cast (IfElse) node;
        blockStructure(if_node.condition,  belong_to, bbt);
        blockStructure(if_node.if_block,   belong_to, bbt);
        blockStructure(if_node.else_block, belong_to, bbt);
    break;

    case while_:
        set();
        auto while_node = cast (While) node;
        blockStructure(while_node.condition, belong_to, bbt);
        blockStructure(while_node.block,     belong_to, bbt);
    break;

    case return_:
        set();
        auto return_node = cast (Return) node;
        blockStructure(return_node.expr, belong_to, bbt);
    break;

    case block:
        set();
        auto block_node = cast (Block) node;
        // new block
        foreach (statement; block_node.statements) {
            blockStructure(statement, node, bbt);   // new block
        }
    break;

    default:
        writeln("AST type ", node.ast_type, " has not implemented yet.");
        assert(0);
    }
}

/* ************************************************** */
/* ************************************************** */
/* ************************************************** */

// S0; let x1; S1; func f1; S2; let x2; S3; func f2; S4;
// x1.index = 3, f1.index = 0, x2.index = 4, f2.index = 0
// ssi[S0] = ssi[x1] = 2, ssi[S1] = ssi[f1] = ssi[S2] = ssi[x2] = 3, ssi[S3] = ssi[f2] = ssi[S4] = 4.
/+
void accessibleSymbols(AST node, ScopeSymbols ss, ScopeSymbolsIndices ssi, Symbol[string] symbol_table = null;) {
    if (node is null) return;

    void set_table(Identifier id, Symbol[string] st = symbol_table) {
        if (id is null) return;
        auto str = id.name.str;
        if (str in symbol_table)
            writeln("'", str, "' appears twice in the arguments of function " ~ func_node.name.str); // error
        else
            symbol_table[str] = new Symbol(SymbolType.variable, id, symbol_table.length+1);
    }

    with (ASTType) switch (node.ast_type) {
    case dummy:
    //assert(0, "Dummy AST found");
        writeln("dummy AST found");_
    break;

    case expr:
        ss[node] = symbol_table, ssi[node] = symbol_table.length;
        with (TokenType)
        if (node.token.type == when) {
            auto when_expr = cast (WhenExpression) node;
            accessibleSymbols(when_expr.left,   ss, ssi, symbol_table);
            accessibleSymbols(when_expr.center, ss, ssi, symbol_table);
            accessibleSymbols(when_expr.right,  ss, ssi, symbol_table);
        }
        else if (node.token.type == comma) {
            auto tuple_expr = cast (TupleExpression) node;
            foreach (sub_expr; tuple_expr.exprs) {
                accessibleSymbols(sub_expr, belong_to, bbt);
            }
        }
        else if (node.token.type == struct_) {
            auto struct_literal = cast (StructLiteral) node;
            foreach (val_expr; struct_literal.exprs) {
                accessibleSymbols(val_expr, ss, ssi, symbol_table);
            }
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
        // symbols of let declaration
        if (node.token.type == identifier) {
            auto id_node = cast (Identifier) node;
            set_table(id_node, symbol_table);
            ss[node] = symbol_table, ssi[node] = symbol_table.length-1;   // a variable cannot see itself;
        }
        // function declaration
        else if (node.token.type.among!(func, proc)) {
            auto func_node = cast (Function) node;
            set_table(func_node, symbol_table);
            symbol_table[func_node.name.str].index = 0; // function can be accessed from everywhere
            ss[node] = symbol_table, ssi[node] = symbol_table.length;   // a function can see itself (this is equivalent to symbol_table.length-1 since function can be seen from everywhere);

            // for new block
            Symbol[string] new_symbol_table;
            // collect arguments
            foreach (i, arg; func_node.arguments) {
                accessibleSymbols(arg, ss, ssi, new_symbol_table);
                new_symbol_table[arg.name.str].index = 0;   // arguments can be accessed from everywhere
            }
            // types and conditions
            foreach (type, func_node.types) {
                accessibleSymbols(type, ss, ssi, new_symbol_table);
            }
            foreach (expr, func_node.conditions) {
                accessibleSymbols(expr, ss, ssi, new_symbol_table);
            }

            // function body
            accessibleSymbols(func_node.body, ss, ssi, new_symbol_table);
        }
        // struct/class/interface declaration
        else if (node.token.type.among!(struct_, class_, interface_)) {
            auto struct_node = cast (Struct) node;
            set_table(struct_node, symbol_table);
            symbol_table[struct_node.name.str].index = 0; // struct/class/interface can be accessed from everywhere
            ss[node] = symbol_table, ssi[node] = symbol_table.length;   // a struct/class/interface can see itself

            Symbol[string] new_symbol_table;
            foreach (id_node; struct_node.members) {
                accessibleSymbols(id_node, new_symbol_table);
                new_symbol_table[arg.name.str].index = 0;   // members of struct can be accessed from everywhere
            }
        }
        else {
            writeln(node.token.type, " has not been implemented.");
            assert (0);
        }
    break;

    case let:
        ss[node] = symbol_table, ssi[node] = symbol_table.length;
        auto let_node = cast (LetDeclaration) node;

        // process each identifier
        foreach (id_node; let_node.identifiers) {
            if (id_node is null) continue;
            else {
                set(id_node, symbol_table);
                accessibleSymbols(id_node.type, ss, ssi, symbol_table);
                accessibleSymbols(id_node.body, ss, ssi, symbol_table);
            }
        }
        // block initialization
        accessibleSymbols(let_node.body, ss, ssi, assocArray(string, Symbol));

        // allow the block initialization to access declared symbols;
        ss[let_node.body] = symbol_table, ssi[node.body] = symbol_table.length;

    break;

    case if_:
        ss[node] = symbol_table, ssi[node] = symbol_table.length;
        auto if_node = cast (IfElse) node;
        accessibleSymbols(if_node.condition, ss, ssi, symbol_table);
        accessibleSymbols(if_node.if_block, ss, ssi, symbol_table);
        accessibleSymbols(if_node.else_block, ss, ssi, symbol_table);
    break;

    case while_:
        ss[node] = symbol_table, ssi[node] = symbol_table.length;
        auto while_node = cast (While) node;
        accessibleSymbols(while_node.condition, ss, ssi, symbol_table);
        accessibleSymbols(while_node.block, ss, ssi, assocArray(string, Symbol));
    break;

    case return_:
        ss[node] = symbol_table, ssi[node] = symbol_table.length;
        auto return_node = cast (Return) node;
        accessibleSymbols(return_node.expr, ss, ssi, symbol_table);
    break;

    case block:
        ss[node] = symbol_table, ssi[node] = symbol_table.length;
        Symbol[string] new_symbol_table;
        auto block_node = cast (Block) node;
        foreach (statement; block_node.statements) {
            if (statement is null) continue;
            with (ASTType)
            if (statement.ast_type.among!(func, struct_, class_, interface_)) {
                accessibleSymbols(statement, ss, ssi, new_symbol_table);
            }
        }
        foreach (statement; block_node.statements) {
            if (statement is null) continue;
            with (ASTType)
            if (statement.ast_type.among!(func, struct_, class_, interface_)) {
                ss[statement] = symbol_table, ssi[statement] = symbol_table.length;
            }
            else {
                accessibleSymbols(statement, ss, ssi, new_symbol_table);
            }
        }
    break;

    default:
        writeln("AST type ", node.ast_type, " has not implemented yet.");
    assert(0);
    }
}
+/

/+
// symbols declared within the same scope and that the statement can access
// designated_symbols is set before the statement is processed, for example d_s is the list of arguments of the block
void accessibleSymbols(AST node, ScopeSymbols sss, Symbol[] designated_symbols) {
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
        /*
        with (TokenType)
        // func/proc declaration
        if (node.token.type.among!(func, proc)) {
            Symbol[] arg_symbols;

            auto func_node = cast(Function) node;
            foreach (arg; func_node.arguments) {
                arg_symbols ~= new Symbol(SymbolType.variable, arg);
            }

            accessibleSymbols(func_node.body, sss, arg_symbols); // new block
        }
        // struct/class/interface
        else if (node.token.type.among!(struct_, class_, interface_)) {
            Symbol[] symbols;

            auto struct_node = cast(Struct) node;
            // first collect all
            foreach (member; struct_node.members) {
                if (member is null) continue;
                // func/proc / struct/class/interface
                else if (member.token.type in tokenType_to_symbolType) {
                    symbols ~= new Symbol(tokenType_to_symbolType[member.token.type], member);
                }
                // let declaration
                else if (member.token.type == TokenType.identifier) {
                    symbols ~= new Symbol(SymbolType.variable, member);
                }
                else assert (0);
            }

            // recurse
            foreach (member; struct_node.members) {
                if (member is null) continue;
                 // func/proc / struct/class/interface
                else if (member.token.type in tokenType_to_symbolType) {
                    sss[member] = symbols;  // set
                    accessibleSymbols(member, sss); // new block
                }
                // let declaration
                else if (member.token.type == TokenType.identifier) {
                    sss[member] = symbols;  // set
                    accessibleSymbols(member.body, sss);    // new block
                }
            }
        }
        // not implemented
        else {
            writeln (node.token.type, " has not implemented yet.");
            assert (0);
        }
    */
    break;

    case let:
        auto let_node = cast (LetDeclaration);
        /*
        accessibleSymbols(let_node.body, sss);
        */
    break;

    case if_:
        auto if_node = cast (IfElse) node;
        /*
        accessibleSymbols(if_node, )
        */
    break;

    case while_:
        auto while_node = cast (While) node;
    break;

    case return_:
        auto return_node = cast (Return) node;
    break;

    case block:
        auto block_node = cast (Block) node;
        /*
        */
    }
    break;

    default:
        writeln("AST type ", node.ast_type, " has not implemented yet.");
    assert(0);
    }

}
+/+/
