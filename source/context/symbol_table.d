module context.symbol_table;

import std.stdio, std.typecons;
import parser.defs, context.defs;

unittest {
    //auto symbol_info = new SymbolInfo;
    //resolveSymbols(symbol_info, )
}
/+
//
SymbolInfo resolveSymbols(SymbolInfo symbol_info, AST source) {
    AST[] block_stack = [source];          // stack of blocks, top is the deepest
    size_t[] number_stack = [0];            // currently processing block_stack[$-1].child[number_stack[$-1]]
    Symbol[string] tmp_st;
    SymbolTable[] table_stack = [tmp_st];   // the symbols declared in the block
    SymbolOfNode symbol_of;

    while (true) {
        auto top_block = block_stack[$-1];
        // processed the last statement of the block
        if (number_stack[$-1] >= top_block.child.length) {
            // end the whole routine
            if (block_stack.length == 1) break;
            // end parsing current block
            else {
                block_stack.length -= 1; number_stack.length -= 1; table_stack.length -= 1;
                continue;
            }

        }
        auto statement = top_block.child[number_stack[$-1]];
        // block statement
        if (statement.node_type == ASTType.block) {
            block_stack ~= statement;
            ++number_stack[$-1];
            number_stack ~= 0;
            table_stack ~= [];
            continue;
        }

        with (ASTType) switch (statement.node_type) {
            case dummy: case type:
            assert(0, "Semantics Analysis error.");

            case expr:
            break;

            case func:
            break;

            case proc:
            break;

            case let:
            break;

            case if_:
            break;

            case while_:
            break;

            case struct_:
                writeln("'struct' has not implemented.");
            break;

            default:
            assert(0);
        }
    }

    auto result = new SymbolInfo;
    result.table = table_stack[0];
    result.symbol_of = symbol_of;
    return result;
}

void setSymbolTableOfExpression(AST node, const SymbolTable[] table_stack, SymbolOfNode symbol_of) {

}
+/

/+
SymbolTable getSymbolTable(Node statements) {
    Symbol
    foreach (stmt; statements) {

    }
}
+/
/+
SymbolTable getSymbolTableFromLetVarDeclaration(Node statement) {
    SymbolTable result;
    foreach (id_node; statement.child) {
        // error
        if (id_node.token.str in result) {
            writeln("Same identifier '", id_node.token.str ,"' appeared twice in the let/var declaration.");
            continue;
        }
        result[id_node.token.str] = new Variable(id_node.token.str, id_node.child[0], false);
    }
    return result;
}
+/
/+
SymbolTable getSymbolTableFromFunctionProcedureDeclaration(Node statement)
+/
