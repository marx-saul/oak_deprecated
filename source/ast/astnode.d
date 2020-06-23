module ast.astnode;

import lexer;
import visitor: Visitor;

abstract class ASTNode(AST) {
    // for DFS visiting of the AST
    abstract void accept(Visitor!AST v);
}

