module ast.astnode;

import visitor.visitor: Visitor;

abstract class ASTNode {
    // shallow copy
    abstract ASTNode dup();
    // deep copy
    abstract ASTNode copy();
    // accept for the visitor
    abstract void accept(Visitor v);
}


