// definition of AST that parser generates.
//
module ast.astbase;

import std.algorithm, std.array;
/+
struct ASTBase {
    import ast.astnode, ast.symbol, attribute;
    import lexer;
    import visitor;

    alias Vis = Visitor!ASTBase;
    abstract class ASTNode {
        // shallow copy
        abstract @property ASTNode dup()  { return this; }
        // deep copy
        abstract @property ASTNode copy() { return this; }
        abstract void accept(Vis v);
    }

    /* *************************************** *
                     Expression
     * *************************************** */
    

    /* *************************************** *
                       Statement
     * *************************************** */
    

    /* *************************************** *
                       Type
     * *************************************** */
    

    /* *************************************** *
                    Declaration
     * *************************************** */


    /* ************************************ *
                      Tools
     * ************************************ */
    // identifier : Type
    class TypedIdentifier : Identifier {
        Attribute attr;
        Type type;
        this (string[] n, Attribute attr, Type t) {
            super(n);
            type = t;
        }
        this (string n, Attribute attr, Type t) {
            super(n);
            type = t;
        }
    }

}
+/

