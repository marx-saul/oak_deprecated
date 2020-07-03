module ast.symbol;

import parser.lexer, ast.all;

struct Symbol {
	// a.b.c is ["c", "b", "a"]
	class Identifier {
		string name;
 		this (string n) {
			name = n;
		}
	}
    
	final class Variable : Identifier {
		Attribute attr;
		Type type;
		this (string n, Attribute a, Type t = null) {
			super(n);
			attr = a;
			type = t;
		}
	}

	final class Label : Identifier {
		this (string n) {
			super(n);
		}
	}

	enum UDT {
		struct_,
		union_,
		class_,
		interface_,
		alias_,
	}
	final class UDType : Identifier {
		this (string n) {
			super(n);
		}
		ASTNode dec;	// declaration AST of that user-defined type
	}

	final class Function : Identifier {
		Attribute attr;
		Type ret_type;
		this (string n, Attribute a, Type t) {
			super(n);
			attr = a;
			ret_type = t;
		}
		FunctionDeclaration dec;	// declaration AST of that function
	}
	final class Template : Identifier {
		this (string n) {
			super(n);
		}
	}
	final class Module : Identifier {
		this (string n) {
			super(n);
		}
	}
	final class Package : Identifier {
		this (string n) {
			super(n);
		}
	}
}
