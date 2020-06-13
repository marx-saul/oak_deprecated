module parser.defs;

import std.range, std.traits;
import parser.lexer;

/* token pusher */
enum isTokenRange(T) =
	is(ReturnType!((T t) => t.front) == Token) &&
	//is(ReturnType!((T t) => t.empty) == bool) &&
	is( typeof( { T t; t.popFront(); } ) ) &&
	is(ReturnType!((T t) => t.lookahead) == Token);

class TokenRange(R)
	if (isInputRange!R && is(ReturnType!((R r) => r.front) : immutable dchar))
{
	private R source;
	private immutable(dchar)[] char_lookahead;
	ulong line_num = 1, index_num = 1;
	this (R s) {
		source = s;
		token = nextToken(source, char_lookahead, line_num, index_num);
	}
	
	private Token token;
	private Token token_ahead;
	private bool looked_ahead = false;
	Token front() @property {
		return token;
	}
	void popFront() {
		if (looked_ahead) {
			token = token_ahead;
		}
		else token = nextToken(source, char_lookahead, line_num, index_num);
		looked_ahead = false;
	}
	Token lookahead() @property {
		if (looked_ahead) return token_ahead;
		else return token;
	}
}

/* for AST */
enum ASTType {
	dummy, expr, type, symbol, let, if_, while_, block,
}

class AST {
	ASTType ast_type;
	Token token;
	
	this (ASTType tp = ASTType.init) { ast_type = tp; }
	this (TokenType t) { token.type = t; }
	this (Token tkn) { token = tkn; }
	this (ASTType tp, Token tkn) { token = tkn, ast_type = tp; }

	string stringof() @property {
		return token.str;
	}
}

// ast_type = expr
// these are determined by token.type
class BinaryExpression:AST {
	AST left;
	AST right;
	this (Token tkn, AST l = null, AST r = null) {
		ast_type = ASTType.expr, token = tkn, left = l, right = r;
	}

	override string stringof() @property {
		if (left is null && right is null) return token.str;
		else if (token.type == TokenType.unary_op)
			return (left ? left.token.str : "ERROR") ~ (right ? right.token.str : "ERROR");
		else
			return "(" ~ (left ? left.stringof : "ERROR") ~ " " ~ token.str ~ " " ~ (right ? right.stringof : "ERROR") ~ ")";
	}
}
class WhenExpression:AST {
	AST left;
	AST center;
	AST right;
	this (Token tkn, AST l, AST c, AST r) {
		ast_type = ASTType.expr, token = tkn, left = l, center = c, right = r;
	}

	override string stringof() @property {
		if (left is null && right is null)
			return token.str;
		else
			return "(" ~ (left ? left.stringof : "ERROR") ~ " when " ~ (center ? center.stringof : "ERROR") ~ ") else (" ~ (right ? right.stringof : "ERROR") ~ "))";
	}
}
class TupleExpression:AST {
	AST[] exprs;
	this (AST[] es) {
		ast_type = ASTType.expr, exprs = es;
	}

	override string stringof() @property {
		string result = "(";
		foreach (expr; exprs) {
			result ~= (expr ? expr.stringof : "ERROR") ~ ", ";
		}
		return result[0 .. $-2] ~ ")";
	}
}

// ast_type = type
// these are determined by token.type
// var T is token.type = TokenType.var, left = T, right = null. [T], *T are the same.
// [T:S] is token.type = TokenType.lBrack, left = T, right = S.
// (T1, ..., Tn) is token.type = TokenType.comma
class BinaryType:AST {
	AST left;
	AST right;
	this (Token tkn, AST l = null, AST r = null) {
		ast_type = ASTType.type, token = tkn, left = l, right = r;
	}

	override string stringof() @property {
		if (left is null && right is null)
			return token.str;
		else if (token.type == TokenType.var)
			return "(var " ~ (left ? left.stringof : "ERROR") ~ ")";
		else if (token.type == TokenType.lBrack)
			if (right is null)
				return "[" ~ (left ? left.stringof : "ERROR") ~ "]";
			else
				return "[" ~ (left ? left.stringof : "ERROR") ~ ":" ~ right.stringof ~ "]";
		else if (token.type == TokenType.mul)
			return "(*" ~ (left ? left.stringof : "ERROR") ~ ")";
		else
			return "(" ~ (left ? left.stringof : "ERROR") ~ " " ~ token.str ~ " " ~ (right ? right.stringof : "ERROR") ~ ")";
	}
}
class TupleType:AST {
	AST[] types;
	this () {
		ast_type = ASTType.type;
	}
	override string stringof() @property {
		string result = "(";
		foreach (type; types) {
			result ~= (type ? type.stringof : "ERROR") ~ ", ";
		}
		return result[0 .. $-2] ~ ")";
	}
}

// ast_type = symbol
// for declaration of a symbol like func/proc, struct/class/... declarations.
// token.type records func/proc/let/var/struct/class....
// its name is indicated by name
class Symbol:AST {
	bool is_argument; // is an argument of a function, templates.
	AST type;
	Token name;
	AST attributes; // the list of public/private, ref, etc.
	AST body; // records E where let x:T = E, or Block where f x = Block
	this () {
		ast_type = ASTType.symbol;
	}

}
// func/proc declaration / labmda expression
class Function:Symbol {
	Symbol[] arguments;
	AST[] conditions;	// contitions of the argumentthis ()
	this() {
		ast_type = ASTType.symbol;
	}

	override string stringof() @property {
		string result = token.str ~ " ";
		if (type) result ~= ":" ~ type.stringof ~ " ";
		foreach (i, arg; arguments) {
			if (arg is null) continue;
			result ~= arg.name.str;
			if (arg.type) result ~= ":" ~ arg.type.stringof ~ " ";
			if (conditions[i]) result ~= conditions[i].stringof ~ " ";
		}
		if (body is null) return result ~ "{ NONE }";
		if (body.ast_type == ASTType.expr) {
			return result ~ " = " ~ body.stringof ~ (token.type == TokenType.lambda ? "" : ";");
		}
		else { return result ~ body.stringof; }
	}
}
// struct/class/interface delaration
class Struct:Symbol {
	Symbol[] members;
	this() {
		ast_type = ASTType.symbol;
	}
	override string stringof() @property {
		return " **a struct** ";
	}
}
// let declaration
class LetDeclaration:AST {
	Symbol[] symbols;
	this (Token tkn) {
		ast_type = ASTType.let, token = tkn;
	}
	override string stringof() @property {
		string result = "let ";
		foreach (symbol; symbols) {
			if (symbol is null) continue;
			result ~= symbol.name.str;
			if (symbol.type) result ~= ": " ~ symbol.type.stringof;
			if (symbol.body !is null) result ~= " = " ~ symbol.body.stringof;
			result ~= ", ";
		}
		return result[0 .. $-2] ~ ";";
	}
}
// ast_type = if_
class IfElse:AST {
	AST condition;
	AST if_block;
	AST else_block;
	this (Token tkn) {
		ast_type = ASTType.if_, token = tkn;
	}
	override string stringof() @property {
		string result = "if ";
		if (condition !is null) result ~= condition.stringof;
		else result ~= "ERROR";

		if (if_block) result ~= if_block.stringof;
		else result ~= "ERROR";

		if (else_block) result ~= "else " ~ else_block.stringof;

		return result;
	}
}
// ast_type = while_
class While:AST {
	AST condition;
	AST block;
	this (Token tkn) {
		ast_type = ASTType.while_, token = tkn;
	}
	override string stringof() @property {
		string result = "while ";
		if (condition !is null) result ~= condition.stringof;
		else result ~= "ERROR";

		if (block) result ~= block.stringof;
		else result ~= "ERROR";
		return result;
	}
}
// ast_type = block
class Block:AST {
	AST[] statements;
	this (Token tkn) {
		ast_type = ASTType.block, token = tkn;
	}
	override string stringof() @property {
		string result = "{\n";
		foreach (stmt; statements) {
			if (stmt is null) result ~= "ERROR\n";
			else if (stmt.ast_type == ASTType.expr) result ~= stmt.stringof ~ ";\n";
			else result ~= stmt.stringof ~ "\n";
		}
		return result ~ "}";
	}
}
/+
string stringofNode(AST ast) {
	import parser.expression: stringofExpression;
	import parser.type: stringofType;
	import parser.declaration: stringofFunction, stringofVariables, stringofStruct;
	import parser.statement: stringofIfElseStatement, stringofWhileStatement, stringofBlockStatement;

	if (ast is null) return "";
	with(NodeType) switch (ast.node_type) {
		/+case dummy:
			if (ast.token.type == TokenType.string_literal) return "\"" ~ ast.token.str ~ "\"";
			else return ast.token.str;
		case expr:            return stringofExpression(ast);
		case type: 		      return stringofType(ast);
		case func:            return stringofFunction(ast);
		case let:             return stringofVariables(ast);
		case if_:             return stringofIfElseStatement(ast);
		case while_:          return stringofWhileStatement(ast);
		case block:           return stringofBlockStatement(ast);
		case struct_:         return stringofStruct(ast);
		+/
		default: 		      return " SOMETHING ";
	}
}
+/
