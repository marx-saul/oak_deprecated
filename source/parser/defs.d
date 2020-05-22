module parser.defs;

import std.range, std.traits;
import parser.lexer;

/* token pusher */
enum isTokenRange(T) =
	is(ReturnType!((T t) => t.front) == Token) &&
	is(ReturnType!((T t) => t.empty) == bool) &&
	is( typeof( { T t; t.popFront(); } ) );

class TokenRange(R)
	if (isInputRange!R && is(ReturnType!((R r) => r.front) : immutable dchar))
{
	private R source;
	private immutable(dchar)[] lookahead;
	ulong line_num = 1;
	this (R s) {
		source = s;
		token = nextToken(source, lookahead, line_num);
	}
	
	private bool empty_flag = false;
	private Token token;
	bool empty() @property {
		return empty_flag;
	}
	Token front() @property {
		return token;
	}
	void popFront() {
		if (token.type == TokenType.end_of_file) this.empty_flag = true;
		else token = nextToken(source, lookahead, line_num);
	}
}

/* for AST */
enum NodeType {
	dummy, expr, /*tuple_expr*/
}
class Node {
	NodeType type;
	Token token;
	this (NodeType t = NodeType.init) { type = t; }
	this (TokenType t) { token.type = t; }
	this (Token t) { token = t; }
	this (Token tkn, NodeType tp) { token = tkn, type = tp; } 
}

class ExprNode : Node {
	// unary expression is expressed by unary_op
	ExprNode left;
	ExprNode right;
	ExprNode center;
	bool tuple_solved;		// check if the tuple is enclosed by parenthesis
	
	this (Token t, ExprNode l = null, ExprNode c = null, ExprNode r = null) {
		type = NodeType.expr, token = t, left = l, right = r, center = c;
		tuple_solved = token.type != TokenType.comma;
	}
	this (TokenType t) { type = NodeType.expr, token.type = t; }
}
