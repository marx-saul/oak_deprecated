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
enum NodeType {
	dummy, expr, type, func
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
	Node left;
	Node right;
	//ExprNode center;			// a when b : c is when.left = colon, when.right = b, colon.left = a, colon.right = c
	//bool tuple_solved;		// check if the tuple is enclosed by parenthesis
	
	this (Token t, Node l = null, Node r = null) {
		type = NodeType.expr, token = t, left = l, right = r; //center = c;
		//tuple_solved = token.type != TokenType.comma;
	}
	this (TokenType t) { type = NodeType.expr, token.type = t; }
}

class TypeNode : Node {
	Node left;
	Node right;
	
	this (Token t, Node l = null, Node r = null) {
		type = NodeType.type, token = t, left = l, right = r;
	}
	this (TokenType t) {
		type = NodeType.type, token.type = t;
	}
}

class FunctionNode : Node {
	string func_name;
	ExprNode func_body;
	TypeNode func_type;
	TypeNode return_type;
	string[] args;
	TypeNode[] args_types;
	ExprNode[] args_conditions;
	this (string fn) { type = NodeType.func, func_name = fn; }
}

string stringofNode(Node node) {
	import parser.expression: stringofExpression;
	import parser.type: stringofType;
	
	if (node is null) return "";
	with(NodeType) switch (node.type) {
		case expr: 		return stringofExpression(node);
		case type: 		return stringofType(node);
		default: 		return " SOMETHING ";
	}
}
