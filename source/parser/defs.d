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
	dummy, expr, type, func, proc, let, if_, while_, struct_, block,
}

class Node {
	NodeType type;
	Token token;
	Node[] child;
	
	this (NodeType tp = NodeType.init) { type = tp; }
	this (TokenType t) { token.type = t; }
	this (Token tkn) { token = tkn; }
	this (NodeType tp, Token tkn) { token = tkn, type = tp; }
}

Node expr_node(Token tkn, Node left = null, Node right = null, Node third = null) {
	auto node = new Node(NodeType.expr, tkn);
	// when has three childs
	with (TokenType)
	if      (tkn.type == when)    { node.child = [left, right, third]; }
	else if (tkn.type == struct_) { node.child = [left]; }
	else                          { node.child = [left, right]; }
	return node;
}

Node type_node(Token tkn, Node left = null, Node right = null) {
	auto node = new Node(NodeType.type, tkn);
	node.child = [left, right];
	return node;
}

string stringofNode(Node node) {
	import parser.expression: stringofExpression;
	import parser.type: stringofType;
	import parser.declaration: stringofFunction, stringofVariables, stringofStruct;
	import parser.statement: stringofIfElseStatement, stringofWhileStatement, stringofBlockStatement;

	if (node is null) return "";
	with(NodeType) switch (node.type) {
		case dummy:
			if (node.token.type == TokenType.string_literal) return "\"" ~ node.token.str ~ "\"";
			else return node.token.str;
		case expr:            return stringofExpression(node);
		case type: 		      return stringofType(node);
		case func:            return stringofFunction(node);
		case let:             return stringofVariables(node);
		case if_:             return stringofIfElseStatement(node);
		case while_:          return stringofWhileStatement(node);
		case block:           return stringofBlockStatement(node);
		case struct_:         return stringofStruct(node);
		default: 		      return " SOMETHING ";
	}
}
