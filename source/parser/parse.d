module parser.parse;

import parser.lexer;
import std.stdio, std.algorithm, std.range, std.traits;
import parser.expression;

unittest {
	//auto ast1 = getAST(new TokenRange!(string)("a + b")); //getAST("s |> n when n > 0 : -n # x . y z");
	//showExpression(ast1);
	//writeln("#### ast/ast.d");
}

//alias grammar = parser.grammar;

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

Node getAST(Range)(Range input)
	if (isTokenRange!Range)
{
	return oak.parser.expression.getAST(input);
}
/+
Node getAST(string source) {
	immutable(dchar)[] lookahead;
	ulong line_num;
	
	State[] state_stack = [0];
	Node[]  stack;
	while (true) {
		auto token = nextToken(source, lookahead, line_num);
		auto result = oneStep(
			grammar.grammar_info, table_info,
			token_type_dictionary[token.type], state_stack
		);
		if (result.action == Action.shift) {
			stack ~= new Node(token);
		}
		else if (result.action == Action.reduce) {
			reduction(result.num, stack);
		}
		else if (result.action == Action.accept) {
			return stack[0];
		}
		else if (result.action == Action.error) {
			/* error */
			writeln("Syntax error");
			writeln("state : ", state_stack);
			writeln("stack : ", stack);
			return null;
		}
		else assert(0);
	}
}

void reduction(ulong num, ref Node[] stack) {
	auto label = grammar.labelOf(num);
	if (label.startsWith("Expr_")) {
		expression_reduction(label[5 .. $], stack);
	}
	else { assert(0, "not implemented yet : " ~ label); }
}
+/

