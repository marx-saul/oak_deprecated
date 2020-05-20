module oak.parser.parse;

import comp_d;
import std.typecons;
import oak.parser.definition, oak.lexer.lexer;

enum NodeType {
	dummy, expr
}
class Node {
	NodeType type;
	this (NodeType t = NodeType.init) { type = t; }
}

class ExprNode : Node {
	Node left;
	Node right;
	Node center;		// for the when expression
	Token token;
}

Node getAST(string source) {
	immutable(dchar)[] lookahead;
	ulong line_num;
	
	State[] state_stack = [0];
	Node[]  node_stack;
	while (true) {
		auto token = nextToken(source, lookahead, line_num);
		auto parser_step = oneStep(oak.parser.definition.grammar.grammar_info, table_info, token_type_dictionary[token.type], state_stack);
	}
}


