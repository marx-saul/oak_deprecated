module parser.parse;

import parser.lexer;
import std.stdio, std.algorithm;
import parser.defs, parser.expression;

unittest {
	//auto ast1 = getAST(new TokenRange!(string)("a + b")); //getAST("s |> n when n > 0 : -n # x . y z");
	//showExpression(ast1);
	//writeln("#### ast/ast.d");
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

