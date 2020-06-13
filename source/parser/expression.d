module parser.expression;

import std.stdio, std.typecons, std.algorithm;
import std.conv: to;
import aatree, parser.lexer, parser.defs;

unittest {
	writeln("\n#### parse/expression.d unittest1");
	import parser.defs : TokenRange;
	
	//auto token_pusher = new TokenRange!string("f n = n * f (n-1) when n > 0 else 1");
	//auto token_pusher = new TokenRange!string("a = (b1, b2) when c else d,e");
	//auto token_pusher = new TokenRange!string("f # a when b else f - g.h k * i, j ");
	//auto token_pusher = new TokenRange!string("f # a when b else f - g.h k * (i?3::int, j) ");
	auto token_pusher = new TokenRange!string("fix (\\:int f:int->int n:int = 1 when n == 0 else n * f (n-1))");
	auto node = expression(token_pusher);
	node.stringof.writeln();
	
	token_pusher = new TokenRange!string("a <= b > c");
	node = expression(token_pusher);

	//token_pusher = new TokenRange!string("reverse static_list::int?struct(S){a:3, b:4.4} .stringof");
	//node = expression(token_pusher);
	//node.stringof.writeln();
}

bool isFirstOfExpression(TokenType a) {
	with(TokenType)
		return a.among!(
			add, sub, not, lPar, lambda, dollar, identifier,
			integer, real_number, /*character,*/ string_literal,
			true_, false_, this_, struct_
		) != 0;
}

AST expression(Range)(ref Range input)
	if (isTokenRange!Range)
{
	uint parenthesis_depth;
	return assignExpression(input, parenthesis_depth);
}
//alias NonAssignExpression = SharpExpression;

AST assignExpression(Range)(ref Range input, ref uint parenthesis_depth)
	if (isTokenRange!Range)
{
	debug(parser) { writeln("assign_expression"); scope(exit) writeln("end assign_expression"); }
	auto sharp_expr = sharpExpression(input, parenthesis_depth);
	with(TokenType) if (input.front.type.among!(assign, add_assign, sub_assign, mul_assign, div_assign, mod_assign)) {
		auto assign_token = input.front;
		input.popFront();	// get rid of =
		// check if the following is the start of expression
		if (!input.front.type.isFirstOfExpression()) { writeln("An expression is expected after '=', not " ~ input.front.str); return null; }	// error
		auto assign_expr = assignExpression(input, parenthesis_depth);
		return new BinaryExpression(assign_token, sharp_expr, assign_expr);
	}
	else return sharp_expr;
}

private AST sharpExpression(Range)(ref Range input, ref uint parenthesis_depth)
	if (isTokenRange!Range)
{
	debug(parser) { writeln("sharp_expression"); scope(exit) writeln("end sharp_expression"); }
	auto when_expr = whenExpression(input, parenthesis_depth);
	if (input.front.type == TokenType.sharp) {
		auto sharp_token = input.front;
		input.popFront();	// get rid of #
		// check if the following is the start of expression
		if (!input.front.type.isFirstOfExpression()) { writeln("An expression is expected after '#', not " ~ input.front.str); return null; }	// error
		auto sharp_expr = sharpExpression(input, parenthesis_depth);
		return new BinaryExpression(sharp_token, when_expr, sharp_expr);
	}
	else return when_expr;
}

// WhenExpression:
//     CommaExpression when CommaExpression : WhenExpression
//     CommaExpression
private AST whenExpression(Range)(ref Range input, ref uint parenthesis_depth)
	if (isTokenRange!Range)
{
	debug(parser) { writeln("when_expression"); scope(exit) writeln("end when_expression"); }
	auto comma_expr1 = commaExpression(input, parenthesis_depth);
	if (input.front.type == TokenType.when) {
		auto when_token = input.front;
		input.popFront();	// get rid of 'when'
		// check if the following is the start of expression
		if (!input.front.type.isFirstOfExpression()) { writeln("An expression is expected after 'when', not " ~ input.front.str); return null; }	// error
		auto comma_expr2 = commaExpression(input, parenthesis_depth);
		// else not found error
		if (input.front.type != TokenType.else_) {
			writeln("'else' not found");
			return null;
		}
		//auto colon_token = input.front;
		input.popFront();	// get rid of else
		// check if the following is the start of expression
		if (!input.front.type.isFirstOfExpression()) { writeln("An expression is expected after ':', not " ~ input.front.str); return null; }	// error
		auto when_expr = whenExpression(input, parenthesis_depth);
		return new WhenExpression(when_token, comma_expr1, comma_expr2, when_expr);
	}
	else return comma_expr1;
}

private AST commaExpression(Range)(ref Range input, ref uint parenthesis_depth)
	if (isTokenRange!Range)
{
	debug(parser) { writeln("comma_expression"); scope(exit) writeln("end comma_expression"); }
	return pipelineExpression(input, parenthesis_depth);
	/+if (parenthesis_depth == 0) {
		// error
		if (!input.front.type.isFirstOfExpression()) { writeln("An expression is expected, not " ~ input.front.str); return null; }
		return PipelineExpression(input, parenthesis_depth);
	}
	else assert(0, "Tuple has not implemented yet");+/
}

/*************************************/
//  left associative if 0 mod 10
// right associative if 1 mod 10
//   non associative if 2 mod 10
private static const op_ranks = new AATree!(TokenType, (a,b)=>a<b, int)(
	tuple(TokenType.dummy, 0),
	tuple(TokenType.end_of_file, 0),
	tuple(TokenType.pipeline, 500),
	tuple(TokenType.or,  600),
	tuple(TokenType.and, 700),
	tuple(TokenType.indexing, 800),
	tuple(TokenType.ls,  1102), tuple(TokenType.leq, 1102), tuple(TokenType.gt,  1102),
	tuple(TokenType.geq, 1102), tuple(TokenType.eq,  1102), tuple(TokenType.neq, 1102),
	tuple(TokenType.add, 1200), tuple(TokenType.sub, 1200), tuple(TokenType.cat, 1200),
	tuple(TokenType.mul, 1300), tuple(TokenType.div, 1300), tuple(TokenType.mod, 1300),
);
private enum Action { shift, reduce, error, accept, nonassociative, }
// return which is decendent
private Action precedence(TokenType a, TokenType b) {
	with (TokenType) {
		if ( (a == rPar  && b == lPar) ||
			 (a == dummy && b == rPar) ||
			 (a == lPar  && b == end_of_file)
		) return Action.error;
		
		else if ( a == dummy && b == end_of_file ) return Action.accept;
		//else if ( a == lPar  && b == rPar )        return OpPrec.equal;
		
		auto ar = op_ranks[a],
			 br = op_ranks[b];
		if (ar < br) 			return Action.shift;
		else if (ar > br) 		return Action.reduce;
		else if (ar % 10 == 0)	return Action.reduce;
		else if (ar % 10 == 1) 	return Action.shift;
		else if (ar % 10 == 2)	return Action.nonassociative;
		assert(0);
	}
}

private AST pipelineExpression(Range)(ref Range input, ref uint parenthesis_depth)
	if (isTokenRange!Range)
{
	enum Token dummy_token = {type: TokenType.dummy};
	debug(parser) { writeln("comma_expression"); scope(exit) writeln("end comma_expression"); }
	AST[] nodes;
	AST[] operators = [new BinaryExpression(dummy_token)];
	
	bool operator_expected = false;
	bool end_parsing = false;
	Token token;
	
	while (true) {
		if (end_parsing) token.type = TokenType.end_of_file, token.str = "EOF";
		else token = input.front;
		
		debug(parser) {
			nodes.each!(x => write(x.token.str, " "));
			write("///// ");
			operators.each!(x => write(x.token.str, " "));
			writeln("///// ", operator_expected);
		}
		
		// expressions with higher priorities expected
		if (!operator_expected) {
			// call UnaryExpression
			if (isFirstOfExpression(token.type)) {
				nodes ~= unaryExpression(input, parenthesis_depth);
				operator_expected = true;
			}
			// error
			else {
				writeln("Expression is expected, not " ~ token.str);
				return null;
			}
		}
		// binary operator expected
		else {

			//writeln("binary operator expected");
			// comma ',' must be enclosured by ( .... )
			if ( !end_parsing && !op_ranks.hasKey(token.type) ) {
				end_parsing = true;
				continue;
			}

			//writeln(operators[$-1].token.type, " ", token.type);
			// binary operator
			with(Action) switch (precedence(operators[$-1].token.type, token.type)) {
			case shift:
				operators ~= new BinaryExpression(token);
				input.popFront();	// get rid of the operator.
				operator_expected = false;
			break;
			
			case reduce:
				auto top = cast(BinaryExpression) operators[$-1];
				top.left = nodes[$-2], top.right = nodes[$-1];
				nodes[$-2] = operators[$-1];
				operators.length -= 1, nodes.length -= 1;
			break;
			
			case nonassociative:
				writeln("Expressions of the form a " ~ operators[$-1].token.str ~ " b " ~ token.str ~ " c is invalid.");
			return null;
			
			case accept:
				assert (nodes.length == 1);
			return nodes[0];
			
			case error:
				if (operators[$-1].token.type == TokenType.lPar && token.type == TokenType.end_of_file)  
					writeln("')' was not found.");
				else writeln("Invalid expression.");
			return null;
			
			default: assert(0);
			}
		}
	}
	//return null;
	assert(0);
}
/******************************************/

private AST unaryExpression(Range)(ref Range input, ref uint parenthesis_depth)
	if (isTokenRange!Range)
{
	debug(parser) { writeln("unary_expression"); scope(exit) writeln("end unary_expression"); }
	with(TokenType)
	if (input.front.type.among!(add, sub, and, mul, not)) {
		Token token_unary_op = { type: unary_op, str: "unary_op" };
		auto token_op = input.front;
		input.popFront();	// pop + -, &, *, !
		// error
		if (!input.front.type.isFirstOfExpression()) {
			writeln("An expression is expected after '" ~ token_op.str ~ "', not '" ~ input.front.str ~ "'");
			return null;
		}
		auto unary_expr = unaryExpression(input, parenthesis_depth);
		auto node = new BinaryExpression(token_unary_op, new AST(token_op), unary_expr);
		return node;
	}
	else if (input.front.type.isFirstOfExpression()) {
		return powerExpression(input, parenthesis_depth);
	}
	// error
	else {
		writeln("An expression is expected, not " ~ input.front.str);
		return null;
	}
}

private AST powerExpression(Range)(ref Range input, ref uint parenthesis_depth)
	if (isTokenRange!Range)
{
	debug(parser) { writeln("composition_expression"); scope(exit) writeln("end composition_expression"); }
	auto dot_expr = applyExpression(input, parenthesis_depth);
	if (input.front.type == TokenType.pow) {
		auto composite_token = input.front;
		input.popFront();	// get rid of @
		// check if the following is the start of expression
		if (!input.front.type.isFirstOfExpression()) { writeln("An expression is expected after '^^' "); return null; }	// error
		auto composition_expr = powerExpression(input, parenthesis_depth);
		return new BinaryExpression(composite_token, dot_expr, composition_expr);
	}
	else return dot_expr;
}

private AST applyExpression(Range)(ref Range input, ref uint parenthesis_depth)
	if (isTokenRange!Range)
{
	debug(parser) { writeln("apply_expression"); scope(exit) writeln("end apply_expression"); }
	auto result = compositionExpression(input, parenthesis_depth);
	Token token_app = { type:TokenType.app, str:"app" };
	// f - g is not parsed as f (-g)
	with (TokenType)
	while (!input.front.type.among!(add, sub) && input.front.type.isFirstOfExpression()) {
		auto comp_expr = compositionExpression(input, parenthesis_depth);
		result = new BinaryExpression(token_app, result, comp_expr);
	}
	return result;
}

private AST compositionExpression(Range)(ref Range input, ref uint parenthesis_depth)
	if (isTokenRange!Range)
{
	debug(parser) { writeln("composition_expression"); scope(exit) writeln("end composition_expression"); }
	auto dot_expr = dotExpression(input, parenthesis_depth);
	if (input.front.type == TokenType.composition) {
		auto composite_token = input.front;
		input.popFront();	// get rid of @
		// check if the following is the start of expression
		if (!input.front.type.isFirstOfExpression()) { writeln("An expression is expected after '@' "); return null; }	// error
		auto composition_expr = compositionExpression(input, parenthesis_depth);
		return new BinaryExpression(composite_token, dot_expr, composition_expr);
	}
	else return dot_expr;
}

private AST dotExpression(Range)(ref Range input, ref uint parenthesis_depth)
	if (isTokenRange!Range)
{
	debug(parser) { writeln("dot_expression"); scope(exit) writeln("end dot_expression"); }
	auto result = templateInstanceExpression(input, parenthesis_depth);
	Token token_dot = { type:TokenType.dot, str:"." };
	while (input.front.type == TokenType.dot) {
		input.popFront();		// get rid of .
		// error
		if (!input.front.type.isFirstOfExpression()) {
			writeln("An expression is expected after '.', not " ~ input.front.str);
			return null;
		}
		auto templ_inst_expr = templateInstanceExpression(input, parenthesis_depth);
		result = new BinaryExpression(token_dot, result, templ_inst_expr);
	}
	return result;
}

private AST templateInstanceExpression(Range)(ref Range input, ref uint parenthesis_depth)
	if (isTokenRange!Range)
{
	debug(parser) { writeln("template_instance_expression"); scope(exit) writeln("end template_instance_expression"); }
	import parser.type: isFirstOfType, functionType;
	auto result = atomExpression(input, parenthesis_depth);
	Token token_qt = { type:TokenType.template_instance_type, str:"::" };
	Token token_qe = { type:TokenType.template_instance_expr, str:"?"};
	bool is_type;
	while (true) {
		// :: type
		if (input.front.type == TokenType.template_instance_type) {
			input.popFront();		// get rid of ::
			// error
			if (!input.front.type.isFirstOfType()) {
				writeln("A type is expected after '::', not " ~ input.front.str);
				return null;
			}
			auto type = functionType(input);
			result = new BinaryExpression(token_qt, result, type);
		}
		// ? expr
		else if (input.front.type == TokenType.template_instance_expr) {
			input.popFront();		// get rid of ?
			// error
			if (!input.front.type.isFirstOfExpression()) {
				writeln("An expression is expected after '?', not " ~ input.front.str);
				return null;
			}
			auto expr = atomExpression(input, parenthesis_depth);
			result = new BinaryExpression(token_qe, result, expr);
		}
		else break;
	}
	return result;
}
/*
Lambda:
	\ : Type identifier FunctionArgumentsDeclarations = Expression ;
	\        identifier FunctionArgumentsDeclarations = Expression ;
	//\ : Type identifier FunctionArgumentsDeclarations BlockStatement ;
	//\        identifier FunctionArgumentsDeclarations BlockStatement ;

StructLiteral:
	struct ( Type ) { StructLiteralBodies }
StructLiteralBodies:
	empty
	identifier : Expression
	identifier : Expression , StructLiteralBodies
*/

private AST atomExpression(Range)(ref Range input, ref uint parenthesis_depth)
	if (isTokenRange!Range)
{
	debug(parser) { writeln("atom_expression"); scope(exit) writeln("end atom_expression"); }
	with(TokenType)
	if (input.front.type.among!(dollar, identifier, integer, real_number, string_literal, true_, false_, this_)) {
		auto token = input.front;
		input.popFront();	// get rid of the token
		return new AST(ASTType.expr, token);
	}
	else if (input.front.type == lPar) {
		++parenthesis_depth;
		input.popFront();	// get rid of (
		auto node = assignExpression(input, parenthesis_depth);
		if (input.front.type != rPar) {
			writeln("')' was not found.");
		}
		else { input.popFront(); }
		--parenthesis_depth;
		return node;
	}
	// lambda
	else if (input.front.type == lambda) {
		import parser.declaration: functionProcedureDeclaration;
		return functionProcedureDeclaration(input, true);
	}
	// struct literal
	else if (input.front.type == struct_) {
		assert (0, "struct literal has not implemented");
		/+import parser.type;
		auto struct_token = input.front;
		input.popFront();	// get rid of struct
		// error
		if (input.front.type != lPar) {
			writeln("'(' was expected for struct literals, not " ~ input.front.str);
			return null;
		}
		input.popFront();	// get rid of (
		auto type_node = Type(input);
		// error
		if (input.front.type != rPar) {
			writeln("Enclosure ')' was expected for struct literals, not " ~ input.front.str);
			return null;
		}
		input.popFront();	// get rid of )
		auto result = expr_node(struct_token, type_node);
		// error
		if (input.front.type != lBrace) {
			writeln("'{' was expected for struct literals, not" ~ input.front.str);
			return null;
		}
		input.popFront();	// get rid of {
		// StructLiteralBodies
		while (true) {
			if (input.front.type != identifier) break;
			auto id_node = new Node(input.front);
			input.popFront();	// get rid of identifier
			// error
			if (input.front.type != colon) {
				writeln("':' was expected for the struct literals, not " ~ input.front.str);
				input.popFront();
				continue;
			}
			input.popFront();	// get rid of colon
			if (!input.front.type.isFirstOfExpression()) {
				writeln("An expression was expected after ':', not " ~ input.front.str);
				input.popFront();
				continue;
			}
			id_node.child = [Expression(input)];
			result.child ~= id_node;
			if (input.front.type == comma) input.popFront();
		}
		// error
		if (input.front.type != rBrace) {
			writeln("Enclosure '}' was expected for struct literals, not " ~ input.front.str);
			return null;
		}
		return result;
		+/
	}
	// else if (input.front.type == struct)
	else {
		writeln("An atom expression is expected, not " ~ input.front.str);
		return null;
	}
}
/+
string stringofExpression(AST node) {
	import parser.defs: stringofNode;

	//auto node = cast (ExprNode) n;
	with(TokenType) switch (node.token.type) {
	case when:
		auto a = node.child[0], b = node.child[1], c = node.child[2];
		return "((" ~ stringofNode(a) ~ ") when (" ~ stringofNode(b) ~ ") else (" ~ stringofNode(c) ~ "))" ;
	case struct_:
		//return "struct () {}, " ~ node.child.length.to!string;
		string result = "struct(" ~ stringofNode(node.child[0]) ~ ") {";
		foreach (id_node; node.child[1 .. $]) {
			if (id_node is null) result ~= " /+ERROR+/, ";
			else result ~= id_node.token.str ~ ":" ~ stringofNode(id_node.child[0]) ~ ", ";
		}
		return result[0 .. $-2] ~ "}";
	default:
		if (node.child[0] is null && node.child[1] is null )
			if (node.token.type == TokenType.string_literal) return `"` ~ node.token.str ~ `"`;
			else return node.token.str;
		else return "(" ~ stringofNode(node.child[0]) ~ " " ~ node.token.str ~ " " ~ stringofNode(node.child[1]) ~ ")";
	}
}
+/
