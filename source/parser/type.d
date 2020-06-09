module parser.type;

import std.stdio, std.typecons, std.algorithm;
import parser.lexer, parser.defs;

unittest {
	writeln("\n#### parse/type.d unittest1");
	import parser.defs : TokenRange;
	
	//auto token_pusher = new TokenRange!string("f -> int[][string[a -> b]]");
	//auto token_pusher = new TokenRange!string("(a -> b) -> (b -> c) -> (a -> c)");
	auto token_pusher = new TokenRange!string("*[var int:[a]]");
	auto node = Type(token_pusher);
	node.stringofType().writeln();
}

/*
Type:
	TemplateType

TemplateType:
	identifier TemplateArguments
	FunctionType

TemplateArguments:
	!  FunctionType
	!! FunctionType
	TemplateArguments !  FunctionType
	TemplateArguments !! FunctionType

FunctionType:
	VarType -> FunctionType
	VarType

VarType:
	var ListType
	ListType

ListType:
	[ Type : Type ]		// [T : S] is [] --- T, S
	[ Type ]				// [T] is [] --- T, null
	*Type					// *T is * --- T
	AtomType

AtomType:
	identifier
	int
	real
	string
	bool
	TupleType
	//( )

TupleType:
	( TypeList )

TypeList:
	Type , TypeList
	Type
	
*/

//bool isAtomType(TokenType t) {
//	with(TokenType) return t.among!(identifier, int_, real_, string_, bool_) != 0;
//}
bool isFirstOfType(TokenType t) {
	with(TokenType) return t.among!(identifier, int_, real_, string_, bool_, lPar, lBrack, mul, var) != 0;
}

alias Type = TemplateType;

Node TemplateType(Range)(ref Range input)
	if (isTokenRange!Range)
{
	// 
	with(TokenType)
	if (input.front.type == identifier && input.lookahead.type.among!(template_instance_expr, template_instance_type)) {
		assert(0, "?, ?? type not implemented yet");
	}
	else return FunctionType(input);
	
}

Node FunctionType(Range)(ref Range input)
	if (isTokenRange!Range)
{
	auto var_type = VarType(input);
	if (input.front.type == TokenType.right_arrow) {
		auto right_arrow_token = input.front;
		input.popFront();	// get rid of ->
		// check if the following is the start of type
		if (!input.front.type.isFirstOfType()) { writeln("A type is expected after '->' "); return null; }	// error
		auto function_type = FunctionType(input);
		return type_node(right_arrow_token, var_type, function_type);
	}
	else return var_type;
}

Node VarType(Range)(ref Range input)
	if (isTokenRange!Range)
{
	if (input.front.type == TokenType.var) {
		auto var_token = input.front;
		input.popFront();	// get rid of var
		// check if the following is the start of type
		if (!input.front.type.isFirstOfType()) { writeln("A type is expected after 'var' "); return null; }	// error
		auto list_type = ListType(input);
		return type_node(var_token, list_type);
	}
	else {
		// check if the following is the start of type
		if (!input.front.type.isFirstOfType()) { writeln("A type is expected, not ", input.front.str); return null; }	// error
		else return ListType(input);
	}
}

Node ListType(Range)(ref Range input) 
	if (isTokenRange!Range)
{
	// [ ListType ]  [ ListType : ListType ]
	if (input.front.type == TokenType.lBrack) {
		auto lBrack_token = input.front;
		input.popFront();	// get rid of [
		// error
		if (!input.front.type.isFirstOfType()) {
			writeln("A type is expected after '[', not ", input.front.str);
			return null;
		}

		auto left_type = Type(input);
		if (input.front.type == TokenType.colon) {
			input.popFront();	// get rid of :
			// error
			if (!input.front.type.isFirstOfType()) {
				writeln("Type is expected after '[', not ", input.front.str);
			}
			auto right_type = Type(input);
			// error
			if (input.front.type != TokenType.rBrack) {
				writeln("Enclosure ']' of [T:S] was not found.");
			}
			return type_node(lBrack_token, left_type, right_type);
		}
		else if (input.front.type == TokenType.rBrack) {
			input.popFront();	// get rid of ]
			return type_node(lBrack_token, left_type);
		}
		// error
		else {
			writeln("Enclosure ']' or a colon ':' in [a] are expected, not " ~ input.front.str);
			return type_node(lBrack_token, left_type);
		}
	}
	// *ListType
	else if (input.front.type == TokenType.mul) {
		auto mul_token = input.front;
		input.popFront();	// get rid of *
		// error
		if (!input.front.type.isFirstOfType()) {
			writeln("A type is expected after '[', not ", input.front.str);
			return null;
		}
		auto left_type = Type(input);
		return type_node(mul_token, left_type);
	}
	// AtomType
	else if (input.front.type.isFirstOfType()) {
			return AtomType(input);
	}
	else {
		writeln("A type is expected, not ", input.front.str);
		return null;
	}
}

Node AtomType(Range)(ref Range input) 
	if (isTokenRange!Range)
{
	// TupleType
	if (input.front.type == TokenType.lPar) {
		return TupleType(input);
	}
	// int, real, bool, string, ...
	else if (isFirstOfType(input.front.type)) {
		auto token = input.front;
		input.popFront();	// get rid of int/real,...
		return type_node(token);
	}
	// error (somewhere not checking if it starts with FIRST(Type) when Type(Range) is called?)
	else assert(0, input.front.str);
}

Node TupleType(Range)(ref Range input)
	if (isTokenRange!Range)
{
	assert (input.front.type == TokenType.lPar);
	input.popFront();	// get rid of (
	// empty tuple
	if (input.front.type == TokenType.rPar) {
		Token empty_tuple = { type: TokenType.empty_tuple, str: "()" };
		return type_node(empty_tuple);
	}
	// check if the following is the start of type
	else if (!input.front.type.isFirstOfType()) { writeln("A type is expected after '(', not ", input.front.str); return null; }	// error
	
	Node top = null;
	Node bottom = Type(input);
	while (true) {
		if (input.front.type == TokenType.comma) {
			auto comma_token = input.front;
			input.popFront();	// get rid of ,
			// check if the following is the start of type
			if (!input.front.type.isFirstOfType()) { writeln("A type is expected after ',' in a tuple type, not ", input.front.str); break; }	// error
			auto right_type = Type(input);
			auto tuple_type = type_node(comma_token, bottom, right_type);
			if (top is null) top = tuple_type;
			bottom = tuple_type;
		}
		else if (input.front.type == TokenType.rPar) {
			input.popFront();	// get rid of )
			break;
		}
		// error
		else {
			writeln("')' enclosing a tuple type is missing.");
			if (top is null) {
				Token empty_tuple = { type: TokenType.empty_tuple, str: "()" };
				return type_node(empty_tuple);
			}
			break;
		}
	}
	// ( a )
	if (top is null) return bottom;
	else return top;
}

string stringofType(Node node) {
	import parser.defs: stringofNode;
	if (node is null) return "";
	
	//auto node = cast (ExprNode) n;
	with(TokenType) switch (node.token.type) {
	case lBrack:
		if (node.child[1] is null) return "[" ~ stringofNode(node.child[0]) ~ "]";
		else return "[" ~ stringofNode(node.child[0]) ~ ":" ~ stringofNode(node.child[1]) ~ "]";
	case mul:
		return "*" ~ stringofNode(node.child[0]);
	case var:
		return "(var " ~ stringofNode(node.child[0]) ~ ")";
	default:
		if (node.child[0] is null && node.child[1] is null ) return "" ~ node.token.str;
		else return "(" ~ stringofNode(node.child[0]) ~ "" ~ node.token.str ~ stringofNode(node.child[1]) ~ ")";
	
	}
}
