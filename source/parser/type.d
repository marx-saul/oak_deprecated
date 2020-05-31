module parser.type;

import std.stdio, std.typecons, std.algorithm;
import parser.lexer, parser.defs;

unittest {
	writeln("\n#### parse/type.d unittest1");
	import parser.defs : TokenRange;
	
	//auto token_pusher = new TokenRange!string("f -> int[][string[a -> b]]");
	//auto token_pusher = new TokenRange!string("(a -> b) -> (b -> c) -> (a -> c)");
	auto token_pusher = new TokenRange!string("a[][int]");
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
	AtomType Indices

TypeIndices:
	empty
	[ ] TypeIndices
	[ Type ] TypeIndices
	* TypeIndices

AtomType:
	identifier
	int
	real
	string
	bool
	TupleType
	( )

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
	with(TokenType) return t.among!(identifier, int_, real_, string_, bool_, lPar, var) != 0;
}

alias Type = TemplateType;

TypeNode TemplateType(Range)(ref Range input)
	if (isTokenRange!Range)
{
	// 
	with(TokenType)
	if (input.front.type == identifier && input.lookahead.type.among!(template_instance_expr, template_instance_type)) {
		assert(0, "?, ?? type not implemented yet");
	}
	else return FunctionType(input);
	
}

TypeNode FunctionType(Range)(ref Range input)
	if (isTokenRange!Range)
{
	auto var_type = VarType(input);
	if (input.front.type == TokenType.right_arrow) {
		auto right_arrow_token = input.front;
		input.popFront();	// get rid of ->
		// check if the following is the start of type
		if (!input.front.type.isFirstOfType()) { writeln("A type is expected after '->' "); return null; }	// error
		auto function_type = FunctionType(input);
		return new TypeNode(right_arrow_token, var_type, function_type);
	}
	else return var_type;
}

TypeNode VarType(Range)(ref Range input)
	if (isTokenRange!Range)
{
	if (input.front.type == TokenType.var) {
		auto var_token = input.front;
		input.popFront();	// get rid of var
		// check if the following is the start of type
		if (!input.front.type.isFirstOfType()) { writeln("A type is expected after 'var' "); return null; }	// error
		auto list_type = ListType(input);
		return new TypeNode(var_token, list_type);
	}
	else {
		// check if the following is the start of type
		if (!input.front.type.isFirstOfType()) { writeln("A type is expected, not ", input.front.str); return null; }	// error
		else return ListType(input);
	}
}

TypeNode ListType(Range)(ref Range input) 
	if (isTokenRange!Range)
{
	auto atom_type = AtomType(input);
	auto type_indices = TypeIndices(input, atom_type);
	return type_indices;
}

// fst = top, snd = bottom
TypeNode TypeIndices(Range)(ref Range input, TypeNode atom_type) 
	if (isTokenRange!Range)
{
	auto top = atom_type;
	while (true) {
		with (TokenType)
		// [ 
		if      (input.front.type == lBrack) {
			auto lBrack_token = input.front;
			input.popFront();	// get rid of [
			// [ ]
			if (input.front.type == rBrack) {
				input.popFront();	// get rid of ]
				top = new TypeNode(lBrack_token, top, null);
			}
			// [ Type ]
			else if (input.front.type.isFirstOfType()) {
				auto type = Type(input);
				// missing ']' error
				if (input.front.type != rBrack) {
					writeln("']' is missing.");
				}
				else input.popFront();	// get rid of ]
				top = new TypeNode(lBrack_token, top, type);
			}
			// error
			else {
				writeln("[ ] or [ some type ] is expected.");
				break;
			}
		}
		else if (input.front.type == mul) {
			auto mul_token = input.front;
			input.popFront();	// get rid of *
			top = new TypeNode(mul_token, top, null);
		}
		else break;
	}
	return top;
}

TypeNode AtomType(Range)(ref Range input) 
	if (isTokenRange!Range)
{
	// TupleType
	if (input.front.type == TokenType.lPar) {
		return TupleType(input);
	}
	// int, real, bool, string, ...
	else if (isFirstOfType(input.front.type)) {
		auto token = input.front;
		input.popFront();
		return new TypeNode(token);
	}
	// error (somewhere not checking if it starts with FIRST(Type) when Type(Range) is called?)
	else assert(0, input.front.str);
}

TypeNode TupleType(Range)(ref Range input)
	if (isTokenRange!Range)
{
	assert (input.front.type == TokenType.lPar);
	input.popFront();	// get rid of (
	// empty tuple
	if (input.front.type == TokenType.rPar) {
		Token empty_tuple = { type: TokenType.empty_tuple, str: "()" };
		return new TypeNode(empty_tuple);
	}
	// check if the following is the start of type
	else if (!input.front.type.isFirstOfType()) { writeln("A type is expected after '(', not ", input.front.str); return null; }	// error
	
	TypeNode top = null;
	TypeNode bottom = Type(input);
	while (true) {
		if (input.front.type == TokenType.comma) {
			auto comma_token = input.front;
			input.popFront();	// get rid of ,
			// check if the following is the start of type
			if (!input.front.type.isFirstOfType()) { writeln("A type is expected after ',' in a tuple type, not ", input.front.str); break; }	// error
			auto right_type = Type(input);
			auto tuple_type = new TypeNode(comma_token, bottom, right_type);
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
				return new TypeNode(empty_tuple);
			}
			break;
		}
	}
	// ( a )
	if (top is null) return bottom;
	else return top;
}

string stringofType(Node n) {
	import parser.defs: stringofNode;
	if (n is null) return "";
	
	auto node = cast(TypeNode) n;
	//auto node = cast (ExprNode) n;
	with(TokenType) switch (node.token.type) {
	case lBrack:
		return stringofNode(node.left) ~ "[" ~ stringofNode(node.right) ~ "]";
	case mul:
		return stringofNode(node.left) ~ "*";
	case var:
		return "var (" ~ stringofNode(node.left) ~ ")";
	default:
		if (node.left is null && node.right is null ) return "" ~ node.token.str;
		else return "(" ~ stringofNode(node.left) ~ "" ~ node.token.str ~ stringofNode(node.right) ~ ")";
	
	}
}
