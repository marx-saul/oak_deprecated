module parser.type;

import std.stdio, std.typecons, std.algorithm;
import parser.lexer, parser.defs;

unittest {
	writeln("\n#### parse/type.d unittest1");
	import parser.defs : TokenRange;
	
	//auto token_pusher = new TokenRange!string("f -> int[][string[a -> b]]");
	//auto token_pusher = new TokenRange!string("(a -> b) -> (b -> c) -> (a -> c)");
	auto token_pusher = new TokenRange!string("*[var int:[a]]");
	auto node = type(token_pusher);
	node.stringof.writeln();
}

/*
Type:
	TemplateType

TemplateType:
	identifier TemplateArguments
	FunctionType

TemplateArguments:
	:: FunctionType
	?  Expression
	TemplateArguments :: FunctionType
	TemplateArguments ?  FunctionType

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

alias type = templateType;

AST templateType(Range)(ref Range input)
	if (isTokenRange!Range)
{
	// 
	with (TokenType)
	if (input.front.type == identifier && input.lookahead.type.among!(template_instance_expr, template_instance_type)) {
		assert(0, "template instancing type has not implemented");
	}
	else return functionType(input);
	
}

AST functionType(Range)(ref Range input)
	if (isTokenRange!Range)
{
	auto var_type = varType(input);
	if (input.front.type == TokenType.right_arrow) {
		auto right_arrow_token = input.front;
		input.popFront();	// get rid of ->
		// check if the following is the start of type
		if (!input.front.type.isFirstOfType()) { writeln("A type is expected after '->' "); return null; }	// error
		auto function_type = functionType(input);
		return new BinaryType(right_arrow_token, var_type, function_type);
	}
	else return var_type;
}

AST varType(Range)(ref Range input)
	if (isTokenRange!Range)
{
	if (input.front.type == TokenType.var) {
		auto var_token = input.front;
		input.popFront();	// get rid of var
		// check if the following is the start of type
		if (!input.front.type.isFirstOfType()) { writeln("A type is expected after 'var' "); return null; }	// error
		auto list_type = listType(input);
		return new BinaryType(var_token, list_type);
	}
	else {
		// check if the following is the start of type
		if (!input.front.type.isFirstOfType()) { writeln("A type is expected, not ", input.front.str); return null; }	// error
		else return listType(input);
	}
}

AST listType(Range)(ref Range input)
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

		auto left_type = type(input);
		if (input.front.type == TokenType.colon) {
			input.popFront();	// get rid of :
			// error
			if (!input.front.type.isFirstOfType()) {
				writeln("Type is expected after '[', not ", input.front.str);
			}
			auto right_type = type(input);
			// error
			if (input.front.type != TokenType.rBrack) {
				writeln("Enclosure ']' of [T:S] was not found.");
			}
			return new BinaryType(lBrack_token, left_type, right_type);
		}
		else if (input.front.type == TokenType.rBrack) {
			input.popFront();	// get rid of ]
			return new BinaryType(lBrack_token, left_type);
		}
		// error
		else {
			writeln("Enclosure ']' or a colon ':' in [a] are expected, not " ~ input.front.str);
			return new BinaryType(lBrack_token, left_type);
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
		auto left_type = type(input);
		return new BinaryType(mul_token, left_type);
	}
	// AtomType
	else if (input.front.type.isFirstOfType()) {
			return atomType(input);
	}
	else {
		writeln("A type is expected, not ", input.front.str);
		return null;
	}
}

AST atomType(Range)(ref Range input)
	if (isTokenRange!Range)
{
	// TupleType
	if (input.front.type == TokenType.lPar) {
		return tupleType(input);
	}
	// int, real, bool, string, ...
	else if (isFirstOfType(input.front.type)) {
		auto token = input.front;
		input.popFront();	// get rid of int/real,...
		return new AST(ASTType.type, token);
	}
	// error (somewhere not checking if it starts with FIRST(Type) when Type(Range) is called?)
	else assert(0, input.front.str);
}

AST tupleType(Range)(ref Range input)
	if (isTokenRange!Range)
{
	AST[] members;
	input.popFront();	// get rid of (
	while (true) {
		if (input.front.type.isFirstOfType()) {
			members ~= type(input);
		}
		else if (input.front.type == TokenType.rPar) {
			break;
		}
		// error
		else {
			writeln("A type is expected in a tuple (T, ...), not " ~ input.front.str);
			input.popFront();
			break;
		}

		if (input.front.type == TokenType.comma) input.popFront();	// get rid of ,
	}
	// (T) = T
	if (members.length == 1) return members[0];
	else if (members.length == 0) return null;
	else {
		auto result = new TupleType;
		result.types = members;
		return result;
	}
}
/+
string stringofType(AST node) {
	import parser.defs: stringofNode;
	
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
+/
