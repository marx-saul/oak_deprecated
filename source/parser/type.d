module parser.type;

import std.stdio, std.typecons, std.algorithm;
import aatree, parser.lexer, parser.defs;
import parser.defs;

/*
Type:
	FunctionType

FunctionType:
	VarType -> FunctionType
	VarType

VarType:
	var ListType
	ListType

ListType:
	AtomType Indices
	AtomType

Indices:
	[ ]
	[ ] Indices
	[ Type ]
	[ Type ] Indices
	*
	* Indices

AtomType:
	int
	real
	string
	bool
	identifier
	( TypeList )
	
TypeList:
	Type , TypeList
	Type
	
*/

bool isAtomType(TokenType t) {
	with(TokenType) return t.among!(identifier, int_, real_, string_, bool_) != 0;
}
bool isFistOfType(TokenType t) {
	with(TokenType) return t.among!(identifier, int_, real_, string_, bool_, lPar, var) != 0;
}

TypeNode Type(Range)(ref Range input)
	if (isTokenRange!Range)
{
	int parenthesis_depth = 0;
	// error
	//if (!input.front.token.type.isFirstOfType()) {
		//
	//}
	return FunctionType(input, parenthesis_depth);
}

TypeNode FunctionType(Range)(ref Range input, ref int parenthesis_depth)
	if (isTokenRange!Range)
{
	auto var_type = VarType(input, parenthesis_depth);
	if (input.front.token.type == TokenType.right_arrow) {
		auto function_type = FunctionType(input, parenthesis_depth);
		return new TypeNode(input.front.token, var_type, functionType);
	}
	else return var_type;
}

TypeNode VarType(Range)(ref Range input, ref int parenthesis_depth)
	if (isTokenRange!Range)
{
	if (input.front.type == TokenType.var) {
		auto var_token = input.front;
		input.popFront();
		auto list_type = ListType(input, parenthesis_depth);
		return new TypeNode(var_token, list_type);
	}
	else {
		return ListType(input, parenthesis_depth);
	}
}

TypeNode ListType(Range)(ref Range input, ref int parenthesis_depth) 
	if (isTokenRange!Range)
{
	auto atom_type = AtomType(input, parenthesis_depth);
	auto indices = Indices(input);
	return null;
}

TypeNode ListType(Range)(ref Range input, ref int parenthesis_depth) 
	if (isTokenRange!Range)
{
	return null;
}
