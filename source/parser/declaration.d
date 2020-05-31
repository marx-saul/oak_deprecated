module parser.declaration;

import std.stdio, std.typecons, std.algorithm;
import parser.lexer, parser.defs;

unittest {
	writeln("\n#### parse/declration.d unittest1");
	import parser.defs : TokenRange;
	
	//auto token_pusher = new TokenRange!string("def f:int 0 = 1");
	//auto token_pusher = new TokenRange!string("def f:int n:int = n * f(n-1)");
	auto token_pusher = new TokenRange!string("def anti_compose:(a->c) f:a->b g:b->c = g.f");
	auto node = FunctionDeclaration(token_pusher);
	node.stringofFunction().writeln();
}

/*
LetDeclaration:
	let IdentifierDeclaration ;

VarDeclaration:
	var IdentifierDeclaration ;

IdentifierDeclarations:
	IdentifierDeclaration , IdentifierDeclarations
	IdentifierDeclaration

IdentifierDeclaration:
	identifier : Type
	identifier : Type = Expression


FunctionDeclaration:
	def identifier : Type FunctionArgumentsDeclarations = Expression ;
	def identifier        FunctionArgumentsDeclarations = Expression ;

FunctionArgumentsDeclarations:
	FunctionArgumentsDeclaration FunctionArgumentdeclarations
	empty

FunctionArgumentsDeclaration:
	identifier
	identifier ( NonAssignExpression )
	identifier : Type
	identifier : Type ( NonAssignExpression )
	any
	any : Type
	Literal
*/

FunctionNode FunctionDeclaration(Range)(ref Range input)
	if (isTokenRange!Range)
{
	assert (input.front.type == TokenType.def);
	input.popFront();	// get rid of def
	
	// error
	if (input.front.type != TokenType.identifier) {
		writeln("An identifier is expected after def, not " ~ input.front.str);
		return null;
	}
	auto func_node = new FunctionNode(input.front.str);
	input.popFront();	// get rid of id
	// : Type
	if (input.front.type == TokenType.colon) {
		input.popFront();	// get rid of :
		
		import parser.type: isFirstOfType, Type;
		if (!input.front.type.isFirstOfType()) {
			// error
			writeln("A type is expected after :, not " ~ input.front.str);
			return null;
		}
		auto type = Type(input);
		func_node.return_type = type;
	}
	
	FunctionArgumentsDeclarations(input, func_node);
	
	// = 
	if (input.front.type == TokenType.assign) {
		input.popFront();	// get rid of =
		import parser.expression: Expression;
		func_node.func_body = Expression(input);
	}
	// block statements
	// else if (input.front.type == TokenType.assign) {}
	// error
	else {
		writeln("= is expected after def declaration, not " ~ input.front.str);
	}
	
	return func_node;
}

void FunctionArgumentsDeclarations(Range)(ref Range input, ref FunctionNode func_node) {
	import parser.expression: Expression;
	import parser.type: Type;
	
	with (TokenType)
	while (true) {
		// id
		if (input.front.type == identifier) {
			func_node.args ~= input.front.str;
			input.popFront();	// get rid of id
			// ( Expression )
			if (input.front.type == lPar) {
				func_node.args_types ~= null;
				input.popFront();	// get rid of (
				func_node.args_conditions ~= Expression(input);
				if (input.front.type != rPar) {
					writeln("Enclosure of a function argument condition ) was not found.");
				}
			}
			// : Type
			else if (input.front.type == colon) {
				input.popFront();	// get rid of :
				func_node.args_types ~= Type(input);
				// ( Expression )
				if (input.front.type == lPar) {
					input.popFront();	// get rid of (
					func_node.args_conditions ~= Expression(input);
					if (input.front.type != rPar) {
						writeln("Enclosure of a function argument condition ) was not found.");
					}
				}
				// empty
				else func_node.args_conditions ~= null;
			}
			else {
				func_node.args_types ~= null;
				func_node.args_conditions ~= null;
			}
		}
		// any
		else if (input.front.type == any) {
			func_node.args ~= "any";
			if (input.front.type == colon) {
				input.popFront();	// get rid of :
				func_node.args_types ~= Type(input);
			}
			else func_node.args_types ~= null;
			func_node.args_conditions ~= null;
		}
		// Literal
		else if (input.front.type.among!(integer, string_literal, real_number, true_, false_)) {
			func_node.args ~= "";
			func_node.args_types ~= null;
			func_node.args_conditions ~= new ExprNode(input.front);
			input.popFront();
		}
		else break;
	}
}

string stringofFunction(FunctionNode node) {
	import std.range: iota;
	import parser.type: stringofType;
	import parser.expression: stringofExpression;
	
	string result = node.func_name;
	if (node.return_type !is null) result ~= ":" ~ stringofType(node.return_type) ~ " ";
	else result ~= " ";
	
	foreach (i; 0 .. node.args.length) {
		// literal
		if (node.args[i] == "") {
			result ~= stringofExpression(node.args_conditions[i]) ~ " ";
		}
		else if (node.args[i] == "any") {
			result ~= "any";
			if (node.args_types[i] !is null) result ~= ":" ~ stringofType(node.args_types[i]) ~ " ";
		}
		else {
			result ~= node.args[i];
			if (node.args_types[i] !is null) result ~= ":" ~ stringofType(node.args_types[i]) ~ " ";
			if (node.args_conditions[i] !is null) result ~= "(" ~ stringofExpression(node.args_conditions[i]) ~ ")";
		}
	}
	result ~= "=" ~ stringofExpression(node.func_body);
	return result;
}
