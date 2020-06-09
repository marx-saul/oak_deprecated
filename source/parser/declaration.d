module parser.declaration;

import std.stdio, std.typecons, std.algorithm;
import parser.lexer, parser.defs;

unittest {
	writeln("\n#### parse/declration.d unittest1");
	import parser.defs : TokenRange;

	auto token_pusher = new TokenRange!string("func:a->c anti_compose f:a->b g:b->c = g.f");
	auto node = FunctionProcedureDeclaration(token_pusher);
	node.stringofFunction().writeln();

	token_pusher = new TokenRange!string("let w, x: (a->int)->a, y = \\t = 2*t, z:string = \"a string\";");
	node = LetVarDeclaration(token_pusher);
	node.stringofVariables().writeln();
}

/*
LetVarDeclaration:
	let IdentifierWithTypeDeclaration ;
	var IdentifierWithTypeDeclaration ;
	let IdentifierDeclaration ;
	var IdentifierDeclaration ;

IdentifierWithTypeDeclarations:
	IdentifierWithTypeDeclaration , IdentifierWithTypeDeclarations
	IdentifierWithTypeDeclaration

IdentifierWithTypeDeclaration:
	identifier
	identifier = Expression
	identifier : Type
	identifier : Type = Expression

IdentifierDeclarations:
	IdentifierDeclaration , IdentifierDeclarations
	IdentifierDeclaration

IdentifierDeclaration:
	identifier
	identifier = Expression
	identifier : Type
	identifier : Type = Expression


FunctionProcedureDeclaration:
	func : Type identifier FunctionArgumentsDeclarations = Expression ;
	func        identifier FunctionArgumentsDeclarations = Expression ;
	proc : Type identifier FunctionArgumentsDeclarations = Expression ;
	proc        identifier FunctionArgumentsDeclarations = Expression ;


FunctionArgumentsDeclarations:
	FunctionArgumentsDeclaration FunctionArgumentdeclarations
	empty

FunctionArgumentsDeclaration:
	identifier
	identifier ( Expression )
	identifier : Type
	identifier : Type ( Expression )
	any
	any : Type
	Literal
*/

/*
let w, x:S, y = E, z:T = E';
has AST:
let
|- w
   |- null
   |- null

|- x
   |- S
   |- null

|- y
   |- null
   |- E

|- z
   |- T
   |- E


let:T x = E, y = E'
has AST:
let
|- x
   |- T
   |- E

|- y
   |- T
   |- E'

*/

/*
func:T f w x:S y(E') z:R(E'') = E
has AST:
f
|- T

|- w
   |- null
   |- null

|- x
   |- S
   |- null

|- y
   |- null
   |- E'

|- z
   |- R
   |- E''

|- E

*********************

func g any 3 any:T = F
has AST :
g
|- null

|- any
   |- null
   |- null

|- 3

|- any
   |- T
   |- null
   
|- F

*/

Node FunctionProcedureDeclaration(Range)(ref Range input)
	if (isTokenRange!Range)
{
	input.popFront();	// get rid of func/proc

	Node type_node;
	// func : Type
	if (input.front.type == TokenType.colon) {
		input.popFront();
		import parser.type: isFirstOfType, Type;
		if (!input.front.type.isFirstOfType()) {
			// error
			writeln("A type is expected after :, not " ~ input.front.str);
		}
		else {
			type_node = Type(input);
		}
	}

	// func (: Type) id
	// error
	if (input.front.type != TokenType.identifier) {
		writeln("An identifier is expected after 'func (: T)', not " ~ input.front.str);
		return null;
	}
	auto func_node = new Node(input.front);
	func_node.child = [type_node];
	input.popFront();	// get rid of id

	FunctionArgumentsDeclarations(input, func_node);
	
	// = 
	if (input.front.type == TokenType.assign) {
		input.popFront();	// get rid of =
		import parser.expression: Expression;
		func_node.child ~= Expression(input);
	}
	// block statements
	// else if (input.front.type == TokenType.assign) {}
	// error
	else {
		writeln("= is expected after func/proc declaration, not " ~ input.front.str);
	}
	
	return func_node;
}

void FunctionArgumentsDeclarations(Range)(ref Range input, ref Node func_node, bool allow_condition = true, bool allow_literal = true) {
	import parser.expression: Expression;
	import parser.type: Type;
	
	with (TokenType)
	while (true) {
		// id
		if (input.front.type == identifier) {
			func_node.child ~= new Node(input.front);
			func_node.child[$-1].child.length = 2;
			input.popFront();	// get rid of id
			// ( Expression )
			if (allow_condition && input.front.type == lPar) {
				input.popFront();	// get rid of (
				func_node.child[$-1].child[1] = Expression(input);
				if (input.front.type != rPar) {
					writeln("Enclosure of a function argument condition ) was not found.");
				}
			}
			// : Type
			else if (input.front.type == colon) {
				input.popFront();	// get rid of :
				func_node.child[$-1].child[0] = Type(input);
				// ( Expression )
				if (input.front.type == lPar) {
					input.popFront();	// get rid of (
					func_node.child[$-1].child[1] = Expression(input);
					if (input.front.type != rPar) {
						writeln("Enclosure of a function argument condition ) was not found.");
					}
				}
			}
		}
		// any
		else if (input.front.type == any) {
			func_node.child ~= new Node(input.front);
			func_node.child[$-1].child.length = 1;
			func_node.child ~= new Node(input.front);
			if (input.front.type == colon) {
				input.popFront();	// get rid of :
				func_node.child[$-1].child[0] = Type(input);
			}
		}
		// Literal
		else if (allow_literal && input.front.type.among!(integer, string_literal, real_number, true_, false_)) {
			func_node.child ~= expr_node(input.front);
			input.popFront();
		}
		else break;
	}
}

string stringofFunction(Node node) {
	import parser.defs: stringofNode;
	
	if (node is null) return "";
	
	string result = node.token.str;
	if (node.child[0]) result ~= ": " ~ stringofNode(node.child[0]) ~ " ";
	foreach (node2; node.child[1 .. $-1]) {
		result ~= node2.token.str;
		if (node2.child[0]) result ~= ": " ~ stringofNode(node2.child[0]) ~ " ";
		if (node2.child[1]) result ~= "(" ~ stringofNode(node2.child[1]) ~ ") ";
	}
	result ~= "= " ~ stringofNode(node.child[$-1]);
	return result;
}


Node LetVarDeclaration(Range)(ref Range input)
	if (isTokenRange!Range)
{
	import parser.type: Type, isFirstOfType;
	import parser.expression: Expression, isFirstOfExpression;
	Token letvar_token = input.front;
	input.popFront();	// get rid of let/var
	auto result = new Node(NodeType.var, letvar_token);

	with(TokenType)
	// let: int x, y, ...
	if (input.front.type == colon) {
		input.popFront();	// get rid of :

		Node type_node;
		// error
		if (!input.front.type.isFirstOfType()) {
			writeln("A type expected after '" ~ letvar_token.str ~ ":', not " ~ input.front.str);
		}
		else type_node = Type(input);

		while (true) {
			// error
			if (input.front.type != identifier) {
				writeln("An identifier expected in the let/var declaration, not" ~ input.front.str);
				return result;
			}
			// id
			auto id_node = new Node(input.front);
			input.popFront();	// get rid of identifier

			// error: tried to write like let:int x:string
			if (input.front.type == colon) {
				writeln("Declarations like let:T x:S is invalid. It will be regarded as x: T.");
				input.popFront();	// get rid of :
				if (input.front.type.isFirstOfType()) Type(input);
			}

			// id (= E)
			Node expr_node;
			if (input.front.type == assign) {
				input.popFront();	// get rid of =
				// error
				if (!input.front.type.isFirstOfExpression()) {
					writeln("An expression expected after '" ~ letvar_token.str ~ ":', not " ~ input.front.str);
				}
				else expr_node = Expression(input);
			}

			id_node.child = [type_node, expr_node];
			result.child ~= id_node;

			// id (: T) (= Expression) ,
			if (input.front.type == comma) {
				input.popFront();	// get rid of ,
				continue;
			}
			// id (: T) (= Expression) ;
			else if (input.front.type == semicolon) {
				input.popFront();	// get rid of ;
				return result;
			}
			// error
			else {
				writeln("';' or ',' expected in the let/var declaration, not " ~ input.front.str);
				return result;
			}
		}
	}
	// let w, x:T, y = E, z:S = E'
	else if (input.front.type == identifier) {
		while (true) {
			// error
			if (input.front.type != identifier) {
				writeln("An identifier expected in the let/var declaration, not" ~ input.front.str);
				return result;
			}
			// id
			auto id_node = new Node(input.front);
			input.popFront();	// get rid of identifier

			// id (: T)
			Node type_node;
			if (input.front.type == colon) {
				input.popFront();	// get rid of :
				// error
				if (!input.front.type.isFirstOfType()) {
					writeln("A type expected after '" ~ letvar_token.str ~ ":', not " ~ input.front.str);
				}
				else type_node = Type(input);
			}

			// id (: T) (= E)
			// id = Expression
			Node expr_node;
			if (input.front.type == assign) {
				input.popFront();	// get rid of =
				// error
				if (!input.front.type.isFirstOfExpression()) {
					writeln("An expression expected after '" ~ letvar_token.str ~ ":', not " ~ input.front.str);
				}
				else expr_node = Expression(input);
			}

			id_node.child = [type_node, expr_node];
			result.child ~= id_node;

			// id (: T) (= Expression) ,
			if (input.front.type == comma) {
				input.popFront();	// get rid of ,
				continue;
			}
			// id (: T) (= Expression) ;
			else if (input.front.type == semicolon) {
				input.popFront();	// get rid of ;
				return result;
			}
			// error
			else {
				writeln("';' or ',' expected in the let/var declaration, not " ~ input.front.str);
				return result;
			}
		}
	}
	// error
	else {
		writeln("An identifier or ':' followed by a type expected after '" ~ letvar_token.str ~ "', not " ~ input.front.str);
		return null;
	}
}

string stringofVariables(Node node) {
	import parser.defs: stringofNode;

	if (node is null) return "";

	string result = node.token.str ~ " ";
	foreach (node2; node.child) {
		result ~= node2.token.str;
		if (node2.child[0]) result ~= ": " ~ stringofNode(node2.child[0]) ~ " ";
		if (node2.child[1]) result ~= " = " ~ stringofNode(node2.child[1]) ~ " ";
		result ~= ", ";
	}
	result = result[0 .. $-2] ~ ";";
	return result;
}
