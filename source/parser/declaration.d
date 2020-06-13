module parser.declaration;

import std.stdio, std.typecons, std.algorithm;
import parser.lexer, parser.defs;

unittest {
	writeln("\n#### parse/declration.d unittest1");
	import parser.defs : TokenRange;

	//auto token_pusher = new TokenRange!string("func:a->c anti_compose f:a->b g:b->c = g.f");
	auto token_pusher = new TokenRange!string("func:int f n:int(n>0) = n * f(n-1);");
	AST node = functionProcedureDeclaration(token_pusher);
	node.stringof.writeln();

	token_pusher = new TokenRange!string("let w, x: a->int->a, y = \\t = 2*t, z:string = \"a string\";");
	node = letVarDeclaration(token_pusher);
	node.stringof.writeln();

	token_pusher = new TokenRange!string("var x:(int, real) = (9, 0xFF.FF), y:(var int, [int],) = (12,2,);");
	node = letVarDeclaration(token_pusher);
	node.stringof.writeln();
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
	//func : Type identifier FunctionArgumentsDeclarations BlockStatement
	//func        identifier FunctionArgumentsDeclarations BlockStatement
	//proc : Type identifier FunctionArgumentsDeclarations BlockStatement
	//proc        identifier FunctionArgumentsDeclarations BlockStatement
	//func : Type identifier FunctionArgumentsDeclarations ;
	//func        identifier FunctionArgumentsDeclarations ;
	//proc : Type identifier FunctionArgumentsDeclarations ;
	//proc        identifier FunctionArgumentsDeclarations ;
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

StructDeclaration:
	struct identifier { StructDeclarationBodies }
StructDeclarationBodies:
	StructDeclarationBody StructDeclarationBodies
	empty
StructDeclarationBody:
	LetVarDeclaration ;
	FuncProcDeclaration ;
*/


LetDeclaration letVarDeclaration(Range)(ref Range input)
	if (isTokenRange!Range)
{
	import parser.type: type, isFirstOfType;
	import parser.expression: expression, isFirstOfExpression;
	Token letvar_token = input.front;
	input.popFront();	// get rid of let/var
	auto result = new LetDeclaration(letvar_token);

	with(TokenType)
	// let/var : int x, y, ...
	if (input.front.type == colon) {
		input.popFront();	// get rid of :

		AST type_node;
		// error
		if (!input.front.type.isFirstOfType()) {
			writeln("A type expected after '" ~ letvar_token.str ~ ":', not " ~ input.front.str);
		}
		else type_node = type(input);

		while (true) {
			// error
			if (input.front.type != identifier) {
				writeln("An identifier expected in the let/var declaration, not" ~ input.front.str);
				return result;
			}
			// id
			auto id_node = new Symbol;
			id_node.name = input.front;
			input.popFront();	// get rid of identifier

			// error: tried to write like let:int x:string
			if (input.front.type == colon) {
				writeln("Declarations like let:T x:S is invalid. It will be regarded as x: T.");
				input.popFront();	// get rid of :
				if (input.front.type.isFirstOfType()) type(input);
			}

			// id (= E)
			AST expr_node;
			if (input.front.type == assign) {
				input.popFront();	// get rid of =
				// error
				if (!input.front.type.isFirstOfExpression()) {
					writeln("An expression expected after '" ~ letvar_token.str ~ ":', not " ~ input.front.str);
				}
				else expr_node = expression(input);
			}

			// var x: int    is same as     let x: var int
			if (letvar_token.type == TokenType.var) {
				if (type_node !is null && type_node.token.type == TokenType.var) {
					writeln("Declaration of the form 'var x: var T' was found.");
				}
				else {
					auto var_type_node = new BinaryType(letvar_token, type_node);
					type_node = var_type_node;
				}
			}
			id_node.type = type_node;
			id_node.body = expr_node;
			result.symbols ~= id_node;

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
	// let/var w, x:T, y = E, z:S = E'
	else if (input.front.type == identifier) {
		while (true) {
			// error
			if (input.front.type != identifier) {
				writeln("An identifier expected in the let/var declaration, not" ~ input.front.str);
				return result;
			}
			// id
			auto id_node = new Symbol;
			id_node.name = input.front;
			input.popFront();	// get rid of identifier

			// id (: T)
			AST type_node;
			if (input.front.type == colon) {
				input.popFront();	// get rid of :
				// error
				if (!input.front.type.isFirstOfType()) {
					writeln("A type expected after '" ~ letvar_token.str ~ ":', not " ~ input.front.str);
				}
				else type_node = type(input);
			}

			// id (: T) (= E)
			// id = Expression
			AST expr_node;
			if (input.front.type == assign) {
				input.popFront();	// get rid of =
				// error
				if (!input.front.type.isFirstOfExpression()) {
					writeln("An expression expected after '" ~ letvar_token.str ~ ":', not " ~ input.front.str);
				}
				else expr_node = expression(input);
			}

			// var x: int    is same as     let x: var int
			if (letvar_token.type == TokenType.var) {
				if (type_node !is null && type_node.token.type == TokenType.var) {
					writeln("Declaration of the form 'var x: var T' was found.");
				}
				else {
					auto var_type_node = new BinaryType(letvar_token, type_node);
					type_node = var_type_node;
				}
			}
			id_node.type = type_node;
			id_node.body = expr_node;
			result.symbols ~= id_node;

			// id (: T) (= Expression) ,
			if (input.front.type == comma) {
				input.popFront();	// get rid of ,
			}
			// id (: T) (= Expression) ;
			if (input.front.type == semicolon) {
				input.popFront();	// get rid of ;
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

Function functionProcedureDeclaration(Range)(ref Range input, bool is_lambda = false)
	if (isTokenRange!Range)
{
	auto result = new Function;
	result.token = input.front;
	input.popFront();	// get rid of func/proc

	AST type_node;
	// func : Type
	if (input.front.type == TokenType.colon) {
		input.popFront();
		import parser.type: isFirstOfType, type;
		if (!input.front.type.isFirstOfType()) {
			// error
			writeln("A type is expected after ':', not " ~ input.front.str);
		}
		else {
			type_node = type(input);
		}
	}
	result.type = type_node;

	// func (: Type) id
	// error
	if (!is_lambda && input.front.type != TokenType.identifier) {
		writeln("An identifier is expected after 'func (: T)', not " ~ input.front.str);
		return null;
	}

	if (!is_lambda) {
		result.name = input.front;
		input.popFront();	// get rid of id
	}
	else {
		result.name.type = TokenType.lambda;
		result.name.str = "\\";
	}

	functionArgumentsDeclarations(input, result, is_lambda);

	AST expr_node;

	with (TokenType)
	// = Expression ;
	if (input.front.type == assign) {
		import parser.expression: isFirstOfExpression, expression;
		input.popFront();	// get rid of =
		if (!input.front.type.isFirstOfExpression()) {
			// error
			writeln("A type is expected after '=', not " ~ input.front.str);
		}
		else {
			expr_node = expression(input);
		}
		result.body = expr_node;
		if ( !is_lambda ) {
			// error
			if (input.front.type != semicolon) {
				writeln("';' was expected after the expression, not " ~ input.front.str);
			}
			else input.popFront();	// get rid of ;
		}
	}
	// block statements
	// else if (input.front.type == TokenType.lBrace) {}
	// error
	else {
		writeln("= is expected after func/proc declaration, not " ~ input.front.str);
	}
	
	return result;
}

void functionArgumentsDeclarations(Range)(ref Range input, ref Function func_node, bool is_lambda) {
	import parser.expression: isFirstOfExpression, expression;
	import parser.type: isFirstOfType, type;
	import std.conv: to;

	with (TokenType)
	while (true) {
		// id
		if (input.front.type.among!(identifier, any)) {
			Symbol argument = new Symbol; argument.name = input.front;
			AST expr_node;
			AST type_node;
			input.popFront();	// get rid of id

			// : Type
			if (input.front.type == colon) {
				input.popFront();	// get rid of (
				if (!input.front.type.isFirstOfType()) {
					writeln("An expression expected after '(', not " ~ input.front.str);
				}
				else {
					type_node = type(input);
				}

			}
			// (: Type) ( Expression )
			if (input.front.type == lPar) {
				// error
				if ( argument.name.type == any ) {
					writeln("The argument 'any' cannot have a condition");
				}
				// error
				if ( is_lambda ) {
					writeln("Conditions are not allowed for lambdas.");
				}
				input.popFront();	// get rid of (
				if (!input.front.type.isFirstOfExpression()) {
					writeln("An expression expected after '(', not " ~ input.front.str);
				}
				else {
					expr_node = expression(input);
					if (input.front.type != rPar) {
						writeln("Enclosure of a function argument condition ) was not found.");
					}
					else input.popFront();	// get rid of )
				}
			}
			argument.type = type_node;
			func_node.arguments ~= argument;
			func_node.conditions ~= expr_node;
		}
		// Literal
		else if (input.front.type.among!(integer, string_literal, real_number, true_, false_)) {
			// lambdas
			if ( is_lambda ) {
				writeln("Literals for arguments are not allowed");
			}
			auto argument = new Symbol;

			Token arg_temp = { str:"__" ~ func_node.arguments.length.to!string, type:TokenType.identifier };
			argument.name = arg_temp;

			Token type_temp = { str:type_of_literal[input.front.type], type:reserved_words[type_of_literal[input.front.type]] };
			argument.type = new AST(ASTType.type, type_temp);

			func_node.arguments ~= argument;

			Token token_eq = { str:"==", type:TokenType.eq };
			func_node.conditions ~= new BinaryExpression(token_eq, new AST(ASTType.expr, arg_temp), new AST(ASTType.expr, input.front));

			input.popFront();	// get rid of the literal
		}
		else break;
	}
}

Struct structDeclaration(Range)(ref Range input)
	if (isTokenRange!Range)
{
	auto struct_token = input.front;
	input.popFront();	// get rid of struct
	// error
	if (input.front.type != TokenType.identifier) {
		writeln("An identifier is expected after 'struct', not " ~ input.front.str);
		return null;
	}
	// struct identifier
	auto result = new Struct(input.front);
	input.popFront();	// get rid of identifier;
	// error
	if (input.front.type != TokenType.lBrace) {
		writeln("'{' is expected after 'struct " ~ result.token.str ~ "', not " ~ input.front.str);
		return null;
	}
	// struct identifer {
	else input.popFront();	// get rid of {
	result.members = structDeclarationBoodies(input);
	if (input.front.type != TokenType.rBrace) {
		writeln("Enclosure '}' is expected, not " ~ input.front.str);
		return null;
	}
	else input.popFront();	// get rid of }
	return result;
}

Symbol[] structDeclarationBoodies(Range)(ref Range input)
	if (isTokenRange!Range)
{
	Symbol[] result;
	with (TokenType)
	while (true) {
		if (input.front.type.among!(func, proc)) result ~= functionProcedureDeclaration(input);
		else if (input.front.type.among!(let, var)) result ~= letVarDeclaration(input).symbols;
		else if (input.front.type == rBrace) break;
		// error
		else {
			writeln("A function or let/var declaration is expected, not " ~ input.front.str);
			break;
		}
	}
	return result;
}

alias classDeclaration = structDeclaration;
