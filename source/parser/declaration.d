module parser.declaration;

import parser.parse: check;
import parser.lexer, parser.expression, parser.type, ast.all, message;


LetDeclaration letDeclaration(L)(L lex, Attribute attr) {
	auto loc_tmp = lex.loc;
	lex.nextToken(); // get rid of let
	Type whole_type;
	// let:int a = 0, ...;
	if (lex.token.type == TokenType.colon) {
		lex.nextToken(); // get rid of :
		whole_type = type(lex);
	}

	Symbol.Variable[] tids;
	Expression[] exprs;

	with(TokenType)
	while (lex.token.type == identifier) {
		auto name = lex.token.str;
		lex.nextToken(); // get rid of id
		Type ind_type;
		Expression expr;
		// a : T
		if (lex.token.type == TokenType.colon) {
			lex.nextToken(); // get rid of :
			ind_type = type(lex);
		}
		// a (: T) = E
		if (lex.token.type == TokenType.assign) {
			lex.nextToken(); // get rid of =
			expr = expression(lex);
		}

		tids ~= new Symbol.Variable(name, attr, ind_type ? ind_type : whole_type);
		if (tids[$ - 1].type) tids[$ - 1].type.attr &= (attr & Attribute.type_qualifier);
		exprs ~= expr;

		if (lex.token.type == comma) {
			lex.nextToken(); // get rid of ,
		}
		if (lex.token.type == semicolon) {
			lex.nextToken(); // get rid of )
			break;
		}
		if (lex.token.type == end_of_file) {
			error("Reached EOF when expecting ; in a let declaration.");
			break;
		}
	}

	return new LetDeclaration(loc_tmp, tids, exprs);
}

FunctionDeclaration functionDeclaration(L)(L lex, Attribute attr) {
	auto loc_tmp = lex.loc;
	lex.nextToken(); // get rid of func

	auto name = lex.token.str;
	lex.check(TokenType.identifier);

	Type ret_type;
	if (lex.token.type == TokenType.colon) {
		lex.nextToken(); // get rid of :
		ret_type = attributedType(lex, Attribute.ref_);
	}

	auto id = new Symbol.Function(name, Attribute.none, ret_type);

	// arguments
	Symbol.Variable[] args;

	import std.algorithm: among;
	with(TokenType)
	while (lex.token.type.among!(identifier, any)) {
		auto arg_name = lex.token.str;
		lex.nextToken(); // get rid of id
		Type arg_type;
		if (lex.token.type == TokenType.colon) {
			lex.nextToken(); // get rid of :
			arg_type = attributedType(lex, Attribute.ref_ | Attribute.lazy_);
		}
		args ~= new Symbol.Variable(arg_name, Attribute.none, arg_type);
	}

	Expression body;
	with(TokenType)
	if (lex.token.type == semicolon) {
		lex.nextToken(); // get rid of ;
	}
	else if (lex.token.type == TokenType.assign) {
		lex.nextToken(); // get rid of =
		body = expression(lex);
		lex.check(TokenType.semicolon);
	}
	else {
		error("= or ; expected for the function body.");
	}

	return new FunctionDeclaration(loc_tmp, attr, id, args, body);
}

