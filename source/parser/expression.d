module parser.expression;

import parser.parse: check;
import parser.lexer, parser.attribute, parser.declaration, parser.type, ast.all, message;

pure bool isFirstOfExpression(TokenType t) {
	import std.algorithm: among;
	with(TokenType)
	return t.among!(
		integer, real_number, string_literal,
		true_, false_, this_, super_, any, identifier, dollar,
		u_sub, bit_not, not, ref_of, deref,
		lambda, lBrack, struct_, lPar, lBrace,
		if_, while_, do_, for_, foreach_, foreach_reverse_,
		break_, continue_, return_,
	) != 0;
}

alias expression = assignExpression;

Expression assignExpression(L)(L lex) {
	import std.algorithm: among;

	auto e = whenExpression(lex);

	with(TokenType)
	if (lex.token.type.among!(
			assign, add_assign, sub_assign, mul_assign, div_assign,
			mod_assign, cat_assign, and_assign, xor_assign, or_assign, )) {
		auto tt = lex.token.type;
		auto loc_tmp = lex.loc;
		lex.nextToken(); // get rid of _=
		auto e2 = assignExpression(lex);
		e = new BinaryExpression(loc_tmp, tt, e, e2);
	}

	return e;
}

Expression whenExpression(L)(L lex) {
	auto e = pipelineExpression(lex);

	with(TokenType)
	if (lex.token.type == when) {
		auto loc_tmp = lex.loc;
		lex.nextToken(); // get rid of when
		auto e2 = pipelineExpression(lex);
		lex.check(else_); // get rid of else
		auto e3 = whenExpression(lex);
		e = new WhenExpression(loc_tmp, e, e2, e3);
	}

	return e;
}

Expression pipelineExpression(L)(L lex) {
	auto e = appExpression(lex);

	with(TokenType)
	while (lex.token.type == pipeline) {
		auto loc_tmp = lex.loc;
		lex.nextToken(); // get rid of |>
		auto e2 = appExpression(lex);
		e = new BinaryExpression(loc_tmp, pipeline, e, e2);
	}

	return e;
}

Expression appExpression(L)(L lex) {
	auto e = orExpression(lex);

	with(TokenType)
	if (lex.token.type == app) {
		auto loc_tmp = lex.loc;
		lex.nextToken(); // get rid of app
		auto e2 = appExpression(lex);
		e = new BinaryExpression(loc_tmp, app, e, e2);
	}

	return e;
}

Expression orExpression(L)(L lex) {
	auto e = xorExpression(lex);

	with(TokenType)
	while (lex.token.type == or) {
		auto loc_tmp = lex.loc;
		lex.nextToken(); // get rid of or
		auto e2 = xorExpression(lex);
		e = new BinaryExpression(loc_tmp, or, e, e2);
	}

	return e;
}

Expression xorExpression(L)(L lex) {
	auto e = andExpression(lex);

	with(TokenType)
	while (lex.token.type == xor) {
		auto loc_tmp = lex.loc;
		lex.nextToken(); // get rid of xor
		auto e2 = andExpression(lex);
		e = new BinaryExpression(loc_tmp, xor, e, e2);
	}

	return e;
}

Expression andExpression(L)(L lex) {
	auto e = bit_orExpression(lex);

	with(TokenType)
	while (lex.token.type == and) {
		auto loc_tmp = lex.loc;
		lex.nextToken(); // get rid of and
		auto e2 = bit_orExpression(lex);
		e = new BinaryExpression(loc_tmp, and, e, e2);
	}

	return e;
}

Expression bit_orExpression(L)(L lex) {
	auto e = bit_xorExpression(lex);

	with(TokenType)
	while (lex.token.type == bit_or) {
		auto loc_tmp = lex.loc;
		lex.nextToken(); // |
		auto e2 = bit_xorExpression(lex);
		e = new BinaryExpression(loc_tmp, bit_or, e, e2);
	}

	return e;
}

Expression bit_xorExpression(L)(L lex) {
	auto e = bit_andExpression(lex);

	with(TokenType)
	while (lex.token.type == bit_xor) {
		auto loc_tmp = lex.loc;
		lex.nextToken(); // get rid of ^
		auto e2 = bit_andExpression(lex);
		e = new BinaryExpression(loc_tmp, bit_xor, e, e2);
	}

	return e;
}

Expression bit_andExpression(L)(L lex) {
	auto e = compareExpression(lex);

	with(TokenType)
	while (lex.token.type == bit_and) {
		auto loc_tmp = lex.loc;
		lex.nextToken(); // get rid of and
		auto e2 = compareExpression(lex);
		e = new BinaryExpression(loc_tmp, bit_and, e, e2);
	}

	return e;
}

Expression compareExpression(L)(L lex) {
	import std.algorithm: among;

	auto e = shiftExpression(lex);

	with(TokenType)
	if (lex.token.type.among!(eq, neq, ls, gt, leq, geq, in_, nin, is_, nis)) {
		auto loc_tmp = lex.loc;
		auto tt = lex.token.type;
		lex.nextToken(); // get rid of ==, ...
		auto e2 = shiftExpression(lex);
		e = new BinaryExpression(loc_tmp, tt, e, e2);
	}

	return e;
}

Expression shiftExpression(L)(L lex) {
	import std.algorithm: among;

	auto e = addExpression(lex);

	with(TokenType)
	while (lex.token.type.among!(lshift, rshift, log_shift)) {
		auto loc_tmp = lex.loc;
		auto tt = lex.token.type;
		lex.nextToken(); // get rid of << >> >>>
		auto e2 = addExpression(lex);
		e = new BinaryExpression(loc_tmp, tt, e, e2);
	}

	return e;
}

Expression addExpression(L)(L lex) {
	import std.algorithm: among;

	auto e = mulExpression(lex);

	with(TokenType)
	while (lex.token.type.among!(add, sub, cat)) {
		auto loc_tmp = lex.loc;
		auto tt = lex.token.type;
		lex.nextToken(); // get rid of + - ++
		auto e2 = mulExpression(lex);
		e = new BinaryExpression(loc_tmp, tt, e, e2);
	}

	return e;
}

Expression mulExpression(L)(L lex) {
	import std.algorithm: among;

	auto e = unaryExpression(lex);

	with(TokenType)
	while (lex.token.type.among!(mul, div, mod)) {
		auto loc_tmp = lex.loc;
		auto tt = lex.token.type;
		lex.nextToken(); // get rid of * / %
		auto e2 = unaryExpression(lex);
		e = new BinaryExpression(loc_tmp, tt, e, e2);
	}

	return e;
}

Expression unaryExpression(L)(L lex) {
	import std.algorithm: among;

	with(TokenType)
	if (lex.token.type.among!(u_sub, bit_not, not, ref_of, deref)) {
		auto loc_tmp = lex.loc;
		auto tt = lex.token.type;
		lex.nextToken(); // get rid of -- ~ not # !
		return new UnaryExpression(loc_tmp, tt, unaryExpression(lex));
	} else return powExpression(lex);
}

Expression powExpression(L)(L lex) {
	auto e = applyExpression(lex);

	with(TokenType)
	if (lex.token.type == pow) {
		auto loc_tmp = lex.loc;
		lex.nextToken(); // get rid of ^^
		auto e2 = applyExpression(lex);
		e = new BinaryExpression(loc_tmp, pow, e, e2);
	}

	return e;
}

Expression applyExpression(L)(L lex) {
	auto e = indexingExpression(lex);

	with(TokenType)
	while (isFirstOfExpression(lex.token.type)) {
		auto loc_tmp = lex.loc;
		auto e2 = indexing_unaryExpression(lex);
		e = new BinaryExpression(loc_tmp, apply, e, e2);
	}

	return e;
}

Expression indexing_unaryExpression(L)(L lex) {
	import std.algorithm: among;

	with(TokenType)
	if (lex.token.type.among!(u_sub, bit_not, not, ref_of, deref)) {
		auto loc_tmp = lex.loc;
		auto tt = lex.token.type;
		lex.nextToken(); // get rid of -- ~ not # !
		return new UnaryExpression(loc_tmp, tt, indexing_unaryExpression(lex));
	} else return indexingExpression(lex);
}

Expression indexingExpression(L)(L lex) {
	auto e = compositionExpression(lex);

	with(TokenType)
	while (lex.token.type == indexing) {
		auto loc_tmp = lex.loc; // location of ![
		lex.nextToken(); // get rid of ![
		auto e2 = expression(lex);
		// { E2 .. E3 }
		if (lex.token.type == dotdot) {
			lex.nextToken(); // get rid of ..
			auto e3 = expression(lex);
			lex.check(rBrack); // get rid of ]
			e = new SliceExpression(loc_tmp, e, e2, e3);
		}
		// ![ E2_0 , E2_1 , ... , E2_n ]
		else {
			Expression[] es = [e2];
			while (true) {
				if (lex.token.type == comma) {
					lex.nextToken(); // get rid of ,
				}
				if (lex.token.type == rBrack) {
					lex.nextToken(); // get rid of ]
					break;
				}
				if (lex.token.type == end_of_file) {
					error("EOF found when expecting the indexing ].");
					break;
				}
				es ~= expression(lex);
			}
			e = new IndexExpression(loc_tmp, e, es);
		}
	}

	return e;
}

Expression compositionExpression(L)(L lex) {
	auto e = dotExpression(lex);

	with(TokenType)
	while (lex.token.type == composition) {
		auto loc_tmp = lex.loc;
		lex.nextToken(); // get rid of @
		auto e2 = dotExpression(lex);
		e = new BinaryExpression(loc_tmp, composition, e, e2);
	}

	return e;
}

Expression dotExpression(L)(L lex) {
	auto e = templateExpression(lex);

	with(TokenType)
	while (lex.token.type == dot) {
		auto loc_tmp = lex.loc;
		lex.nextToken(); // get rid of .
		auto e2 = templateExpression(lex);
		e = new BinaryExpression(loc_tmp, dot, e, e2);
	}

	return e;
}

Expression templateExpression(L)(L lex) {
	import std.algorithm: among;

	auto e = atomExpression(lex);

	with(TokenType)
	while (lex.token.type.among!(param_type, param_expr)) {
		auto loc_tmp = lex.loc;
		auto tt = lex.token.type;
		lex.nextToken(); // get rid of .
		auto e2 = templateExpression(lex);
		e = new BinaryExpression(loc_tmp, tt, e, e2);
	}

	return e;
}

Expression atomExpression(L)(L lex) {
	import parser.statement;

	with(TokenType)
	switch (lex.token.type) {
	case integer:
		return integerExpression(lex);
	case real_number:
		return realExpression(lex);
	case string_literal:
		return stringExpression(lex);
	case identifier:
		return identifierExpression(lex);
	case dollar:
		return dollarExpression(lex);
	case this_:
		return thisExpression(lex);
	case super_:
		return superExpression(lex);
	case lambda:
		assert(0, "lambda expression has not been implemented");
		//return lambdaExpression(lex);
	case struct_:
		return structExpression(lex);
	case lBrack:
		return assocArrayExpression(lex);
	case lPar:
		return tupleExpression(lex);
	case if_:
		return ifElseExpression(lex);
	case lBrace:
		return blockExpression(lex);
	case while_:
		return whileStatement(lex);
	case for_:
		return forStatement(lex);
	case foreach_:
		return foreachStatement(lex);
	case foreach_reverse_:
		return foreachReverseStatement(lex);
	case break_:
		return breakStatement(lex);
	case continue_:
		return continueStatement(lex);
	case return_:
		return returnStatement(lex);

	default:
		error("An expression expected, not "~lex.token.str);
		lex.nextToken();
		return null;
	}
}

Expression integerExpression(L)(L lex) {
	import std.algorithm: among;
	import std.meta: aliasSeqOf;
	long val = 0;
	// Hex
	if (lex.token.str.length >= 2 && lex.token.str[0..2].among!("0x", "0X")) {
		auto s = lex.token.str[2..$];
		foreach(c; s) {
			if (c.among!(aliasSeqOf!"0123456789")) {
				val = val * 16 + c - '0';
			} else if (c.among!(aliasSeqOf!"abcdef")) {
				val = val * 16 + c - 'a';
			} else if (c.among!(aliasSeqOf!"ABCDEF")) {
				val = val * 16 + c - 'A';
			} else if (c == '_') {}
		}
	}
	// binary
	else if (lex.token.str.length >= 2 && lex.token.str[0..2].among!("0b", "0B")) {
		auto s = lex.token.str[2..$];
		foreach(c; s) {
			if (c.among!(aliasSeqOf!"01")) {
				val = val * 2 + c - '0';
			}
			else if (c == '_') {}
		}
	}
	// 10
	else {
		auto s = lex.token.str;
		foreach(c; s) {
			if (c.among!(aliasSeqOf!"0123456789")) {
				val = val * 10 + c - '0';
			} else if (c == '_') {}
		}
	}

	auto e = new IntegerExpression(lex.loc, val);
	lex.nextToken();
	return e;
}

Expression realExpression(L)(L lex) {
	auto e = new RealExpression(lex.loc, 0.0);
	lex.nextToken();
	return e;
}

Expression stringExpression(L)(L lex) {
	auto e = new StringExpression(lex.loc, lex.token.str);
	lex.nextToken();
	return e;
}

Expression identifierExpression(L)(L lex) {
	auto e = new IdentifierExpression(lex.loc, new Symbol.Identifier(lex.token.str));
	lex.nextToken();
	return e;
}
Expression dollarExpression(L)(L lex) {
	auto e = new DollarExpression(lex.loc, new Symbol.Identifier(lex.token.str));
	lex.nextToken();
	return e;
}
Expression thisExpression(L)(L lex) {
	auto e = new ThisExpression(lex.loc, new Symbol.Identifier(lex.token.str));
	lex.nextToken();
	return e;
}
Expression superExpression(L)(L lex) {
	auto e = new SuperExpression(lex.loc, new Symbol.Identifier(lex.token.str));
	lex.nextToken();
	return e;
}

Expression structExpression(L)(L lex) {
	auto loc_tmp = lex.loc;
	lex.nextToken(); // get rid of struct
	lex.nextToken(); // get rid of (
	auto type = parser.type.type(lex);
	lex.check(TokenType.rPar);
	lex.check(TokenType.lBrace);
	string[] members;
	Expression[] exprs;
	with(TokenType)
	while (true) {
		if (lex.token.type == comma) {
			lex.nextToken(); // get rid of ,
		}
		if (lex.token.type == rBrace) {
			lex.nextToken(); // get rid of }
			break;
		}
		if (lex.token.type == end_of_file) {
			error("EOF found when expecting the enclosure ] of an associative array.");
			break;
		}

		if (lex.token.type != identifier) {
			error("An identifier was expected as a member of a struuct literal.");
			lex.nextToken();
			members ~= "";
		}
		else {
			members ~= lex.token.str;
			lex.nextToken();
		}
		lex.check(TokenType.colon);
		exprs ~= expression(lex);
	}
	return new StructExpression(loc_tmp, type, members, exprs);
}

// an associative array or an array
Expression assocArrayExpression(L)(L lex) {
	auto loc_tmp = lex.loc;
	lex.nextToken(); // get rid of [
	if (lex.token.type == TokenType.rBrack) {
		lex.nextToken(); // get rid of ]
		return new ArrayExpression(loc_tmp, []);
	}

	auto e1 = expression(lex);
	with(TokenType)
	// associative array
	if (lex.token.type == colon) {
		lex.nextToken(); // get rid of :
		auto v1 = expression(lex);
		auto keys = [e1], values = [v1];
		while (true) {
			if (lex.token.type == comma) {
				lex.nextToken(); // get rid of ,
			}
			if (lex.token.type == rBrack) {
				lex.nextToken(); // get rid of ]
				break;
			}
			if (lex.token.type == end_of_file) {
				error("EOF found when expecting the enclosure ] of an associative array.");
				break;
			}
			keys ~= expression(lex);
			lex.check(TokenType.colon);
			values ~= expression(lex);
		}
		return new AssocArrayExpression(loc_tmp, keys, values);
	}
	// array
	else if (lex.token.type == comma) {
		auto es = [e1];
		while (true) {
			if (lex.token.type == comma) {
				lex.nextToken(); // get rid of ,
			}
			if (lex.token.type == rBrack) {
				lex.nextToken(); // get rid of ]
				break;
			}
			if (lex.token.type == end_of_file) {
				error("Reached EOF when expecting ] of an array.");
				break;
			}
			es ~= expression(lex);
		}
		return new ArrayExpression(loc_tmp, es);
	}
	else if (lex.token.type == rBrack) {
		lex.nextToken(); // get rid of ]
		return new ArrayExpression(loc_tmp, [e1]);
	}
	else {
		import std.conv: to;
		error("] expected for an array, not " ~ lex.token.type.to!string);
		return new ArrayExpression(loc_tmp, [e1]);
	}
}

Expression tupleExpression(L)(L lex) {
	auto loc_tmp = lex.loc;
	lex.nextToken(); // get rid of (

	// ()
	if (lex.token.type == TokenType.rPar) {
		lex.nextToken(); // get rid of )
		return new UnitExpression(loc_tmp);
	}

	auto e1 = expression(lex);
	Expression[] es = [e1];
	with(TokenType)
	while (true) {
		if (lex.token.type == comma) {
			lex.nextToken(); // get rid of ,
		}
		if (lex.token.type == rPar) {
			lex.nextToken(); // get rid of )
			break;
		}
		if (lex.token.type == end_of_file) {
			error("Reached EOF when expecting ) of a tuple expression (a, b, ...)");
			break;
		}
		es ~= expression(lex);
	}
	if (es.length > 1) return new TupleExpression(loc_tmp, es);
	else return e1;
}

IfElseExpression ifElseExpression(L)(L lex) {
	auto loc_tmp = lex.loc;
	lex.nextToken(); // get rid of if
	auto cond = expression(lex);
	lex.check(TokenType.colon);
	auto if_block = expression(lex);
	if (lex.token.type == TokenType.else_) {
		lex.nextToken(); // get rid of else
		auto else_block = expression(lex);
		return new IfElseExpression(loc_tmp, cond, if_block, else_block);
	} else {
		return new IfElseExpression(loc_tmp, cond, if_block, new UnitExpression(lex.loc));
	}
}

BlockExpression blockExpression(L)(L lex) {
	auto loc_tmp = lex.loc;
	lex.nextToken(); // get rid of {
	ASTNode[] ns;
	with(TokenType)
	do {
		// label
		if (lex.token.type == identifier && lex.lookahead.type == colon) {
			ns ~= new LabelDeclaration(lex.loc, lex.token.str);
			lex.nextToken(); // get rid of identifier
			lex.nextToken(); // get rid of :
			continue;
		}
		// expression
		if (isFirstOfExpression(lex.token.type)) {
			ns ~= expression(lex);
			auto is_semicolon = false;
			if (lex.token.type == semicolon) {
				lex.nextToken(); // get rid of ;
				is_semicolon = true;
			}
			if (lex.token.type == rBrace) {
				if (is_semicolon) ns ~= new UnitExpression(lex.loc);
				lex.nextToken(); // get rid of }
				break;
			}
		}
		// declaration
		else with(Attribute) {
			auto attr = attribute(lex,
				immut | const_ | inout_ | private_ | protected_ | package_ | public_ | export_ |
				final_ | static_ | pure_ | safe | trusted | system | throwable
			);
			with(TokenType)
			switch (lex.token.type) {
			case let:
				ns ~= letDeclaration(lex, attr);
				break;
			case func:
				ns ~= functionDeclaration(lex, attr);
				break;
			default:
				error("An expression or declarations expected in a block expression, not "~lex.token.str);
				lex.nextToken();
				break;
			}
		}

		if (lex.token.type == rBrace) {
			lex.nextToken();
			break;
		}
		else if (lex.token.type == end_of_file) {
			error("Reached EOF when expecting }");
			break;
		}

	}
	while (true);
	if (ns.length == 0) ns = [new UnitExpression(loc_tmp)];
	return new BlockExpression(loc_tmp, ns);
}
