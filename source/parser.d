module parser;

import message, lexer, ast.astbase;
import std.algorithm, std.meta;

unittest {

}

final class Parser(AST, Lex)
	if (isLexer!Lex)
{

	Lex lex;
	this (Lex l) {
		lex = l;
	}
	Token token() @property { return lex.front; }
	Location loc()   @property { return lex.front.loc; }
	void nextToken() { lex.popFront(); }

	void check(TokenType tt, bool always_pop = true, string additional_msg = "") {
		if (tt != token.type) {
			error(tt.stringof ~ " expected, not '" ~ token.str ~ "' " ~ additional_msg);
			if (always_pop) lex.popFront();
		}
		else lex.popFront();
	}

	/* ************************* Expression ************************* */
	private pure bool isFirstOfExpression(TokenType t) @property {
		with (TokenType)
		return t.among!(
			integer, real_number, string_literal,
			true_, false_, this_, super_,
			identifier, dollar,
			u_sub, bit_not, not, ref_of, deref,
			lambda, lBrack, struct_,
			lPar,
		) != 0;
	}

	private bool isFirstOfExpression() @property {
		return isFirstOfExpression(token.type);
	}

	alias expression = assignExpression;

	AST.Expression assignExpression() {
		auto e = whenExpression();

		with (TokenType)
		if (token.type.among!(
			assign, add_assign, sub_assign, mul_assign, div_assign,
			mod_assign, cat_assign, and_assign, xor_assign,  or_assign,)) {
			auto tt = token.type;
			auto loc_tmp = loc;
			nextToken();	// get rid of _=
			auto e2 = assignExpression();
			e = new AST.BinaryExpression(loc_tmp, tt, e, e2);
		}

		return e;
	}

	AST.Expression whenExpression() {
		auto e = pipelineExpression();

		with (TokenType)
		if (token.type == when) {
			auto loc_tmp = loc;
			nextToken();	// get rid of when
			auto e2 = pipelineExpression();
			check(else_);	// get rid of else
			auto e3 = whenExpression();
			e = new AST.WhenExpression(loc_tmp, e, e2, e3);
		}

		return e;
	}

	AST.Expression pipelineExpression() {
		auto e = appExpression();

		with (TokenType)
		while (token.type == pipeline) {
			auto loc_tmp = loc;
			nextToken();	// get rid of |>
			auto e2 = appExpression();
			e = new AST.BinaryExpression(loc_tmp, pipeline, e, e2);
		}

		return e;
	}

	AST.Expression appExpression() {
		auto e = orExpression();

		with (TokenType)
		if (token.type == app) {
			auto loc_tmp = loc;
			nextToken();	// get rid of app
			auto e2 = orExpression();
			e = new AST.BinaryExpression(loc_tmp, app, e, e2);
		}

		return e;
	}

	AST.Expression orExpression() {
		auto e = xorExpression();

		with (TokenType)
		while (token.type == or) {
			auto loc_tmp = loc;
			nextToken();	// get rid of or
			auto e2 = xorExpression();
			e = new AST.BinaryExpression(loc_tmp, or, e, e2);
		}

		return e;
	}

	AST.Expression xorExpression() {
		auto e = andExpression();

		with (TokenType)
		while (token.type == xor) {
			auto loc_tmp = loc;
			nextToken();	// get rid of xor
			auto e2 = andExpression();
			e = new AST.BinaryExpression(loc_tmp, xor, e, e2);
		}

		return e;
	}

	AST.Expression andExpression() {
		auto e = bit_orExpression();

		with (TokenType)
		while (token.type == and) {
			auto loc_tmp = loc;
			nextToken();	// get rid of and
			auto e2 = bit_orExpression();
			e = new AST.BinaryExpression(loc_tmp, and, e, e2);
		}

		return e;
	}

	AST.Expression bit_orExpression() {
		auto e = bit_xorExpression();

		with (TokenType)
		while (token.type == bit_or) {
			auto loc_tmp = loc;
			nextToken();	// |
			auto e2 = bit_xorExpression();
			e = new AST.BinaryExpression(loc_tmp, bit_or, e, e2);
		}

		return e;
	}

	AST.Expression bit_xorExpression() {
		auto e = bit_andExpression();

		with (TokenType)
		while (token.type == bit_xor) {
			auto loc_tmp = loc;
			nextToken();	// get rid of ^
			auto e2 = bit_andExpression();
			e = new AST.BinaryExpression(loc_tmp, bit_xor, e, e2);
		}

		return e;
	}

	AST.Expression bit_andExpression() {
		auto e = compareExpression();

		with (TokenType)
		while (token.type == bit_and) {
			auto loc_tmp = loc;
			nextToken();	// get rid of and
			auto e2 = compareExpression();
			e = new AST.BinaryExpression(loc_tmp, bit_and, e, e2);
		}

		return e;
	}

	AST.Expression compareExpression() {
		auto e = shiftExpression();

		with (TokenType)
		if (token.type.among!(eq, neq, ls, gt, leq, geq, in_, nin, is_, nis)) {
			auto loc_tmp = loc;
			auto tt = token.type;
			nextToken();	// get rid of ==, ...
			auto e2 = shiftExpression();
			e = new AST.BinaryExpression(loc_tmp, tt, e, e2);
		}

		return e;
	}

	AST.Expression shiftExpression() {
		auto e = addExpression();

		with (TokenType)
		while (token.type.among!(lshift, rshift, log_shift)) {
			auto loc_tmp = loc;
			auto tt = token.type;
			nextToken();	// get rid of << >> >>>
			auto e2 = addExpression();
			e = new AST.BinaryExpression(loc_tmp, tt, e, e2);
		}

		return e;
	}

	AST.Expression addExpression() {
		auto e = mulExpression();

		with (TokenType)
		while (token.type.among!(add, sub, cat)) {
			auto loc_tmp = loc;
			auto tt = token.type;
			nextToken();	// get rid of + - ++
			auto e2 = mulExpression();
			e = new AST.BinaryExpression(loc_tmp, tt, e, e2);
		}

		return e;
	}

	AST.Expression mulExpression() {
		auto e = unaryExpression();

		with (TokenType)
		while (token.type.among!(mul, div, mod)) {
			auto loc_tmp = loc;
			auto tt = token.type;
			nextToken();	// get rid of * / %
			auto e2 = unaryExpression();
			e = new AST.BinaryExpression(loc_tmp, tt, e, e2);
		}

		return e;
	}

	AST.Expression unaryExpression() {
		with (TokenType)
		if (token.type.among!(u_sub, bit_not, not, ref_of, deref)) {
			auto loc_tmp = loc;
			auto tt = token.type;
			nextToken();	// get rid of -- ~ not # !
			return new AST.UnaryExpression(loc_tmp, tt, unaryExpression());
		}
		else return powExpression();
	}

	AST.Expression powExpression() {
		auto e = applyExpression();

		with (TokenType)
		if (token.type == pow) {
			auto loc_tmp = loc;
			nextToken();	// get rid of ^^
			auto e2 = applyExpression();
			e = new AST.BinaryExpression(loc_tmp, pow, e, e2);
		}

		return e;
	}

	AST.Expression applyExpression() {
		auto e = indexingExpression();

		with (TokenType)
		while (isFirstOfExpression()) {
			auto loc_tmp = loc;
			auto e2 = indexing_unaryExpression();
			e = new AST.BinaryExpression(loc_tmp, apply, e, e2);
		}

		return e;
	}

	AST.Expression indexing_unaryExpression() {
		with (TokenType)
		if (token.type.among!(u_sub, bit_not, not, ref_of, deref)) {
			auto loc_tmp = loc;
			auto tt = token.type;
			nextToken();	// get rid of -- ~ not # !
			return new AST.UnaryExpression(loc_tmp, tt, indexing_unaryExpression());
		}
		else return indexingExpression();
	}

	AST.Expression indexingExpression() {
		auto e = compositionExpression();

		with (TokenType)
		while (token.type == lBrace) {
			auto loc_tmp = loc;	// location of {
			nextToken();	// get rid of {
			auto e2 = expression();
			// { E2 .. E3 }
			if (token.type == dotdot) {
				nextToken();	// get rid of ..
				auto e3 = expression();
				check(rBrace);	// get rid of }
				e = new AST.SliceExpression(loc_tmp, e, e2, e3);
			}
			// { E2_0 , E2_1 , ... , E2_n }
			else {
				AST.Expression[] es = [e2];
				while (true) {
					if (token.type == comma) {
						nextToken();	// get rid of ,
					}
					if (token.type == rBrace) {
						nextToken();	// get rid of }
						break;
					}
					es ~= expression();
				}
				e = new AST.IndexExpression(loc_tmp, e, es);
			}
		}

		return e;
	}

	AST.Expression compositionExpression() {
		auto e = dotExpression();

		with (TokenType)
		while (token.type == composition) {
			auto loc_tmp = loc;
			nextToken();	// get rid of @
			auto e2 = dotExpression();
			e = new AST.BinaryExpression(loc_tmp, composition, e, e2);
		}

		return e;
	}

	AST.Expression dotExpression() {
		auto e = templateExpression();

		with (TokenType)
		while (token.type == dot) {
			auto loc_tmp = loc;
			nextToken();	// get rid of .
			auto e2 = templateExpression();
			e = new AST.BinaryExpression(loc_tmp, dot, e, e2);
		}

		return e;
	}

	AST.Expression templateExpression() {
		auto e = atomExpression();

		with (TokenType)
		while (token.type.among!(param_type, param_expr)) {
			auto loc_tmp = loc;
			auto tt = token.type;
			nextToken();	// get rid of .
			auto e2 = templateExpression();
			e = new AST.BinaryExpression(loc_tmp, tt, e, e2);
		}

		return e;
	}

	AST.Expression atomExpression() {
		with (TokenType)
		switch (token.type) {
		case integer:
			return integerExpression();
		case real_number:
			return realExpression();
		case string_literal:
			return stringExpression();

		case identifier:
			return identifierExpression();
		case dollar:
			return dollarExpression();
		case this_:
			return thisExpression();
		case super_:
			return superExpression();

		case lambda:
			assert (0, "lambda expression has not been implemented");
			//return lambdaExpression();
		case struct_:
			assert (0, "struct literal expression has not been implemented");
			//return structExpression();

		case lBrack:
			assert (0, "(associative) array expression has not been implemented");
			//return assocArrayExpression();

		case lPar:
			return tupleExpression();

		default:
			error("An expression expected, not " ~ token.str);
			nextToken();
			return null;
		}
	}

	AST.Expression integerExpression() {
		long val = 0;
		// Hex
		if      (token.str.length >= 2 && token.str[0..2].among!("0x", "0X")) {
			auto s = token.str[2 .. $];
			foreach (c; s) {
				if      (c.among!(aliasSeqOf!"0123456789")) { val = val*16 + c-'0'; }
				else if (c.among!(aliasSeqOf!"abcdef"))     { val = val*16 + c-'a'; }
				else if (c.among!(aliasSeqOf!"ABCDEF"))     { val = val*16 + c-'A'; }
				else if (c == '_') {}
			}
		}
		// binary
		else if (token.str.length >= 2 && token.str[0..2].among!("0b", "0B")) {
			auto s = token.str[2 .. $];
			foreach (c; s) {
				if      (c.among!(aliasSeqOf!"01")) { val = val*2 + c-'0'; }
				else if (c == '_') {}
			}
		}
		// 10
		else  {
			auto s = token.str;
			foreach (c; s) {
				if      (c.among!(aliasSeqOf!"0123456789")) { val = val*10 + c-'0'; }
				else if (c == '_') {}
			}
		}
		import std.stdio;
		writeln(token.str, " ", val);

		auto e = new AST.IntegerExpression(loc, val);
		nextToken();
		return e;
	}

	AST.Expression realExpression() {
		auto e = new AST.RealExpression(loc, 0.0);
		nextToken();
		return e;
	}

	AST.Expression stringExpression() {
		auto e = new AST.StringExpression(loc, token.str);
		nextToken();
		return e;
	}

	AST.Expression identifierExpression() {
		auto e = new AST.IdentifierExpression(loc, token.str);
		nextToken();
		return e;
	}
	AST.Expression dollarExpression() {
		auto e = new AST.DollarExpression(loc);
		nextToken();
		return e;
	}
	AST.Expression thisExpression() {
		auto e = new AST.ThisExpression(loc);
		nextToken();
		return e;
	}
	AST.Expression superExpression() {
		auto e = new AST.SuperExpression(loc);
		nextToken();
		return e;
	}

	// an associative array or an array
	//AST.Expression assocArrayExpression() {
	//	nextToken();	// get rid of [
	//	assert (0, "array literal not implemented");
	//}

	AST.Expression tupleExpression() {
		auto loc_tmp = loc;
		nextToken();	// get rid of (
		auto e1 = expression();
		// ( E ) = E

		AST.Expression[] es = [e1];
		with (TokenType)
		while (true) {
			if (token.type == comma) {
				nextToken();	// get rid of ,
			}
			if (token.type == rPar) {
				nextToken();	// get rid of )
				break;
			}
			if (token.type == end_of_file) {
				error("EOF found in a tuple expression.");
				break;
			}
			es ~= expression();
		}
		if (es.length > 1) return new AST.TupleExpression(loc_tmp, es);
		else return e1;
	}

}
