module parser;

import message, lexer, ast.astbase, ast.symbol;
import std.algorithm, std.meta, std.conv;

unittest {

}

final class Parser(AST, Lex)
	if (isLexer!Lex)
{
	/* **** methods **** */
	Lex lex;
	this (Lex l) {
		lex = l;
	}
	Token token() @property { return lex.front; }
	Location loc()   @property { return lex.front.loc; }
	void nextToken() { lex.popFront(); }
	void check(TokenType tt, bool always_pop = true, string additional_msg = "") {
		if (tt != token.type) {
			error(tt.to!string ~ " expected, not '" ~ token.str ~ "' " ~ additional_msg);
			if (always_pop) lex.popFront();
		}
		else lex.popFront();
	}

	/* *************************************** *
					 Expression
	 * *************************************** */
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
		while (token.type == indexing) {
			auto loc_tmp = loc;	// location of ![
			nextToken();	// get rid of ![
			auto e2 = expression();
			// { E2 .. E3 }
			if (token.type == dotdot) {
				nextToken();	// get rid of ..
				auto e3 = expression();
				check(rBrack);	// get rid of ]
				e = new AST.SliceExpression(loc_tmp, e, e2, e3);
			}
			// ![ E2_0 , E2_1 , ... , E2_n ]
			else {
				AST.Expression[] es = [e2];
				while (true) {
					if (token.type == comma) {
						nextToken();	// get rid of ,
					}
					if (token.type == rBrack) {
						nextToken();	// get rid of ]
						break;
					}
					if (token.type == end_of_file) {
						error("EOF found when expecting the indexing ].");
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
		auto e = new AST.IdentifierExpression(loc, new Identifier(token.str));
		nextToken();
		return e;
	}
	AST.Expression dollarExpression() {
		auto sym = new Identifier(token.str);
		auto e = new AST.DollarExpression(loc, sym);
		nextToken();
		return e;
	}
	AST.Expression thisExpression() {
		auto sym = new Identifier(token.str);
		auto e = new AST.ThisExpression(loc, sym);
		nextToken();
		return e;
	}
	AST.Expression superExpression() {
		auto sym = new Identifier(token.str);
		auto e = new AST.SuperExpression(loc, sym);
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

		// ()
		if (token.type == TokenType.rPar) {
			nextToken();	// get rid of )
			return new AST.UnitExpression(loc_tmp);
		}

		auto e1 = expression();
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
				error("EOF found in a tuple expression (a, b, ...)");
				break;
			}
			es ~= expression();
		}
		if (es.length > 1) return new AST.TupleExpression(loc_tmp, es);
		else return e1;
	}

	/* *************************************** *
					   Type
	 * *************************************** */
	alias type = functionType;

	AST.Type functionType() {
		auto t = qualifiedType();
		if (token.type == TokenType.right_arrow) {
			auto loc_tmp = loc;
			nextToken();	// get rid of ->
			auto t2 = functionType();
			t = new AST.FunctionType(loc_tmp, t, t2);
		}
		return t;
	}

	AST.Type qualifiedType() {
		with (TokenType)
		switch (token.type) {
		case immut:
			nextToken();
			break;

		case const_:
			nextToken();
			break;

		case inout_:
			nextToken();
			break;

		default:
			break;
		}
		auto t = arrayType();
		return t;
	}

	// array, associative array, pointer
	AST.Type arrayType() {
		with (TokenType)
		if      (token.type == lBrack) {
			AST.Type t;
			auto loc_tmp = loc;
			nextToken();	// get rid of [
			auto k = type();
			// associative array
			if (token.type == colon) {
				nextToken();	// get rid of :
				auto v = type();
				t = new AST.AssocArrayType(loc_tmp, k, v);
			}
			// array
			else {
				t = new AST.ArrayType(loc_tmp, k);
			}
			check(TokenType.rBrack);
			return t;
		}
		else if (token.type == ref_of) {
			auto loc_tmp = loc;
			nextToken();	// get rid of #
			auto t2 = qualifiedType();
			return new AST.PointerType(loc_tmp, t2);
		}
		else return atomType();
	}

	AST.Type atomType() {
		with (TokenType)
		if      (token.type == lPar) {
			return tupleType();
		}
		else if (token.type.among!(int_, real_, string_, bool_, void_, unit)) {
			return primitiveType();
		}
		else if (token.type == identifier) {
			return identifierType();
		}
		else {
			error("Type expected. not " ~ token.str);
			nextToken();
			return null;
		}
	}

	AST.Type tupleType() {
		auto loc_tmp = loc;
		nextToken();	// get rid of (

		// ()
		if (token.type == TokenType.rPar) {
			nextToken();	// get rid of )
			error("() is the literal of the type 'unit', not the type itself.");
			return new AST.PrimitiveType(loc_tmp, TokenType.unit);
		}

		auto t1 = type();
		AST.Type[] ts = [t1];
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
				error("EOF found in a tuple type.");
				break;
			}
			ts ~= type();
		}
		if (ts.length > 1) return new AST.TupleType(loc_tmp, ts);
		else return t1;
	}

	AST.PrimitiveType primitiveType() {
		auto t = new AST.PrimitiveType(loc, token.type);
		nextToken();
		return t;
	}

	AST.IdentifierType identifierType() {
		auto t = new AST.IdentifierType(loc, new Identifier(token.str));
		nextToken();
		return t;
	}

	/* *************************************** *
					 Statement
	 * *************************************** */
	AST.Statement statement() {
		with (TokenType)
		if (isFirstOfExpression()) {
			return expressionStatement();
		}
		with (TokenType)
		switch (token.type) {
			case lBrace:           return blockStatement();
			case if_:              return ifElseStatement();
			case while_:           return whileStatement();
			case for_:             return forStatement();
        	case foreach_:         return foreachStatement();
        	case foreach_reverse_: return foreachReverseStatement();
        	case break_:           return breakStatement();
        	case continue_:        return continueStatement();
        	case return_:          return returnStatement();
			default:
				error("A statement expected, not " ~ token.str);
				return null;
		}
	}

	AST.ExpressionStatement expressionStatement() {
		auto loc_tmp = loc;
		auto e = expression();
		check(TokenType.semicolon);
		return new AST.ExpressionStatement(loc_tmp, e);
	}

	AST.BlockStatement blockStatement() {
		auto loc_tmp = loc;
		nextToken();	// get rid of {
		AST.Statement[] ss;
		with (TokenType)
		while (token.type != rBrace) {
			ss ~= statement();
			if (token.type == end_of_file) {
				error("Reached EOF when } was expected.");
				break;
			}
		}
		return new AST.BlockStatement(loc_tmp, ss);
	}

	AST.IfElseStatement ifElseStatement() {
		auto loc_tmp = loc;
		nextToken();	// get rid of if
		auto cond = expression();
		auto if_block = statement();
		if (token.type == TokenType.else_) {
			nextToken();	// get rid of else
			auto else_block = statement();
			return new AST.IfElseStatement(loc_tmp, cond, if_block, else_block);
		}
		else {
			return new AST.IfElseStatement(loc_tmp, cond, if_block, null);
		}
	}

	AST.WhileStatement whileStatement() {
		auto loc_tmp = loc;
		nextToken();	// get rid of while
		auto cond = expression();
		auto body = statement();
		return new AST.WhileStatement(loc_tmp, cond, body);
	}

	AST.DoWhileStatement doWhileStatement() {
		auto loc_tmp = loc;
		nextToken();	// get rid of do
		auto body = statement();
		check(TokenType.while_);
		auto cond = expression();
		check(TokenType.semicolon);
		return new AST.DoWhileStatement(loc_tmp, body, cond);
	}

	AST.ForStatement forStatement() {
		auto loc_tmp = loc;
		nextToken();	// get rid of for
		auto init = statement();
		auto cond = expression();
		check(TokenType.semicolon);
		auto exec = expression();
		auto body = statement();
		return new AST.ForStatement(loc_tmp, init, cond, exec, body);
	}

	AST.ForeachStatement foreachStatement() {
		//auto loc_tmp = loc;
		//nextToken();	// get rid of foreach
		return null;
	}

	AST.ForeachReverseStatement foreachReverseStatement() {
		//auto loc_tmp = loc;
		//nextToken();	// get rid of foreach
		return null;
	}

	AST.BreakStatement breakStatement() {
		auto loc_tmp = loc;
		nextToken();	// get rid of break
		Identifier label;
		if (token.type == TokenType.identifier) {
			label = new Identifier(token.str);
			nextToken();	// get rid of label
		}
		check(TokenType.semicolon);
		return new AST.BreakStatement(loc_tmp, label);
	}

	AST.ContinueStatement continueStatement() {
		auto loc_tmp = loc;
		nextToken();	// get rid of continue
		Identifier label;
		if (token.type == TokenType.identifier) {
			label = new Identifier(token.str);
			nextToken();	// get rid of label
		}
		check(TokenType.semicolon);
		return new AST.ContinueStatement(loc_tmp, label);
	}

	AST.ReturnStatement returnStatement() {
		auto loc_tmp = loc;
		nextToken();	// get rid of continue
		AST.Expression expr;
		if (isFirstOfExpression()) {
			expr = expression();
		}
		check(TokenType.semicolon);
		return new AST.ReturnStatement(loc_tmp, expr);
	}

}
