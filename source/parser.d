module parser;

import message, lexer, ast.astbase, ast.symbol, attribute;
import std.algorithm, std.meta, std.conv;

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
			true_, false_, this_, super_, any, identifier, dollar,
			u_sub, bit_not, not, ref_of, deref,
			lambda, lBrack, struct_, lPar, lBrace,
			if_, while_, do_, for_, foreach_, foreach_reverse_,
			break_, continue_, return_,
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
			auto e2 = appExpression();
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
			case integer:          return integerExpression();
			case real_number:      return realExpression();
			case string_literal:   return stringExpression();
			case identifier:       return identifierExpression();
			case dollar:           return dollarExpression();
			case this_:            return thisExpression();
			case super_:           return superExpression();
			case lambda:           assert (0, "lambda expression has not been implemented");
			//return lambdaExpression();
			case struct_:          return structExpression();
			case lBrack:           return assocArrayExpression();
			case lPar:             return tupleExpression();
			case if_:              return ifElseExpression();
			case lBrace:           return blockExpression();
			case while_:           return whileStatement();
			case for_:             return forStatement();
			case foreach_:         return foreachStatement();
			case foreach_reverse_: return foreachReverseStatement();
			case break_:           return breakStatement();
			case continue_:        return continueStatement();
			case return_:          return returnStatement();

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

	AST.Expression structExpression() {
		auto loc_tmp = loc;
		nextToken();	// get rid of struct
		nextToken();	// get rid of (
		auto type = type();
		check(TokenType.rPar);
		check(TokenType.lBrace);
		string[] members;
		AST.Expression[] exprs;
		with (TokenType)
		while (true) {
			if (token.type == comma) {
				nextToken();	// get rid of ,
			}
			if (token.type == rBrace) {
				nextToken();	// get rid of }
				break;
			}
			if (token.type == end_of_file) {
				error("EOF found when expecting the enclosure ] of an associative array.");
				break;
			}

			if (token.type != identifier) {
				error("An identifier was expected as a member of a struuct literal.");
				nextToken();
				members ~= "";
			}
			else {
				members ~= token.str;
				nextToken();
			}
			check(TokenType.colon);
			exprs ~= expression();
		}
		return new AST.StructExpression(loc_tmp, type, members, exprs);
	}

	// an associative array or an array
	AST.Expression assocArrayExpression() {
		auto loc_tmp = loc;
		nextToken();	// get rid of [
		if (token.type == TokenType.rBrack) {
			nextToken();	// get rid of ]
			return new AST.ArrayExpression(loc_tmp, []);
		}

		auto e1 = expression();
		with (TokenType)
		// associative array
		if (token.type == colon) {
			nextToken();	// get rid of :
			auto v1 = expression();
			auto keys = [e1], values = [v1];
			while (true) {
				if (token.type == comma) {
					nextToken();	// get rid of ,
				}
				if (token.type == rBrack) {
					nextToken();	// get rid of ]
					break;
				}
				if (token.type == end_of_file) {
					error("EOF found when expecting the enclosure ] of an associative array.");
					break;
				}
				keys ~= expression();
				check(TokenType.colon);
				values ~= expression();
			}
			return new AST.AssocArrayExpression(loc_tmp, keys, values);
		}
		// array
		else if (token.type == comma) {
			auto es = [e1];
			while (true) {
				if (token.type == comma) {
					nextToken();	// get rid of ,
				}
				if (token.type == rBrack) {
					nextToken();	// get rid of ]
					break;
				}
				if (token.type == end_of_file) {
					error("Reached EOF when expecting ] of an array.");
					break;
				}
				es ~= expression();
			}
			return new AST.ArrayExpression(loc_tmp, es);
		}
		else if (token.type == rBrack) {
			nextToken();	// get rid of ]
			return new AST.ArrayExpression(loc_tmp, [e1]);
		}
		else {
			error("] expected for an array, not " ~ token.type.to!string);
			return new AST.ArrayExpression(loc_tmp, [e1]);
		}
	}

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
				error("Reached EOF when expecting ) of a tuple expression (a, b, ...)");
				break;
			}
			es ~= expression();
		}
		if (es.length > 1) return new AST.TupleExpression(loc_tmp, es);
		else return e1;
	}

	AST.IfElseExpression ifElseExpression() {
		auto loc_tmp = loc;
		nextToken();	// get rid of if
		auto cond = expression();
		check(TokenType.colon);
		auto if_block = expression();
		if (token.type == TokenType.else_) {
			nextToken();	// get rid of else
			auto else_block = expression();
			return new AST.IfElseExpression(loc_tmp, cond, if_block, else_block);
		}
		else {
			return new AST.IfElseExpression(loc_tmp, cond, if_block, new AST.UnitExpression(loc));
		}
	}

	AST.BlockExpression blockExpression() {
		auto loc_tmp = loc;
		nextToken();	// get rid of {
		AST.ASTNode[] ns;
		with (TokenType)
		do {
			// expression
			if (isFirstOfExpression()) {
				ns ~= expression();
				auto is_semicolon = false;
				if (token.type == semicolon) {
					nextToken();	// get rid of ;
					is_semicolon = true;
				}
				if (token.type == rBrace) {
					if (is_semicolon) ns ~= new AST.UnitExpression(loc);
					nextToken();	// get rid of }
					break;
				}
			}
			// let declaration
			else if (token.type == let) {
				ns ~= letDeclaration();
			}
			else if (token.type == func) {
				ns ~= functionDeclaration();
			}
			else {
				error("Invalid token '" ~ token.str ~ "' found in a block expression");
				nextToken();
			}

			if (token.type == rBrace) {
				nextToken();
				break;
			}
			else if (token.type == end_of_file) {
				error("Reached EOF when expecting }");
				break;
			}

		} while (true);
		if (ns.length == 0) ns = [new AST.UnitExpression(loc_tmp)];
		return new AST.BlockExpression(loc_tmp, ns);
	}

	/* *************************************** *
					 Statement
	 * *************************************** */
	AST.WhileStatement whileStatement() {
		auto loc_tmp = loc;
		nextToken();	// get rid of while
		auto cond = expression();
		check(TokenType.colon);
		auto body = expression();
		return new AST.WhileStatement(loc_tmp, cond, body);
	}

	AST.DoWhileStatement doWhileStatement() {
		auto loc_tmp = loc;
		nextToken();	// get rid of do
		auto body = expression();
		check(TokenType.while_);
		auto cond = expression();
		check(TokenType.semicolon);
		return new AST.DoWhileStatement(loc_tmp, body, cond);
	}

	AST.ForStatement forStatement() {
		auto loc_tmp = loc;
		nextToken();	// get rid of for
		auto init = expression();
		check(TokenType.semicolon);
		auto cond = expression();
		check(TokenType.semicolon);
		auto exec = expression();
		check(TokenType.colon);
		auto body = expression();
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

	/* *************************************** *
					   Type
	 * *************************************** */
	AST.Type type() {
		auto attr = typeQualifier();
		auto result = functionType();
		result.attr = attr;
		return result;
	}

	Attribute typeQualifier() {
		Attribute attr;
		with (TokenType)
		while (token.type.among!(immut, const_, inout_, ref_, lazy_)) {
			auto t = token.type;
			switch (token.type) {
				case immut:  attr &= Attribute.immut;  break;
				case const_: attr &= Attribute.const_; break;
				case inout_: attr &= Attribute.inout_; break;
				case ref_:   attr &= Attribute.ref_;   break;
				case lazy_:  attr &= Attribute.lazy_;  break;
				default: assert(0);
			}
		}
		return attr;
	}

	AST.Type functionType() {
		auto t = arrayType();
		if (token.type == TokenType.right_arrow) {
			auto loc_tmp = loc;
			nextToken();	// get rid of ->
			auto t2 = functionType();
			t = new AST.FunctionType(loc_tmp, t, t2);
		}
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
			auto t2 = type();
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
					 Declaration
	 * *************************************** */
	AST.LetDeclaration letDeclaration() {
		auto loc_tmp = loc;
		nextToken();	// get rid of let
		AST.Type whole_type;
		// let:int a = 0, ...;
		if (token.type == TokenType.colon) {
			nextToken();	// get rid of :
			whole_type = type();
		}

		AST.TypedIdentifier[] tids;
		AST.Expression[] exprs;

		with (TokenType)
		while (token.type == identifier) {
			auto name = token.str;
			nextToken();	// get rid of id
			AST.Type ind_type;
			AST.Expression expr;
			// a : T
			if (token.type == TokenType.colon) {
				nextToken();	// get rid of :
				ind_type = type();
			}
			// a (: T) = E
			if (token.type == TokenType.assign) {
				nextToken();	// get rid of =
				expr = expression();
			}

			tids ~= new AST.TypedIdentifier(name, Attribute.none, ind_type ? ind_type : whole_type);
			exprs ~= expr;

			if (token.type == comma) {
				nextToken();	// get rid of ,
			}
			if (token.type == semicolon) {
				nextToken();	// get rid of )
				break;
			}
			if (token.type == end_of_file) {
				error("Reached EOF when expecting ; in a let declaration.");
				break;
			}
		}

		return new AST.LetDeclaration(loc_tmp, tids, exprs);
	}

	AST.FunctionDeclaration functionDeclaration() {
		auto loc_tmp = loc;
		nextToken();	// get rid of func

		auto name = token.str;
		check(TokenType.identifier);

		AST.Type ret_type;
		if (token.type == TokenType.colon) {
			nextToken();	// get rid of :
			ret_type = type();
		}

		auto id = new AST.TypedIdentifier(name, Attribute.none, ret_type);

		// arguments
		AST.TypedIdentifier[] args;
		with (TokenType)
		while (token.type.among!(identifier, any)) {
			auto arg_name = token.str;
			nextToken();	// get rid of id
			AST.Type arg_type;
			if (token.type == TokenType.colon) {
				nextToken();	// get rid of :
				arg_type = type();
			}
			args ~= new AST.TypedIdentifier(arg_name, Attribute.none, arg_type);
		}

		AST.Expression body;
		with (TokenType)
		if (token.type == semicolon) {
			nextToken();	// get rid of ;
		}
		else if (token.type == TokenType.assign) {
			nextToken();	// get rid of =
			body = expression();
			check(TokenType.semicolon);
		}
		else {
			error("= or ; expected for the function body.");
		}

		return new AST.FunctionDeclaration(loc_tmp, Attribute.none, id, args, body);
	}

}
