module parser.parse;

import message, parser.lexer, ast.all;
import std.algorithm, std.meta, std.conv;

void check(L)(L lex, TokenType tt, bool always_pop = true, string additional_msg = "") {
	if (tt != lex.token.type) {
		error(tt.to!string ~ " expected, not '" ~ lex.token.str ~ "' " ~ additional_msg);
		if (always_pop) lex.nextToken();
	}
	else lex.nextToken();
}


/+
final class Parser(Lex)
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
	private pure bool isFirstOfExpression(TokenType t) {
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

	private bool isFirstOfExpression() {
		return isFirstOfExpression(token.type);
	}

	alias expression = assignExpression;

	Expression assignExpression() {
		auto e = whenExpression();

		with (TokenType)
		if (token.type.among!(
			assign, add_assign, sub_assign, mul_assign, div_assign,
			mod_assign, cat_assign, and_assign, xor_assign,  or_assign,)) {
			auto tt = token.type;
			auto loc_tmp = loc;
			nextToken();	// get rid of _=
			auto e2 = assignExpression();
			e = new BinaryExpression(loc_tmp, tt, e, e2);
		}

		return e;
	}

	Expression whenExpression() {
		auto e = pipelineExpression();

		with (TokenType)
		if (token.type == when) {
			auto loc_tmp = loc;
			nextToken();	// get rid of when
			auto e2 = pipelineExpression();
			check(else_);	// get rid of else
			auto e3 = whenExpression();
			e = new WhenExpression(loc_tmp, e, e2, e3);
		}

		return e;
	}

	Expression pipelineExpression() {
		auto e = appExpression();

		with (TokenType)
		while (token.type == pipeline) {
			auto loc_tmp = loc;
			nextToken();	// get rid of |>
			auto e2 = appExpression();
			e = new BinaryExpression(loc_tmp, pipeline, e, e2);
		}

		return e;
	}

	Expression appExpression() {
		auto e = orExpression();

		with (TokenType)
		if (token.type == app) {
			auto loc_tmp = loc;
			nextToken();	// get rid of app
			auto e2 = appExpression();
			e = new BinaryExpression(loc_tmp, app, e, e2);
		}

		return e;
	}

	Expression orExpression() {
		auto e = xorExpression();

		with (TokenType)
		while (token.type == or) {
			auto loc_tmp = loc;
			nextToken();	// get rid of or
			auto e2 = xorExpression();
			e = new BinaryExpression(loc_tmp, or, e, e2);
		}

		return e;
	}

	Expression xorExpression() {
		auto e = andExpression();

		with (TokenType)
		while (token.type == xor) {
			auto loc_tmp = loc;
			nextToken();	// get rid of xor
			auto e2 = andExpression();
			e = new BinaryExpression(loc_tmp, xor, e, e2);
		}

		return e;
	}

	Expression andExpression() {
		auto e = bit_orExpression();

		with (TokenType)
		while (token.type == and) {
			auto loc_tmp = loc;
			nextToken();	// get rid of and
			auto e2 = bit_orExpression();
			e = new BinaryExpression(loc_tmp, and, e, e2);
		}

		return e;
	}

	Expression bit_orExpression() {
		auto e = bit_xorExpression();

		with (TokenType)
		while (token.type == bit_or) {
			auto loc_tmp = loc;
			nextToken();	// |
			auto e2 = bit_xorExpression();
			e = new BinaryExpression(loc_tmp, bit_or, e, e2);
		}

		return e;
	}

	Expression bit_xorExpression() {
		auto e = bit_andExpression();

		with (TokenType)
		while (token.type == bit_xor) {
			auto loc_tmp = loc;
			nextToken();	// get rid of ^
			auto e2 = bit_andExpression();
			e = new BinaryExpression(loc_tmp, bit_xor, e, e2);
		}

		return e;
	}

	Expression bit_andExpression() {
		auto e = compareExpression();

		with (TokenType)
		while (token.type == bit_and) {
			auto loc_tmp = loc;
			nextToken();	// get rid of and
			auto e2 = compareExpression();
			e = new BinaryExpression(loc_tmp, bit_and, e, e2);
		}

		return e;
	}

	Expression compareExpression() {
		auto e = shiftExpression();

		with (TokenType)
		if (token.type.among!(eq, neq, ls, gt, leq, geq, in_, nin, is_, nis)) {
			auto loc_tmp = loc;
			auto tt = token.type;
			nextToken();	// get rid of ==, ...
			auto e2 = shiftExpression();
			e = new BinaryExpression(loc_tmp, tt, e, e2);
		}

		return e;
	}

	Expression shiftExpression() {
		auto e = addExpression();

		with (TokenType)
		while (token.type.among!(lshift, rshift, log_shift)) {
			auto loc_tmp = loc;
			auto tt = token.type;
			nextToken();	// get rid of << >> >>>
			auto e2 = addExpression();
			e = new BinaryExpression(loc_tmp, tt, e, e2);
		}

		return e;
	}

	Expression addExpression() {
		auto e = mulExpression();

		with (TokenType)
		while (token.type.among!(add, sub, cat)) {
			auto loc_tmp = loc;
			auto tt = token.type;
			nextToken();	// get rid of + - ++
			auto e2 = mulExpression();
			e = new BinaryExpression(loc_tmp, tt, e, e2);
		}

		return e;
	}

	Expression mulExpression() {
		auto e = unaryExpression();

		with (TokenType)
		while (token.type.among!(mul, div, mod)) {
			auto loc_tmp = loc;
			auto tt = token.type;
			nextToken();	// get rid of * / %
			auto e2 = unaryExpression();
			e = new BinaryExpression(loc_tmp, tt, e, e2);
		}

		return e;
	}

	Expression unaryExpression() {
		with (TokenType)
		if (token.type.among!(u_sub, bit_not, not, ref_of, deref)) {
			auto loc_tmp = loc;
			auto tt = token.type;
			nextToken();	// get rid of -- ~ not # !
			return new UnaryExpression(loc_tmp, tt, unaryExpression());
		}
		else return powExpression();
	}

	Expression powExpression() {
		auto e = applyExpression();

		with (TokenType)
		if (token.type == pow) {
			auto loc_tmp = loc;
			nextToken();	// get rid of ^^
			auto e2 = applyExpression();
			e = new BinaryExpression(loc_tmp, pow, e, e2);
		}

		return e;
	}

	Expression applyExpression() {
		auto e = indexingExpression();

		with (TokenType)
		while (isFirstOfExpression()) {
			auto loc_tmp = loc;
			auto e2 = indexing_unaryExpression();
			e = new BinaryExpression(loc_tmp, apply, e, e2);
		}

		return e;
	}

	Expression indexing_unaryExpression() {
		with (TokenType)
		if (token.type.among!(u_sub, bit_not, not, ref_of, deref)) {
			auto loc_tmp = loc;
			auto tt = token.type;
			nextToken();	// get rid of -- ~ not # !
			return new UnaryExpression(loc_tmp, tt, indexing_unaryExpression());
		}
		else return indexingExpression();
	}

	Expression indexingExpression() {
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
				e = new SliceExpression(loc_tmp, e, e2, e3);
			}
			// ![ E2_0 , E2_1 , ... , E2_n ]
			else {
				Expression[] es = [e2];
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
				e = new IndexExpression(loc_tmp, e, es);
			}
		}

		return e;
	}

	Expression compositionExpression() {
		auto e = dotExpression();

		with (TokenType)
		while (token.type == composition) {
			auto loc_tmp = loc;
			nextToken();	// get rid of @
			auto e2 = dotExpression();
			e = new BinaryExpression(loc_tmp, composition, e, e2);
		}

		return e;
	}

	Expression dotExpression() {
		auto e = templateExpression();

		with (TokenType)
		while (token.type == dot) {
			auto loc_tmp = loc;
			nextToken();	// get rid of .
			auto e2 = templateExpression();
			e = new BinaryExpression(loc_tmp, dot, e, e2);
		}

		return e;
	}

	Expression templateExpression() {
		auto e = atomExpression();

		with (TokenType)
		while (token.type.among!(param_type, param_expr)) {
			auto loc_tmp = loc;
			auto tt = token.type;
			nextToken();	// get rid of .
			auto e2 = templateExpression();
			e = new BinaryExpression(loc_tmp, tt, e, e2);
		}

		return e;
	}

	Expression atomExpression() {
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

	Expression integerExpression() {
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

		auto e = new IntegerExpression(loc, val);
		nextToken();
		return e;
	}

	Expression realExpression() {
		auto e = new RealExpression(loc, 0.0);
		nextToken();
		return e;
	}

	Expression stringExpression() {
		auto e = new StringExpression(loc, token.str);
		nextToken();
		return e;
	}

	Expression identifierExpression() {
		auto e = new IdentifierExpression(loc, new Identifier(token.str));
		nextToken();
		return e;
	}
	Expression dollarExpression() {
		auto sym = new Identifier(token.str);
		auto e = new DollarExpression(loc, sym);
		nextToken();
		return e;
	}
	Expression thisExpression() {
		auto sym = new Identifier(token.str);
		auto e = new ThisExpression(loc, sym);
		nextToken();
		return e;
	}
	Expression superExpression() {
		auto sym = new Identifier(token.str);
		auto e = new SuperExpression(loc, sym);
		nextToken();
		return e;
	}

	Expression structExpression() {
		auto loc_tmp = loc;
		nextToken();	// get rid of struct
		nextToken();	// get rid of (
		auto type = type();
		check(TokenType.rPar);
		check(TokenType.lBrace);
		string[] members;
		Expression[] exprs;
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
		return new StructExpression(loc_tmp, type, members, exprs);
	}

	// an associative array or an array
	Expression assocArrayExpression() {
		auto loc_tmp = loc;
		nextToken();	// get rid of [
		if (token.type == TokenType.rBrack) {
			nextToken();	// get rid of ]
			return new ArrayExpression(loc_tmp, []);
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
			return new AssocArrayExpression(loc_tmp, keys, values);
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
			return new ArrayExpression(loc_tmp, es);
		}
		else if (token.type == rBrack) {
			nextToken();	// get rid of ]
			return new ArrayExpression(loc_tmp, [e1]);
		}
		else {
			error("] expected for an array, not " ~ token.type.to!string);
			return new ArrayExpression(loc_tmp, [e1]);
		}
	}

	Expression tupleExpression() {
		auto loc_tmp = loc;
		nextToken();	// get rid of (

		// ()
		if (token.type == TokenType.rPar) {
			nextToken();	// get rid of )
			return new UnitExpression(loc_tmp);
		}

		auto e1 = expression();
		Expression[] es = [e1];
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
		if (es.length > 1) return new TupleExpression(loc_tmp, es);
		else return e1;
	}

	IfElseExpression ifElseExpression() {
		auto loc_tmp = loc;
		nextToken();	// get rid of if
		auto cond = expression();
		check(TokenType.colon);
		auto if_block = expression();
		if (token.type == TokenType.else_) {
			nextToken();	// get rid of else
			auto else_block = expression();
			return new IfElseExpression(loc_tmp, cond, if_block, else_block);
		}
		else {
			return new IfElseExpression(loc_tmp, cond, if_block, new UnitExpression(loc));
		}
	}

	BlockExpression blockExpression() {
		auto loc_tmp = loc;
		nextToken();	// get rid of {
		ASTNode[] ns;
		with (TokenType)
		do {
			// label
			if (token.type == identifier && lex.lookahead.type == colon) {
				ns ~= new LabelDeclaration(loc, token.str);
				nextToken();	// get rid of identifier
				nextToken();	// get rid of :
				continue;
			}
			// expression
			if (isFirstOfExpression()) {
				ns ~= expression();
				auto is_semicolon = false;
				if (token.type == semicolon) {
					nextToken();	// get rid of ;
					is_semicolon = true;
				}
				if (token.type == rBrace) {
					if (is_semicolon) ns ~= new UnitExpression(loc);
					nextToken();	// get rid of }
					break;
				}
			}
			// declaration
			else with (Attribute) {
				auto attr = attribute(
					immut | const_ | inout_ | private_ | protected_ | package_ | public_ | export_
				  | final_ | static_ | pure_ | safe | trusted | system | throwable
				);
				with (TokenType)
				switch (token.type) {
				case let:
					ns ~= letDeclaration(attr);
					break;
				case func:
					ns ~= functionDeclaration(attr);
					break;
				default:
					error("An expression or declarations expected in a block expression, not " ~ token.str);
					nextToken();
					break;
				}
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
		if (ns.length == 0) ns = [new UnitExpression(loc_tmp)];
		return new BlockExpression(loc_tmp, ns);
	}

	/* *************************************** *
					 Statement
	 * *************************************** */
	WhileStatement whileStatement() {
		auto loc_tmp = loc;
		nextToken();	// get rid of while
		auto cond = expression();
		check(TokenType.colon);
		auto body = expression();
		return new WhileStatement(loc_tmp, cond, body);
	}

	DoWhileStatement doWhileStatement() {
		auto loc_tmp = loc;
		nextToken();	// get rid of do
		auto body = expression();
		check(TokenType.while_);
		auto cond = expression();
		check(TokenType.semicolon);
		return new DoWhileStatement(loc_tmp, body, cond);
	}

	ForStatement forStatement() {
		auto loc_tmp = loc;
		nextToken();	// get rid of for
		auto init = expression();
		check(TokenType.semicolon);
		auto cond = expression();
		check(TokenType.semicolon);
		auto exec = expression();
		check(TokenType.colon);
		auto body = expression();
		return new ForStatement(loc_tmp, init, cond, exec, body);
	}

	ForeachStatement foreachStatement() {
		//auto loc_tmp = loc;
		//nextToken();	// get rid of foreach
		return null;
	}

	ForeachReverseStatement foreachReverseStatement() {
		//auto loc_tmp = loc;
		//nextToken();	// get rid of foreach
		return null;
	}

	BreakStatement breakStatement() {
		auto loc_tmp = loc;
		nextToken();	// get rid of break
		Identifier label;
		if (token.type == TokenType.identifier) {
			label = new Identifier(token.str);
			nextToken();	// get rid of label
		}
		check(TokenType.semicolon);
		return new BreakStatement(loc_tmp, label);
	}

	ContinueStatement continueStatement() {
		auto loc_tmp = loc;
		nextToken();	// get rid of continue
		Identifier label;
		if (token.type == TokenType.identifier) {
			label = new Identifier(token.str);
			nextToken();	// get rid of label
		}
		check(TokenType.semicolon);
		return new ContinueStatement(loc_tmp, label);
	}

	ReturnStatement returnStatement() {
		auto loc_tmp = loc;
		nextToken();	// get rid of continue
		Expression expr;
		if (isFirstOfExpression()) {
			expr = expression();
		}
		check(TokenType.semicolon);
		return new ReturnStatement(loc_tmp, expr);
	}

	/* *************************************** *
					   Type
	 * *************************************** */
	Type attributedType(Attribute allowed) {
		auto attr = attribute(allowed);
		auto result = type();
		if (result) result.attr &= attr;
		return result;
	}

	Type type() {
		auto attr = attribute(Attribute.immut & Attribute.const_ & Attribute.inout_);
		auto result = functionType();
		if (result) result.attr &= attr;
		return result;
	}

	Type functionType() {
		auto t = arrayType();
		if (token.type == TokenType.right_arrow) {
			auto loc_tmp = loc;
			nextToken();	// get rid of ->
			auto t2 = functionType();
			t = new FunctionType(loc_tmp, t, t2);
		}
		return t;
	}

	// array, associative array, pointer
	Type arrayType() {
		with (TokenType)
		if      (token.type == lBrack) {
			Type t;
			auto loc_tmp = loc;
			nextToken();	// get rid of [
			auto k = type();
			// associative array
			if (token.type == colon) {
				nextToken();	// get rid of :
				auto v = type();
				t = new AssocArrayType(loc_tmp, k, v);
			}
			// array
			else {
				t = new ArrayType(loc_tmp, k);
			}
			check(TokenType.rBrack);
			return t;
		}
		else if (token.type == ref_of) {
			auto loc_tmp = loc;
			nextToken();	// get rid of #
			auto t2 = dotType();
			return new PointerType(loc_tmp, t2);
		}
		else return dotType();
	}

	// array, associative array, pointer
	Type dotType() {
		auto t = atomType();

		with (TokenType)
		while (token.type == dot) {
			auto loc_tmp = loc;
			nextToken();	// get rid of .
			auto t2 = atomType();
			t = new DotType(loc_tmp, t, t2);
		}

		return t;
	}

	Type atomType() {
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

	Type tupleType() {
		auto loc_tmp = loc;
		nextToken();	// get rid of (

		// ()
		if (token.type == TokenType.rPar) {
			nextToken();	// get rid of )
			error("() is the literal of the type 'unit', not the type itself.");
			return new PrimitiveType(loc_tmp, TokenType.unit);
		}

		auto t1 = type();
		Type[] ts = [t1];
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
		if (ts.length > 1) return new TupleType(loc_tmp, ts);
		else return t1;
	}

	PrimitiveType primitiveType() {
		auto t = new PrimitiveType(loc, token.type);
		nextToken();
		return t;
	}

	IdentifierType identifierType() {
		auto t = new IdentifierType(loc, new Identifier(token.str));
		nextToken();
		return t;
	}

	/* *************************************** *
					 Declaration
	 * *************************************** */
	LetDeclaration letDeclaration(Attribute attr) {
		auto loc_tmp = loc;
		nextToken();	// get rid of let
		Type whole_type;
		// let:int a = 0, ...;
		if (token.type == TokenType.colon) {
			nextToken();	// get rid of :
			whole_type = type();
		}

		TypedIdentifier[] tids;
		Expression[] exprs;

		with (TokenType)
		while (token.type == identifier) {
			auto name = token.str;
			nextToken();	// get rid of id
			Type ind_type;
			Expression expr;
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

			tids ~= new TypedIdentifier(name, attr, ind_type ? ind_type : whole_type);
			if (tids[$-1].type) tids[$-1].type.attr &= (attr & Attribute.type_qualifier);
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

		return new LetDeclaration(loc_tmp, tids, exprs);
	}

	FunctionDeclaration functionDeclaration(Attribute attr) {
		auto loc_tmp = loc;
		nextToken();	// get rid of func

		auto name = token.str;
		check(TokenType.identifier);

		Type ret_type;
		if (token.type == TokenType.colon) {
			nextToken();	// get rid of :
			ret_type = attributedType(Attribute.ref_);
		}

		auto id = new TypedIdentifier(name, Attribute.none, ret_type);

		// arguments
		TypedIdentifier[] args;
		with (TokenType)
		while (token.type.among!(identifier, any)) {
			auto arg_name = token.str;
			nextToken();	// get rid of id
			Type arg_type;
			if (token.type == TokenType.colon) {
				nextToken();	// get rid of :
				arg_type = attributedType(Attribute.ref_ | Attribute.lazy_);
			}
			args ~= new TypedIdentifier(arg_name, Attribute.none, arg_type);
		}

		Expression body;
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

		return new FunctionDeclaration(loc_tmp, attr, id, args, body);
	}

	/* ********************************** *
					 Tools
	 * ********************************** */
	Attribute attribute(Attribute allowed = Attribute.all) {
		Attribute result;
		while (true) {
			if (token.type == TokenType.composition)
				nextToken();	// get rid of @

			auto attr = getAttribute(token);

			// not an attribute allowed
			if (!(attr & allowed)) {
				break;
			}
			else {
				// private, protected, package, public, export
				if (result & Attribute.access_level && attr & Attribute.access_level) {
					error("Multiple access level (private/protected/package/public/export) collision : " ~ token.str);
				}
				// @safe @trusted @system
				else if (result & Attribute.function_safety && attr & Attribute.function_safety) {
					error("Multiple function safety (@safe/@trusted/@system) collision : " ~ token.str);
				}
				else result |= attr;
			}
			nextToken();
		}
		return result;
	}
}
+/
