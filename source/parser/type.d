module parser.type;

import parser.parse: check;
import parser.lexer, parser.attribute, ast.all, message;

Type attributedType(L)(L lex, Attribute allowed) {
	auto attr = attribute(lex, allowed);
	auto result = type(lex);
	if (result) result.attr &= attr;
	return result;
}

Type type(L)(L lex) {
	auto attr = attribute(lex, Attribute.immut & Attribute.const_ & Attribute.inout_);
	auto result = functionType(lex);
	if (result) result.attr &= attr;
	return result;
}

Type functionType(L)(L lex) {
	auto t = arrayType(lex);
	if (lex.token.type == TokenType.right_arrow) {
		auto loc_tmp = lex.loc;
		lex.nextToken(); // get rid of ->
		auto t2 = functionType(lex);
		t = new FunctionType(loc_tmp, t, t2);
	}
	return t;
}

// array, associative array, pointer
Type arrayType(L)(L lex) {
	with(TokenType)
	if (lex.token.type == lBrack) {
		Type t;
		auto loc_tmp = lex.loc;
		lex.nextToken(); // get rid of [
		auto k = type(lex);
		// associative array
		if (lex.token.type == colon) {
			lex.nextToken(); // get rid of :
			auto v = type(lex);
			t = new AssocArrayType(loc_tmp, k, v);
		}
		// array
		else {
			t = new ArrayType(loc_tmp, k);
		}
		lex.check(TokenType.rBrack);
		return t;
	}
	else if (lex.token.type == ref_of) {
		auto loc_tmp = lex.loc;
		lex.nextToken(); // get rid of #
		auto t2 = dotType(lex);
		return new PointerType(loc_tmp, t2);
	}
	else return dotType(lex);
}

// array, associative array, pointer
Type dotType(L)(L lex) {
	auto t = atomType(lex);

	with(TokenType)
	while (lex.token.type == dot) {
		auto loc_tmp = lex.loc;
		lex.nextToken(); // get rid of .
		auto t2 = atomType(lex);
		t = new DotType(loc_tmp, t, t2);
	}

	return t;
}

Type atomType(L)(L lex) {
	import std.algorithm: among;
	with(TokenType)
	if (lex.token.type == lPar) {
		return tupleType(lex);
	}
	else if (lex.token.type.among!(int_, real_, string_, bool_, void_, unit)) {
		return primitiveType(lex);
	}
	else if (lex.token.type == identifier) {
		return identifierType(lex);
	}
	else {
		error("Type expected. not " ~ lex.token.str);
		lex.nextToken();
		return null;
	}
}

Type tupleType(L)(L lex) {
	auto loc_tmp = lex.loc;
	lex.nextToken(); // get rid of (

	// ()
	if (lex.token.type == TokenType.rPar) {
		lex.nextToken(); // get rid of )
		error("() is the literal of the type 'unit', not the type itself.");
		return new PrimitiveType(loc_tmp, TokenType.unit);
	}

	auto t1 = type(lex);
	Type[] ts = [t1];
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
			error("EOF found in a tuple type.");
			break;
		}
		ts ~= type(lex);
	}
	if (ts.length > 1) return new TupleType(loc_tmp, ts);
	else return t1;
}

PrimitiveType primitiveType(L)(L lex) {
	auto t = new PrimitiveType(lex.loc, lex.token.type);
	lex.nextToken();
	return t;
}

IdentifierType identifierType(L)(L lex) {
	auto t = new IdentifierType(lex.loc, new Symbol.Identifier(lex.token.str));
	lex.nextToken();
	return t;
}
