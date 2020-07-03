module parser.attribute;

import parser.parse: check;
import parser.lexer, ast.attribute, message;

Attribute attribute(L)(L lex, Attribute allowed = Attribute.all) {
	Attribute result;
	while (true) {
		if (lex.token.type == TokenType.composition)
			lex.nextToken(); // get rid of @

		auto attr = getAttribute(lex.token);

		// not an attribute allowed
		if (!(attr & allowed)) {
			break;
		}
		else {
			// private, protected, package, public, export
			if (result & Attribute.access_level && attr & Attribute.access_level) {
				error("Multiple access level (private/protected/package/public/export) collision : " ~ lex.token.str);
			}
			// @safe @trusted @system
			else if (result & Attribute.function_safety && attr & Attribute.function_safety) {
				error("Multiple function safety (@safe/@trusted/@system) collision : " ~ lex.token.str);
			}
			else result |= attr;
		}
		lex.nextToken();
	}
	return result;
}
