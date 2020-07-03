module parser.statement;

import parser.parse: check;
import parser.lexer, parser.expression, parser.attribute, ast.all, message;

WhileStatement whileStatement(L)(L lex) {
	auto loc_tmp = lex.loc;
	lex.nextToken(); // get rid of while
	auto cond = expression(lex);
	lex.check(TokenType.colon);
	auto body = expression(lex);
	return new WhileStatement(loc_tmp, cond, body);
}

DoWhileStatement doWhileStatement(L)(L lex) {
	auto loc_tmp = lex.loc;
	lex.nextToken(); // get rid of do
	auto body = expression(lex);
	lex.check(TokenType.while_);
	auto cond = expression(lex);
	lex.check(TokenType.semicolon);
	return new DoWhileStatement(loc_tmp, body, cond);
}

ForStatement forStatement(L)(L lex) {
	auto loc_tmp = lex.loc;
	// for E ; E ; E : E
	lex.nextToken(); // get rid of for
	auto init = expression(lex);	// E
	lex.check(TokenType.semicolon);	// ;
	auto cond = expression(lex);	// E
	lex.check(TokenType.semicolon);	// ;
	auto exec = expression(lex);	// E
	lex.check(TokenType.colon);		// :
	auto body = expression(lex);	// E
	return new ForStatement(loc_tmp, init, cond, exec, body);
}

ForeachStatement foreachStatement(L)(L lex) {
	//auto loc_tmp = lex.loc;
	//lex.nextToken();	// get rid of foreach
	return null;
}

ForeachReverseStatement foreachReverseStatement(L)(L lex) {
	//auto loc_tmp = lex.loc;
	//lex.nextToken();	// get rid of foreach
	return null;
}

BreakStatement breakStatement(L)(L lex) {
	auto loc_tmp = lex.loc;
	lex.nextToken(); // get rid of break
	Symbol.Label label;
	if (lex.token.type == TokenType.identifier) {
		label = new Symbol.Label(lex.token.str);
		lex.nextToken(); // get rid of label
	}
	lex.check(TokenType.semicolon);
	return new BreakStatement(loc_tmp, label);
}

ContinueStatement continueStatement(L)(L lex) {
	auto loc_tmp = lex.loc;
	lex.nextToken(); // get rid of continue
	Symbol.Label label;
	if (lex.token.type == TokenType.identifier) {
		label = new Symbol.Label(lex.token.str);
		lex.nextToken(); // get rid of label
	}
	lex.check(TokenType.semicolon);
	return new ContinueStatement(loc_tmp, label);
}

ReturnStatement returnStatement(L)(L lex) {
	auto loc_tmp = lex.loc;
	lex.nextToken(); // get rid of continue
	Expression expr;
	if (isFirstOfExpression(lex.token.type)) {
		expr = expression(lex);
	}
	lex.check(TokenType.semicolon);
	return new ReturnStatement(loc_tmp, expr);
}

