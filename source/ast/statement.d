module ast.statement;

import parser.lexer, ast.all, visitor.visitor;

// Statement is an expression that will not terminate or will always evaluated to ().
enum Stmt {
	error,

	do_,
	while_,
	for_,
	foreach_,
	foreach_reverse_,
	break_,
	continue_,
	return_,
}

abstract class Statement: Expression {
	Stmt stmt;
	this(Location l, Stmt s) {
		super(loc);
		stmt = s;
	}

	override void accept(Visitor v) {
		v.visit(this);
	}
}

final class WhileStatement: Statement {
	Expression cond;
	Expression body;
	this(Location l, Expression c, Expression b) {
		super(l, Stmt.while_);
		cond = c;
		body = b;
	}

	override @property WhileStatement dup() {
		return new WhileStatement(loc, cond, body);
	}
	override @property WhileStatement copy() {
		return new WhileStatement(loc, cond, body.copy);
	}

	override void accept(Visitor v) {
		v.visit(this);
	}
}

final class DoWhileStatement: Statement {
	Expression body;
	Expression cond;
	this(Location l, Expression b, Expression c) {
		super(l, Stmt.do_);
		body = b;
		cond = c;
	}

	override @property DoWhileStatement dup() {
		return new DoWhileStatement(loc, body, cond);
	}
	override @property DoWhileStatement copy() {
		return new DoWhileStatement(loc, body.copy, cond.copy);
	}

	override void accept(Visitor v) {
		v.visit(this);
	}
}

final class ForStatement: Statement {
	Expression init;
	Expression cond;
	Expression exec;
	Expression body;
	this(Location l, Expression i, Expression c, Expression e, Expression b) {
		super(l, Stmt.for_);
		init = i;
		cond = c;
		exec = e;
		body = b;
	}

	override @property ForStatement dup() {
		return new ForStatement(loc, init, cond, exec, body);
	}
	override @property ForStatement copy() {
		return new ForStatement(loc, init.copy, cond.copy, exec.copy, body.copy);
	}

	override void accept(Visitor v) {
		v.visit(this);
	}
}

final class ForeachStatement: Statement {
	Symbol.Variable[] t_ids;
	Expression expr;
	Expression body;
	this(Location l, Symbol.Variable[] t, Expression e, Expression b) {
		super(l, Stmt.foreach_);
		t_ids = t;
		expr = e;
		body = b;
	}

	override @property ForeachStatement dup() {
		return new ForeachStatement(loc, t_ids, expr, body);
	}
	override @property ForeachStatement copy() {
		return new ForeachStatement(loc, t_ids, expr.copy, body.copy);
	}

	override void accept(Visitor v) {
		v.visit(this);
	}
}

final class ForeachReverseStatement: Statement {
	Symbol.Variable[] t_ids;
	Expression expr;
	Expression body;
	this(Location l, Symbol.Variable[] t, Expression e, Expression b) {
		super(l, Stmt.foreach_reverse_);
		t_ids = t;
		expr = e;
		body = b;
	}

	override @property ForeachReverseStatement dup() {
		return new ForeachReverseStatement(loc, t_ids, expr, body);
	}
	override @property ForeachReverseStatement copy() {
		return new ForeachReverseStatement(loc, t_ids, expr.copy, body.copy);
	}

	override void accept(Visitor v) {
		v.visit(this);
	}
}

final class BreakStatement: Statement {
	Symbol.Label label;
	this(Location l, Symbol.Label lbl) {
		super(l, Stmt.break_);
		label = lbl;
	}

	override @property BreakStatement dup() {
		return new BreakStatement(loc, label);
	}
	override @property BreakStatement copy() {
		return new BreakStatement(loc, label);
	}

	override void accept(Visitor v) {
		v.visit(this);
	}
}

final class ContinueStatement: Statement {
	Symbol.Label label;
	this(Location l, Symbol.Label lbl) {
		super(l, Stmt.continue_);
		label = lbl;
	}

	override @property ContinueStatement dup() {
		return new ContinueStatement(loc, label);
	}
	override @property ContinueStatement copy() {
		return new ContinueStatement(loc, label);
	}

	override void accept(Visitor v) {
		v.visit(this);
	}
}

final class ReturnStatement: Statement {
	Expression expr;
	this(Location l, Expression e) {
		super(l, Stmt.return_);
		expr = e;
	}

	override @property ReturnStatement dup() {
		return new ReturnStatement(loc, expr);
	}
	override @property ReturnStatement copy() {
		return new ReturnStatement(loc, expr.copy);
	}

	override void accept(Visitor v) {
		v.visit(this);
	}
}
