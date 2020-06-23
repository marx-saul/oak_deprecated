module visitor.visitor;

import ast.astbase: ASTBase;

// visiting the AST
abstract class Visitor(AST) {
	abstract void visit(AST.ASTNode)              { assert(0); }
	abstract void visit(AST.Expression)           { assert(0); }
	abstract void visit(AST.BinaryExpression)     { assert(0); }
	abstract void visit(AST.UnaryExpression)      { assert(0); }
	abstract void visit(AST.WhenExpression)       { assert(0); }
	abstract void visit(AST.TupleExpression)      { assert(0); }
	abstract void visit(AST.IndexExpression)      { assert(0); }
	abstract void visit(AST.SliceExpression)      { assert(0); }
	abstract void visit(AST.IntegerExpression)    { assert(0); }
	abstract void visit(AST.RealExpression)       { assert(0); }
	abstract void visit(AST.StringExpression)     { assert(0); }
	abstract void visit(AST.IdentifierExpression) { assert(0); }
	abstract void visit(AST.DollarExpression)     { assert(0); }
	abstract void visit(AST.ThisExpression)       { assert(0); }
	abstract void visit(AST.SuperExpression)      { assert(0); }
	//abstract void visit(AST.)
}
