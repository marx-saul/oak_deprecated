module visitor.visitor;

import ast.astbase: ASTBase;

// AST visitor, forces to implement visit for all AST types.
abstract class Visitor(AST) {
	abstract void visit(AST.ASTNode)                 { assert(0); }
	// Expressions
	abstract void visit(AST.Expression)              { assert(0); }
	abstract void visit(AST.BinaryExpression)        { assert(0); }
	abstract void visit(AST.UnaryExpression)         { assert(0); }
	abstract void visit(AST.WhenExpression)          { assert(0); }
	abstract void visit(AST.TupleExpression)         { assert(0); }
	abstract void visit(AST.IndexExpression)         { assert(0); }
	abstract void visit(AST.SliceExpression)         { assert(0); }
	abstract void visit(AST.IntegerExpression)       { assert(0); }
	abstract void visit(AST.RealExpression)          { assert(0); }
	abstract void visit(AST.StringExpression)        { assert(0); }
	abstract void visit(AST.IdentifierExpression)    { assert(0); }
	abstract void visit(AST.DollarExpression)        { assert(0); }
	abstract void visit(AST.ThisExpression)          { assert(0); }
	abstract void visit(AST.SuperExpression)         { assert(0); }
	abstract void visit(AST.UnitExpression)          { assert(0); }
	abstract void visit(AST.ArrayExpression)         { assert(0); }
	abstract void visit(AST.AssocArrayExpression)    { assert(0); }
	abstract void visit(AST.StructExpression)        { assert(0); }
	abstract void visit(AST.IfElseExpression)        { assert(0); }
	abstract void visit(AST.BlockExpression)         { assert(0); }
	// Statements
	abstract void visit(AST.Statement)               { assert(0); }
	abstract void visit(AST.WhileStatement)          { assert(0); }
	abstract void visit(AST.DoWhileStatement)        { assert(0); }
	abstract void visit(AST.ForStatement)            { assert(0); }
	abstract void visit(AST.ForeachStatement)        { assert(0); }
	abstract void visit(AST.ForeachReverseStatement) { assert(0); }
	abstract void visit(AST.BreakStatement)          { assert(0); }
	abstract void visit(AST.ContinueStatement)       { assert(0); }
	abstract void visit(AST.ReturnStatement)         { assert(0); }
	// Types
	abstract void visit(AST.Type)                    { assert(0); }
	abstract void visit(AST.FunctionType)            { assert(0); }
	abstract void visit(AST.ArrayType)               { assert(0); }
	abstract void visit(AST.AssocArrayType)          { assert(0); }
	abstract void visit(AST.PointerType)             { assert(0); }
	abstract void visit(AST.TupleType)               { assert(0); }
	abstract void visit(AST.IdentifierType)          { assert(0); }
	abstract void visit(AST.PrimitiveType)           { assert(0); }
	// Declaration
	abstract void visit(AST.LetDeclaration)          { assert(0); }
	abstract void visit(AST.FunctionDeclaration)     { assert(0); }
	//abstract void visit(AST.)
}

abstract class PermissiveVisitor(AST) : Visitor!AST {
	override void visit(AST.ASTNode)                 { }
	// Expressions
	override void visit(AST.Expression)              { }
	override void visit(AST.BinaryExpression)        { }
	override void visit(AST.UnaryExpression)         { }
	override void visit(AST.WhenExpression)          { }
	override void visit(AST.TupleExpression)         { }
	override void visit(AST.IndexExpression)         { }
	override void visit(AST.SliceExpression)         { }
	override void visit(AST.IntegerExpression)       { }
	override void visit(AST.RealExpression)          { }
	override void visit(AST.StringExpression)        { }
	override void visit(AST.IdentifierExpression)    { }
	override void visit(AST.DollarExpression)        { }
	override void visit(AST.ThisExpression)          { }
	override void visit(AST.SuperExpression)         { }
	override void visit(AST.UnitExpression)          { }
	override void visit(AST.ArrayExpression)         { }
	override void visit(AST.AssocArrayExpression)    { }
	override void visit(AST.StructExpression)        { }
	override void visit(AST.IfElseExpression)        { }
	override void visit(AST.BlockExpression)         { }
	// Statements
	override void visit(AST.Statement)               { }
	override void visit(AST.WhileStatement)          { }
	override void visit(AST.DoWhileStatement)        { }
	override void visit(AST.ForStatement)            { }
	override void visit(AST.ForeachStatement)        { }
	override void visit(AST.ForeachReverseStatement) { }
	override void visit(AST.BreakStatement)          { }
	override void visit(AST.ContinueStatement)       { }
	override void visit(AST.ReturnStatement)         { }
	// Types
	override void visit(AST.Type)                    { }
	override void visit(AST.FunctionType)            { }
	override void visit(AST.ArrayType)               { }
	override void visit(AST.AssocArrayType)          { }
	override void visit(AST.PointerType)             { }
	override void visit(AST.TupleType)               { }
	override void visit(AST.IdentifierType)          { }
	override void visit(AST.PrimitiveType)           { }
	// Declaration
	override void visit(AST.LetDeclaration)          { }
	override void visit(AST.FunctionDeclaration)     { }
}
