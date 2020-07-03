module visitor.visitor;

import ast.all;

// AST visitor, forces to implement visit for all AST types.
abstract class Visitor {
	abstract void visit(ASTNode)                 { assert(0); }
	// Expressions
	abstract void visit(Expression)              { assert(0); }
	abstract void visit(BinaryExpression)        { assert(0); }
	abstract void visit(UnaryExpression)         { assert(0); }
	abstract void visit(WhenExpression)          { assert(0); }
	abstract void visit(TupleExpression)         { assert(0); }
	abstract void visit(IndexExpression)         { assert(0); }
	abstract void visit(SliceExpression)         { assert(0); }
	abstract void visit(IntegerExpression)       { assert(0); }
	abstract void visit(RealExpression)          { assert(0); }
	abstract void visit(StringExpression)        { assert(0); }
	abstract void visit(IdentifierExpression)    { assert(0); }
	abstract void visit(DollarExpression)        { assert(0); }
	abstract void visit(ThisExpression)          { assert(0); }
	abstract void visit(SuperExpression)         { assert(0); }
	abstract void visit(UnitExpression)          { assert(0); }
	abstract void visit(ArrayExpression)         { assert(0); }
	abstract void visit(AssocArrayExpression)    { assert(0); }
	abstract void visit(StructExpression)        { assert(0); }
	abstract void visit(IfElseExpression)        { assert(0); }
	abstract void visit(BlockExpression)         { assert(0); }
	// Statements
	abstract void visit(Statement)               { assert(0); }
	abstract void visit(WhileStatement)          { assert(0); }
	abstract void visit(DoWhileStatement)        { assert(0); }
	abstract void visit(ForStatement)            { assert(0); }
	abstract void visit(ForeachStatement)        { assert(0); }
	abstract void visit(ForeachReverseStatement) { assert(0); }
	abstract void visit(BreakStatement)          { assert(0); }
	abstract void visit(ContinueStatement)       { assert(0); }
	abstract void visit(ReturnStatement)         { assert(0); }
	// Types
	abstract void visit(Type)                    { assert(0); }
	abstract void visit(FunctionType)            { assert(0); }
	abstract void visit(ArrayType)               { assert(0); }
	abstract void visit(AssocArrayType)          { assert(0); }
	abstract void visit(PointerType)             { assert(0); }
	abstract void visit(TupleType)               { assert(0); }
	abstract void visit(IdentifierType)          { assert(0); }
	abstract void visit(PrimitiveType)           { assert(0); }
	// Declaration
	abstract void visit(LetDeclaration)          { assert(0); }
	abstract void visit(FunctionDeclaration)     { assert(0); }
	abstract void visit(LabelDeclaration)        { assert(0); }
	//abstract void visit()
}

abstract class PermissiveVisitor : Visitor {
	override void visit(ASTNode)                 { }
	// Expressions
	override void visit(Expression)              { }
	override void visit(BinaryExpression)        { }
	override void visit(UnaryExpression)         { }
	override void visit(WhenExpression)          { }
	override void visit(TupleExpression)         { }
	override void visit(IndexExpression)         { }
	override void visit(SliceExpression)         { }
	override void visit(IntegerExpression)       { }
	override void visit(RealExpression)          { }
	override void visit(StringExpression)        { }
	override void visit(IdentifierExpression)    { }
	override void visit(DollarExpression)        { }
	override void visit(ThisExpression)          { }
	override void visit(SuperExpression)         { }
	override void visit(UnitExpression)          { }
	override void visit(ArrayExpression)         { }
	override void visit(AssocArrayExpression)    { }
	override void visit(StructExpression)        { }
	override void visit(IfElseExpression)        { }
	override void visit(BlockExpression)         { }
	// Statements
	override void visit(Statement)               { }
	override void visit(WhileStatement)          { }
	override void visit(DoWhileStatement)        { }
	override void visit(ForStatement)            { }
	override void visit(ForeachStatement)        { }
	override void visit(ForeachReverseStatement) { }
	override void visit(BreakStatement)          { }
	override void visit(ContinueStatement)       { }
	override void visit(ReturnStatement)         { }
	// Types
	override void visit(Type)                    { }
	override void visit(FunctionType)            { }
	override void visit(ArrayType)               { }
	override void visit(AssocArrayType)          { }
	override void visit(PointerType)             { }
	override void visit(TupleType)               { }
	override void visit(IdentifierType)          { }
	override void visit(PrimitiveType)           { }
	// Declaration
	override void visit(LetDeclaration)          { }
	override void visit(FunctionDeclaration)     { }
	override void visit(LabelDeclaration)        { }
}
