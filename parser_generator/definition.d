module definition;
import comp_d;
import std.typecons;

unittest {
}

/* Definition of grammar */
alias grammar = defineGrammar!(`
	Statement:
		IfStatement,
		Expression,
	;
	IfStatement:
		if Expression colon Statement else Statement,
		if Expression colon Statement
	;
`);/*
	ArgumentList:
		@ArgumentListContinue
			ArgumentList comma Expression,
		@ArgumentListEnd
			Experssion
	;
	
	LetDeclaration:
		@let_decl
			let DeclarationBodies semicolon
	;
	VarDeclaration:
		@var_decl
			var DeclarationBodies semicolon
	;
	DeclarationBody:
		@decl_body_type_expr
			identifier lPar Type rPar assign Expression,
		@decl_body_expr
			identifier                assign Expression,
		@decl_body_type
			identifier lPar Type rPar,
	;
	DeclarationBodies:
		@decl_body_end
			DeclarationBody,
		@decl_body_continue
			DeclarationBodies comma DeclarationBody
	;
	
	
	Type:
		AtomType,
		Type lBrack rBrack,
	;
	
	AtomType:
		int,
		real,
		string,
		bool,
	;
	
	
	DefDeclaration:
		def  identifier lPar Type rPar FunctionArgumentDeclarations assign Expression,
		def  identifier                FunctionArgumentDeclarations assign Expression,
	;
	FunctionArgumentDeclarations:
		empty,
		FunctionArgumentDeclarations FunctionArgumentDeclaration
	;
	FunctionArgumentDeclaration:
		identifier lPar Type rPar,
		identifier,
		any,
	;
	
	
`);*/

/* LR table */
static const table_info = SLRtableInfo(grammar.grammar_info);
/+
/* connect lexer and parser */
static const token_type_dictionary = new AATree!(TokenType, (a,b) => a<b, Symbol)(
	/+tuple(TokenType.identifier, 	grammar.numberOf("identifier")),
	tuple(TokenType.integer, 		grammar.numberOf("integer")),
	tuple(TokenType.real_number, 	grammar.numberOf("real_number")),
	tuple(TokenType.character, 		grammar.numberOf("character")),
	tuple(TokenType.string_literal, grammar.numberOf("string_literal")),
	tuple(TokenType.true_, 			grammar.numberOf("true")),
	tuple(TokenType.false_, 		grammar.numberOf("false")),
	tuple(TokenType.int_, 			grammar.numberOf("int")),
	tuple(TokenType.real_, 			grammar.numberOf("real")),
	tuple(TokenType.string_, 		grammar.numberOf("string")),
	tuple(TokenType.bool_, 			grammar.numberOf("bool")),
	tuple(TokenType.if_, 			grammar.numberOf("if")),
	tuple(TokenType.else_, 			grammar.numberOf("else")),
	tuple(TokenType.when, 			grammar.numberOf("when")),
	tuple(TokenType.let, 			grammar.numberOf("let")),
	tuple(TokenType.var, 			grammar.numberOf("var")),
	tuple(TokenType.def, 			grammar.numberOf("def")),
	tuple(TokenType.any, 			grammar.numberOf("any")),
	tuple(TokenType.add, 			grammar.numberOf("add")),
	tuple(TokenType.sub, 			grammar.numberOf("sub")),
	tuple(TokenType.mul, 			grammar.numberOf("mul")),
	tuple(TokenType.div, 			grammar.numberOf("div")),
	tuple(TokenType.mod, 			grammar.numberOf("mod")),
	tuple(TokenType.pow, 			grammar.numberOf("pow")),
	tuple(TokenType.cat, 			grammar.numberOf("cat")),
	tuple(TokenType.and, 			grammar.numberOf("and")),
	tuple(TokenType.or, 			grammar.numberOf("or")),
	tuple(TokenType.not, 			grammar.numberOf("not")),
	tuple(TokenType.eq, 			grammar.numberOf("eq")),
	tuple(TokenType.neq, 			grammar.numberOf("neq")),
	tuple(TokenType.ls, 			grammar.numberOf("ls")),
	tuple(TokenType.gt, 			grammar.numberOf("gt")),
	tuple(TokenType.leq, 			grammar.numberOf("leq")),
	tuple(TokenType.geq, 			grammar.numberOf("geq")),
	tuple(TokenType.composition, 	grammar.numberOf("composition")),
	//tuple(TokenType.dots2, 			grammar.numberOf("dots2")),
	tuple(TokenType.right_arrow, 	grammar.numberOf("right_arrow")),
	tuple(TokenType.dollar, 		grammar.numberOf("dollar")),
	tuple(TokenType.sharp, 			grammar.numberOf("sharp")),
	tuple(TokenType.pipeline, 		grammar.numberOf("pipeline")),
	tuple(TokenType.assign, 		grammar.numberOf("assign")),
	tuple(TokenType.add_assign, 	grammar.numberOf("add_assign")),
	tuple(TokenType.sub_assign, 	grammar.numberOf("sub_assign")),
	tuple(TokenType.mul_assign, 	grammar.numberOf("mul_assign")),
	tuple(TokenType.div_assign, 	grammar.numberOf("div_assign")),
	tuple(TokenType.mod_assign, 	grammar.numberOf("mod_assign")),
	tuple(TokenType.cat_assign, 	grammar.numberOf("cat_assign")),
	tuple(TokenType.colon, 			grammar.numberOf("colon")),
	tuple(TokenType.semicolon, 		grammar.numberOf("semicolon")),
	tuple(TokenType.comma, 			grammar.numberOf("comma")),
	tuple(TokenType.lPar, 			grammar.numberOf("lPar")),
	tuple(TokenType.rPar, 			grammar.numberOf("rPar")),
	tuple(TokenType.lBrack,			grammar.numberOf("lBrack")),
	tuple(TokenType.rBrack, 		grammar.numberOf("rBrack")),
	//tuple(TokenType.lBrace, 		grammar.numberOf("lBrace")),
	//tuple(TokenType.rBrace, 		grammar.numberOf("rBrace")),+/
	
	tuple(TokenType.if_, 			grammar.numberOf("if")),
	tuple(TokenType.else_, 			grammar.numberOf("else")),
	tuple(TokenType.colon, 			grammar.numberOf("colon")),
	
	tuple(TokenType.end_of_file, 	comp_d.end_of_file_),
	
);
+/
