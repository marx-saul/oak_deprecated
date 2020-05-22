module expression;
import comp_d;
import std.typecons;

unittest {
	showSLRtableInfo(grammar.grammar_info);
}
pragma(msg, "expression.d Calculating LR table...");

/* Definition of grammar */
// FunctionArgumentDeclarations, Arguments are given by other module
alias grammar = defineGrammar!(`
	Expression:
		AssignExpression
	;
	AssignExpression:
		@assign
			LambdaExpression assign     AssignExpression,
		@add_assign
			LambdaExpression add_assign AssignExpression,
		@sub_assign
			LambdaExpression sub_assign AssignExpression,
		@mul_assign
			LambdaExpression mul_assign AssignExpression,
		@div_assign
			LambdaExpression div_assign AssignExpression,
		@mod_assign
			LambdaExpression mod_assign AssignExpression,
		@cat_assign
			LambdaExpression cat_assign AssignExpression,
			
			LambdaExpression
	;
	LambdaExpression:
		@lambda
			lambda FunctionArgumentDeclarations right_arrow PipelineExpression,
			
			PipelineExpression
	;
	PipelineExpression:
		@pipeline
			Expression pipeline PipelineExpression,
		
			WhenExpression
	;
	WhenExpression:
		@when
			Expression when SharpExpression colon WhenExpression,
		
			SharpExpression
	;
	SharpExpression:
		@sharp
			OrExpression sharp SharpExpression,
			
			OrExpression
	;
	OrExpression:
		@or
			OrExpression  or  AndExpression,
		
			AndExpression
	;
	AndExpression:
		@and
			AndExpression and EqualExpression,
		
			EqualExpression
	;
	EqualExpression:
		@eq
			EqualExpression eq  AddExpression,
		@neq
			EqualExpression neq AddExpression,
		@ls
			EqualExpression ls  AddExpression,
		@leq
			EqualExpression leq AddExpression,
		@gt
			EqualExpression gt  AddExpression,
		@geq
			EqualExpression geq AddExpression,
			
			AddExpression
	;
	AddExpression:
		@add
			AddExpression add MulExpression,
		@sub
			AddExpression sub MulExpression,
		@cat
			AddExpression cat MulExpression,
		
			MulExpression
	;
	MulExpression:
		@mul
			MulExpression mul UnaryExpression,
		@div
			MulExpression div UnaryExpression,
		@mod
			MulExpression mod UnaryExpression,
		
			UnaryExpression
	;
	UnaryExpression:
		@unary_add
			add UnaryExpression,
		@unary_sub
			sub UnaryExpression,
		@unary_not
			not UnaryExpression,
		@unary_inc
			inc UnaryExpression,
		@unary_dec
			dec UnaryExpression,
		
			PowExpression
	;
	PowExpression:
		@pow
			ApplyExpression pow PowExpression,
		
			ApplyExpression
	;
	ApplyExpression:
		@apply
			ApplyExpression CompositeExpression,
		
			CompositeExpression
	;
	CompositeExpression:
		@composite
			CompositeExpression dot PostfixExpression,
		
			PostfixExpression
	;
	PostfixExpression:
		@index
			PostfixExpression lBrack ArgumentList rBrack,
		@post_inc
			PostfixExpression inc,
		@post_dec
			PostfixExpression dec,
			AtomExpression
	;
	AtomExpression:
		identifier,
		integer,
		real_number,
		string_literal,
		true,
		false,
		@parenthesis lPar Expression rPar,
		dollar,
	;
`);

static const table_info = SLRtableInfo(grammar.grammar_info);
static immutable table  = table_data(grammar.grammar_info, table_info);
/+
static const token_type_dictionary = new AATree!(TokenType, (a,b) => a<b, Symbol)(
	tuple(TokenType.identifier, 	grammar.numberOf("identifier")),
	tuple(TokenType.integer, 		grammar.numberOf("integer")),
	tuple(TokenType.real_number, 	grammar.numberOf("real_number")),
	tuple(TokenType.character, 		grammar.numberOf("character")),
	tuple(TokenType.string_literal, grammar.numberOf("string_literal")),
	tuple(TokenType.true_, 			grammar.numberOf("true")),
	tuple(TokenType.false_, 		grammar.numberOf("false")),
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
	tuple(TokenType.lPar, 			grammar.numberOf("lPar")),
	tuple(TokenType.rPar, 			grammar.numberOf("rPar")),
	tuple(TokenType.lBrack,			grammar.numberOf("lBrack")),
	tuple(TokenType.rBrack, 		grammar.numberOf("rBrack")),
	//tuple(TokenType.lBrace, 		grammar.numberOf("lBrace")),
	//tuple(TokenType.rBrace, 		grammar.numberOf("rBrace")),
	
	tuple(TokenType.end_of_file, 	comp_d.end_of_file_),
	
);
+/
