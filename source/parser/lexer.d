module parser.lexer;
import std.stdio, std.variant, std.range, std.ascii, std.typecons, std.algorithm, std.array;
import std.meta: aliasSeqOf;
import std.conv: to;
import aatree;

/+
unittest {
	string code = `
		a_2;: 234 {let when} ( ) def 0b111 0xF "abcd\n\\\tefgh"
		12.3456, 0x0.909 0b10.111;
		5..10
		a_32; // comment
		1 /* comment
		  */
		2  /*/*/
		3  /*/ 
		*/
		4 /*/* /*/
		
		5 /+/+ +/ +/
		
		6
		/+/ /+
		+/ /+ +/ +/
		
		7
		
		:;,+-*/%^^$+=-=*=/=%= <<=>>= .. ...||&&!!= 
		
		true false if else when let var def
	`;
	immutable(dchar)[] lookahead;
	ulong line_num = 1;
	
	while (true) {
		//writeln(code, " |||| lookahead : '", lookahead, "'");
		auto token = nextToken(code, lookahead, line_num);
		writeln("'", token.str, "'\t\t", token.type, "\t\t", token.int_val, "\t\t", token.real_val);
		if (token.type == TokenType.end_of_file) break;
	}
}
+/

// when adding a token, alter this enum TokenType, (reserved_words/reserved_symbols)
enum TokenType : uint {
	error, dummy,
	identifier,
	
	// literal
	integer, real_number, /*character,*/ string_literal,
	true_, false_,
	
	// type
	int_, real_, string_, bool_,
	
	// reserved words
	if_, else_, when, let, var, def, any, this_,
    
	// expression symbols
	add, sub, mul, div, mod,    // + - * / %
	//inc, dec,					// ++ --
	pow,						// ^^
	cat,						// ~ (concatation)
	and, or, not, 				// && || !
	eq, neq, ls, gt, leq, geq,	// ==, !=, <, >, <=, >=, 
	composition,				// @  (composition of functions) 
	dot, dotdot,				// . ..
	right_arrow, 				// -> (right_arrow)
	indexing,					// !!
	template_instance_type, 	// ?
	template_instance_expr,		// ??
	dollar,						// $
	sharp,						// #
	pipeline,					// |>
	lambda,						// \ 
	assign,						// =
	add_assign, sub_assign, mul_assign, div_assign, mod_assign, cat_assign, // += -= *= /= %= ~=
	
	// for parsers
	unary_op, //u_add, u_sub, u_not, u_inc, u_dec,
	empty_tuple,	// ()
	
	// other symbols
	colon, semicolon, comma,
	lPar, rPar,                 // ( )
	lBrack, rBrack,				// [ ]
	lBrace, rBrace,				// { }
	
	// dummy tokens
	app, postfix,
	
	// EOF
	end_of_file,
}

struct Token {
    TokenType type;
    string str;
    ulong line_num;
    ulong index_num;
    
    // literal value
    long int_val;
    double real_val;
    alias string_val = str;
    // will be implemented by this
    //Variant literal;
}

// These can be replaced by associative array. But currently dmd does not support compile-time associative array, we use this instead.
pure @nogc @safe bool string_dictionary_order(string a, string b) {
	auto n = a.length, m = b.length;
	foreach (i; 0 .. min(n,m)) {
		if (a[i] > b[i]) return false;
		if (a[i] < b[i]) return true;
	}
	
	return m < n;
}

template StringDict(T) {
	alias StringDict = AATree!(string, string_dictionary_order, T);
}
alias TokenDict = StringDict!(TokenType);

static const reserved_words = new TokenDict(
	tuple("true", TokenType.true_),
	tuple("false", TokenType.false_),
	tuple("int", TokenType.int_),
	tuple("real", TokenType.real_),
	tuple("string", TokenType.string_),
	tuple("bool", TokenType.bool_),
	tuple("if", TokenType.if_),
	tuple("else", TokenType.else_),
	tuple("let", TokenType.let),
	tuple("var", TokenType.var),
	tuple("def", TokenType.def),
	tuple("when", TokenType.when),
	tuple("any", TokenType.any),
	tuple("this", TokenType.this_),
);

//Tuple!( dchar, immutable(Tuple!(string, "str", TokenType, "type"))[] )
//Tuple!( dchar, immutable(Tuple!(string, "str", TokenType, "type")[]) )

alias SE = Tuple!(string, "str", TokenType, "type");
static const reserved_symbols = new AATree!(dchar, (a,b) => a<b, SE[])(
	tuple(cast(dchar) '+', [SE("+", TokenType.add), /*SE("++", TokenType.inc),*/ SE("+=", TokenType.add_assign)]),
	tuple(cast(dchar) '-', [SE("-", TokenType.sub), /*SE("--", TokenType.dec),*/ SE("-=", TokenType.sub_assign), SE("->", TokenType.right_arrow)]),
	tuple(cast(dchar) '*', [SE("*", TokenType.mul), SE("*=", TokenType.mul_assign)]),
	tuple(cast(dchar) '/', [SE("/", TokenType.div), SE("/=", TokenType.div_assign)]),
	tuple(cast(dchar) '%', [SE("%", TokenType.mod), SE("%=", TokenType.mod_assign)]),
	tuple(cast(dchar) '^', [SE("^^", TokenType.pow)]),
	tuple(cast(dchar) '~', [SE("~", TokenType.cat), SE("~=", TokenType.cat_assign)]),
	tuple(cast(dchar) '&', [SE("&&", TokenType.and)]),
	tuple(cast(dchar) '|', [SE("||", TokenType.or), SE("|>", TokenType.pipeline)]),
	tuple(cast(dchar) '!', [SE("!", TokenType.not), SE("!=", TokenType.neq), SE("!!", TokenType.indexing)]),
	tuple(cast(dchar) '?', [SE("?", TokenType.template_instance_type), SE("??", TokenType.template_instance_expr)]),
	tuple(cast(dchar) '$', [SE("$", TokenType.dollar)]),
	tuple(cast(dchar) '#', [SE("#", TokenType.sharp)]),
	tuple(cast(dchar) ',', [SE(",", TokenType.comma)]),
	tuple(cast(dchar) '\\', [SE("\\", TokenType.lambda)]),
	tuple(cast(dchar) '=', [SE("=", TokenType.assign), SE("==", TokenType.eq)]),
	tuple(cast(dchar) '<', [SE("<", TokenType.ls), SE("<=", TokenType.leq)]),
	tuple(cast(dchar) '>', [SE(">", TokenType.gt), SE(">=", TokenType.geq)]),
	tuple(cast(dchar) '.', [SE(".", TokenType.dot), SE("..", TokenType.dotdot)]),
	tuple(cast(dchar) '@', [SE("@", TokenType.composition)]),
	
	tuple(cast(dchar) ':', [SE(":", TokenType.colon)]),
	tuple(cast(dchar) ';', [SE(";", TokenType.semicolon)]),
	tuple(cast(dchar) '(', [SE("(", TokenType.lPar)]),
	tuple(cast(dchar) ')', [SE(")", TokenType.rPar)]),
	tuple(cast(dchar) '[', [SE("[", TokenType.lBrack)]),
	tuple(cast(dchar) ']', [SE("]", TokenType.rBrack)]),
	tuple(cast(dchar) '{', [SE("{", TokenType.lBrace)]),
	tuple(cast(dchar) '}', [SE("}", TokenType.rBrace)]),
);

static const escape_sequences = new AATree!(dchar, (a,b) => a<b, dchar) (
	tuple(cast(dchar) '\'',cast(dchar)'\''),
	tuple(cast(dchar) '"', cast(dchar) '"'),
	tuple(cast(dchar) '\\',cast(dchar)'\\'),
	tuple(cast(dchar) '0', cast(dchar) '\0'),
	tuple(cast(dchar) 'a', cast(dchar) '\a'),
	tuple(cast(dchar) 'b', cast(dchar) '\b'),
	tuple(cast(dchar) 'f', cast(dchar) '\f'),
	tuple(cast(dchar) 'n', cast(dchar) '\n'),
	tuple(cast(dchar) 'r', cast(dchar) '\r'),
	tuple(cast(dchar) 't', cast(dchar) '\t'),
	tuple(cast(dchar) 'v', cast(dchar) '\v'),
);

Token nextToken(Range)(ref Range input, ref immutable(dchar)[] lookahead, ref ulong line_num, ref ulong index_num)	// when characters were looked-ahead, they will be pushed on 'lookahead'
	if (isInputRange!Range && is(typeof(input.front) : immutable dchar))
{
	enum EOF = cast(dchar) -1;
	
	
	// wrapper for EOF and look-ahead
	dchar nextChar() {
		if (lookahead.length > 0) {
			auto c = lookahead[0];
			lookahead = lookahead[1 .. $];
			//debug(lexer) writeln("'", c, "'");
			if (c == '\n') ++line_num, index_num = 1;
			else ++index_num;
			return c;
		}
		else {
			if (input.empty)
				return EOF;
			auto c = input.front;
			if (c == '\n') ++line_num, index_num = 1;
			else ++index_num;
			input.popFront();
			//debug(lexer) writeln("'", c, "'");
			return c;
		}
	}
	// look the k-th character after the current character (input.front)
	// lookAhead(0) = input.front 
	dchar lookAhead(uint k = 1) {
		if (input.empty)
			return EOF;
		if (lookahead.length < k) {
			foreach (i; 0 .. k - lookahead.length) {
				if (input.empty) lookahead ~= EOF;
				else {
					auto c = input.front;
					lookahead ~= c;
					input.popFront();
				}
			}
			return lookahead[$-1];
		}
		else return lookahead[k-1];
	}
	void unget(immutable dchar c) {
		lookahead = [c] ~ lookahead;
	}
	
	
	// ignore spaces and comments
	dchar c = nextChar();
	while (true) {
		// spaces
		while (isWhite(c)) {
			c = nextChar();
			if (c == EOF) break;
		}
		
		// not spaces or '/'
		if (c != '/') break;
		
		auto la = lookAhead();
		// one line comment
		if (la == '/') {
			// get rid of all characters until '\n'.
			while (nextChar() != '\n') {}
			c = nextChar();
		}
		// /* comment */
		else if (la == '*') {
			nextChar();		// '*'
			c = nextChar();
			while (true) {
				if (c == '*' && lookAhead() == '/') {
					nextChar();		// '/'
					c = nextChar();
					break;
				}
				else if (c == EOF) break;			// '*/' not found
				c = nextChar();
			}
		}
		// /+ nested comment +/
		else if (la == '+') {
			uint nest_depth = 1;
			nextChar();		// '+'
			c = nextChar();
			while (nest_depth > 0) {
				if (c == '+' && lookAhead() == '/') {
					nextChar();		// '/'
					c = nextChar();
					--nest_depth;
				}
				else if (c == '/' && lookAhead() == '+') {
					nextChar();		// '+'
					c = nextChar();
					++nest_depth;
				}
				else if (c == EOF) break;			// '+/' not found
				c = nextChar();
			}
		}
		else break;
	}
	
	Token token;
	
	// identifier or reserved word
	if (isAlpha(c) || c == '_') {
		token.type = TokenType.identifier, token.line_num = line_num, token.index_num = index_num;
		while (isAlphaNum(c) || c == '_') {
			token.str ~= c;
			c = nextChar();
		}
		unget(c);
		
		/*auto ptr = token.str in reserved_words;
		if (ptr) token.type = *ptr;*/
		
		if (reserved_words.hasKey(token.str)) token.type = reserved_words[token.str];
	}
	
	// integer, real number
	else if (isDigit(c)) {
		token.type = TokenType.integer, token.line_num = line_num, token.index_num = index_num;
		
		// hexadecimal
		if (c == '0' && lookAhead().among!('x', 'X')) {
			auto c2 = nextChar();	// 'x'
			token.str = "0" ~ c2.to!string;
			c = nextChar();
			while (true) {
				if (isDigit(c)) {
					token.int_val *= 16;
					token.int_val += c - '0';
					token.str ~= c;
				}
				else if (c.among!(aliasSeqOf!"abcdef")) {
					token.int_val *= 16;
					token.int_val += c - 'a' + 10;
					token.str ~= c;
				}
				else if (c.among!(aliasSeqOf!"ABCDEF")) {
					token.int_val *= 16;
					token.int_val += c - 'A' + 10;
					token.str ~= c;
				}
				// real number
				else if (c == '.') {
					if (lookAhead() == '.') {
						unget('.');
						return token;
					}
					double exponent = 1.0;
					
					token.str ~= ".";
					token.type = TokenType.real_number;
					token.real_val = cast(double) token.int_val;
					
					c = nextChar();
					while (true) {
						exponent /= 16;
						if (isDigit(c)) {
							token.real_val += exponent * (c - '0');
							token.str ~= c;
						}
						else if (c.among!(aliasSeqOf!"abcdef")) {
							token.real_val += exponent * (c - 'a' + 10);
							token.str ~= c;
						}
						else if (c.among!(aliasSeqOf!"ABCDEF")) {
							token.real_val += exponent * (c - 'A' + 10);
							token.str ~= c;
						}
						else if (c == '_') { token.str ~= c; }
						else { unget(c); return token; }
						c = nextChar();
					}
				}
				else if (c == '_') { token.str ~= c; }
				else { unget(c); return token; }
				c = nextChar();
			}
		}
		// binary
		else if (c == '0' && lookAhead().among!('b', 'B')) {
			auto c2 = nextChar();	// 'b'
			token.str = "0" ~ c2.to!string;
			c = nextChar();
			while (true) {
				if (c.among!('0', '1')) {
					token.int_val *= 2;
					token.int_val += c - '0';
					token.str ~= c;
				}
				// real number
				else if (c == '.') {
					if (lookAhead() == '.') {
						unget('.');
						return token;
					}
					double exponent = 1.0;
					
					token.str ~= ".";
					token.type = TokenType.real_number;
					token.real_val = cast(double) token.int_val;
					
					c = nextChar();
					while (true) {
						exponent /= 2;
						if (c.among!('0', '1')) {
							token.real_val += exponent * (c - '0');
							token.str ~= c;
						}
						else if (c == '_') { token.str ~= c; }
						else { unget(c); return token; }
						c = nextChar();
					}
				}
				else if (c == '_') { token.str ~= c; }
				else { unget(c); return token; }
				c = nextChar();
			}
		}
		// 10
		else {
			while (true) {
				if (isDigit(c)) {
					token.int_val *= 10;
					token.int_val += c - '0';
					token.str ~= c;
				}
				// real number
				else if (c == '.') {
					if (lookAhead() == '.') {
						unget('.');
						return token;
					}
					double exponent = 1.0;
					
					token.str ~= ".";
					token.type = TokenType.real_number;
					token.real_val = cast(double) token.int_val;
					
					c = nextChar();
					while (true) {
						exponent /= 10;
						if (c.among!('0', '1')) {
							token.real_val += exponent * (c - '0');
							token.str ~= c;
						}
						else if (c == '_') { token.str ~= c; }
						else { unget(c); return token; }
						c = nextChar();
					}
				}
				else if (c == '_') { token.str ~= c; }
				else { unget(c); return token; }
				c = nextChar();
			}
		}
	}
	
	// string literal
	else if (c == '"') {
		token.type = TokenType.string_literal, token.line_num = line_num, token.index_num = index_num;
		while (true) {
			c = nextChar();
			if (c == '\\') {
				c = nextChar();
				if (escape_sequences.hasKey(c)) {
					token.str ~= escape_sequences[c];
				}
				// token error: false escape sequence
				else {
				}
			}
			else if (c == '"') { break; }
			// token error: no '"'
			else if (c == EOF) { break; /* error */ }
			else { token.str ~= c.to!string; }
		}
	}
	
	// symbols
	else if (reserved_symbols.hasKey(c)) {
		auto SElist = reserved_symbols[c].dup;
		
		string s = c.to!string;		// expected to be a head of some token
		while (true) {
			// tokens whose head is equal to `s`
			auto newSElist = SElist.filter!(se => se.str[0 .. min(s.length, $)] == s).array;
			
			// by adding one character no token matches.
			if (newSElist.length == 0) {
				token.type = SElist[0].type, token.str = s[0 .. $-1], token.line_num = line_num, token.index_num = index_num;
				unget(s[$-1]);
				return token;
			}
			SElist = newSElist;
			
			// found
			if (SElist.length == 1 && SElist[0].str.length == s.length /* equivalent to SElist[0].str == s */) {
				//writeln("\t length 1");
				token.type = SElist[0].type, token.str = s, token.line_num = line_num, token.index_num = index_num;
				return token;
			}
			// two or more possible tokens
			else {
				//writeln("\t length > 1");
				// read one letter.
				c = nextChar();
				s ~= c.to!string;
			}
		}
	}
	
	// EOF
	else if (c == EOF) token.type = TokenType.end_of_file, token.str = "__EOF__", token.line_num = line_num, token.index_num = index_num;
	
	return token;
}


