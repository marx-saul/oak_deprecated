module lexer;
import std.stdio;
import std.ascii;
import std.algorithm;
import std.array;
import std.typecons: tuple, Tuple;
import std.traits: ReturnType;
import std.range: isInputRange;
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
		
		true false if else when let
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

public struct Location {
	ulong line_num;
	ulong index_num;
	string[] path;
}

// when adding a token, alter this enum TokenType, (reserved_words/reserved_symbols)
enum TokenType {
	dummy,

	identifier,

	integer,
	real_number,
	string_literal,

	// reserved words
	true_,
	false_,
	null_,

	int_,
	real_,
	string_,
	bool_,
	void_,
	unit,

	struct_,
	class_,
	interface_,

	immut,
	const_,
	inout_,

	shadow,
	ref_,

	private_,
	protected_,
	package_,
	public_,
	export_,
	abstract_,
	override_,

	pure_,
	lazy_,

	import_,
	module_,
	let,
	func,

	any,
	this_,
	super_,

	if_,
	else_,
	do_,
	while_,
	for_,
	foreach_,
	foreach_reverse_,
	break_,
	continue_,
	return_,

	// binary(unary) operator (precedence order)
	param_expr,	param_type,				// :. ::
	dot,								// .
	composition,						// @
	indexing, dotdot,					// indexing, slicing ![ ] ![ .. ]
	apply, 								// function application f x
	pow,								// ^^
	//unary
	u_sub, bit_not, not, ref_of, deref,	// -- ~ not # !
	mul, div, mod,						// * / %
	add, sub, cat,						// + - ++
	lshift, rshift, log_shift,			// << >> >>>

	eq, neq, ls, gt, leq, geq, 			// ==, !=, <, >, <=, >=,
	in_, nin, is_, nis,					// in !in is !is

	bit_and,							// &
	bit_xor,							// ^
	bit_or,								// |
	and,								// and
	xor,								// xor
	or,									// or
	app,								// app
	pipeline,							// |>
	when,								// when
	match,								// match
	assign,								// =
	add_assign, sub_assign, mul_assign,	// += -= *=
	div_assign, mod_assign, cat_assign, // /= %= ++=
	and_assign, xor_assign,  or_assign,	// &= ^= |=

	// other symbols
	dollar,								// $
	lambda,								// \
	right_arrow,						// ->
	colon, semicolon, comma,			// : ; ,
	lPar, rPar,                 		// ( )
	lBrack, rBrack,						// [ ]
	lBrace, rBrace,						// { }
	
	// EOF
	end_of_file,
}

struct Token {
	TokenType type;
	string str;
	Location loc;

	// literal value
	//long int_val;
	//double real_val;
	//alias string_val = str;
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
	tuple("true",            TokenType.true_),
	tuple("false",           TokenType.false_),
	tuple("null",            TokenType.null_),

	tuple("int",             TokenType.int_),
	tuple("real",            TokenType.real_),
	tuple("string",          TokenType.string_),
	tuple("bool",            TokenType.bool_),
	tuple("void",            TokenType.void_),
	tuple("unit",            TokenType.unit),

	tuple("struct",          TokenType.struct_),
	tuple("class",           TokenType.class_),
	tuple("interface",       TokenType.interface_),

	tuple("import",          TokenType.import_),
	tuple("module",          TokenType.module_),
	tuple("func",            TokenType.func),
	tuple("let",             TokenType.let),
	tuple("func",            TokenType.func),

	tuple("immut",           TokenType.immut),
	tuple("const",           TokenType.const_),
	tuple("inout",           TokenType.inout_),

	tuple("shadow",          TokenType.shadow),
	tuple("ref",             TokenType.ref_),

	tuple("private",         TokenType.private_),
	tuple("protected",       TokenType.protected_),
	tuple("package",         TokenType.package_),
	tuple("public",          TokenType.public_),
	tuple("export",          TokenType.export_),
	tuple("abstract",        TokenType.abstract_),
	tuple("override",        TokenType.override_),

	tuple("any",             TokenType.any),
	tuple("this",            TokenType.this_),
	tuple("super",           TokenType.super_),

	tuple("if",              TokenType.if_),
	tuple("else",            TokenType.else_),
	tuple("do",              TokenType.do_),
	tuple("while",           TokenType.while_),
	tuple("for",             TokenType.for_),
	tuple("foreach",         TokenType.foreach_),
	tuple("foreach_reverse", TokenType.foreach_reverse_),
	tuple("break",           TokenType.break_),
	tuple("continue",        TokenType.continue_),
	tuple("return",          TokenType.return_),

	tuple("app",             TokenType.app),
	tuple("in",              TokenType.in_),
	tuple("is",              TokenType.is_),
	tuple("not",             TokenType.not),
	tuple("and",             TokenType.and),
	tuple("xor",             TokenType.xor),
	tuple("or",              TokenType.or),
	tuple("when",            TokenType.when),
	tuple("match",           TokenType.match),
);

static const type_of_literal = new AATree!(TokenType, (a,b) => a<b, string) (
	tuple(TokenType.integer,        "int"),
	tuple(TokenType.real_number,    "real"),
	tuple(TokenType.string_literal, "string"),
	tuple(TokenType.true_,          "bool"),
	tuple(TokenType.false_,         "bool"),
);

//Tuple!( dchar, immutable(Tuple!(string, "str", TokenType, "type"))[] )
//Tuple!( dchar, immutable(Tuple!(string, "str", TokenType, "type")[]) )

alias SE = Tuple!(string, "str", TokenType, "type");
static const reserved_symbols = new AATree!(dchar, (a,b) => a<b, SE[])(
	tuple(cast(dchar) '+', [SE("+",   TokenType.add),
							SE("++",  TokenType.cat),
							SE("+=",  TokenType.add_assign),
							SE("++=", TokenType.cat_assign)]),
	tuple(cast(dchar) '-', [SE("-",   TokenType.sub),
							SE("--",  TokenType.u_sub),
							SE("-=",  TokenType.sub_assign),
							SE("->",  TokenType.right_arrow)]),
	tuple(cast(dchar) '*', [SE("*",   TokenType.mul),
							SE("*=",  TokenType.mul_assign)]),
	tuple(cast(dchar) '/', [SE("/",   TokenType.div),
							SE("/=",  TokenType.div_assign)]),
	tuple(cast(dchar) '%', [SE("%",   TokenType.mod),
							SE("%=",  TokenType.mod_assign)]),
	tuple(cast(dchar) '^', [SE("^",   TokenType.bit_xor),
							SE("^^",  TokenType.pow),
							SE("^=",  TokenType.xor_assign),]),
	tuple(cast(dchar) '~', [SE("~",   TokenType.bit_not)]),
	tuple(cast(dchar) '&', [SE("&",   TokenType.bit_and),
							SE("&=",  TokenType.and_assign)]),
	tuple(cast(dchar) '|', [SE("|",   TokenType.bit_or),
							SE("|>",  TokenType.pipeline),
							SE("|=",  TokenType.or_assign)]),
	tuple(cast(dchar) '!', [SE("!",   TokenType.deref),
							SE("![",  TokenType.indexing),
							SE("!=",  TokenType.neq),
							SE("!in", TokenType.nin),
							SE("!is", TokenType.nis),]),
	//tuple(cast(dchar) '?', [SE("?",   TokenType.ref_of)]),
	tuple(cast(dchar) '#', [SE("#",   TokenType.ref_of)]),
	tuple(cast(dchar) '$', [SE("$",   TokenType.dollar)]),
	tuple(cast(dchar) ',', [SE(",",   TokenType.comma)]),
	tuple(cast(dchar) '\\',[SE("\\",  TokenType.lambda)]),
	tuple(cast(dchar) '=', [SE("=",   TokenType.assign),
							SE("==",  TokenType.eq)]),
	tuple(cast(dchar) '<', [SE("<",   TokenType.ls),
							SE("<=",  TokenType.leq),
							SE("<<",  TokenType.lshift)]),
	tuple(cast(dchar) '>', [SE(">",   TokenType.gt),
							SE(">=",  TokenType.geq),
							SE(">>",  TokenType.rshift),
							SE(">>>", TokenType.log_shift)]),
	tuple(cast(dchar) '.', [SE(".",   TokenType.dot),
							SE("..",  TokenType.dotdot)]),
	tuple(cast(dchar) '@', [SE("@",   TokenType.composition)]),
	tuple(cast(dchar) ':', [SE(":",   TokenType.colon),
							SE("::",  TokenType.param_type),
							SE(":.",  TokenType.param_expr)]),
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

Token nextToken(Range)(ref Range input, ref immutable(dchar)[] lookahead, ref ulong line_num, ref ulong index_num, string path = "")	// when characters were looked-ahead, they will be pushed on 'lookahead'
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
		if (c == '\n') --line_num;
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
	token.loc.line_num = line_num; token.loc.index_num = index_num;
	
	// identifier or reserved word
	if (isAlpha(c) || c == '_') {
		token.type = TokenType.identifier;
		while (isAlphaNum(c) || c == '_') {
			token.str ~= c;
			c = nextChar();
		}
		unget(c);
		
		/*auto ptr = token.str in reserved_words;
		if (ptr) token.type = *ptr;*/
		
		if (reserved_words.hasKey(token.str)) token.type = reserved_words[token.str];
		// a token starting with '__'
		if (token.str.length >= 2 && token.str[0] == '_' && token.str[1] == '_' && token.type == TokenType.identifier) {
			writeln("a token starting '__' is reserved by compiler.");
			token.str = "_1_" ~ token.str[2 .. $];
		}
	}
	
	// integer, real number
	else if (isDigit(c)) {
		token.type = TokenType.integer, token.loc.line_num = line_num, token.loc.index_num = index_num;
		
		// hexadecimal
		if (c == '0' && lookAhead().among!('x', 'X')) {
			auto c2 = nextChar();	// 'x'
			token.str = "0x";
			c = nextChar();
			while (true) {
				if (c.among!(aliasSeqOf!"_0123456789abcdefABCDEF")) {
					token.str ~= c;
				}
				// '.' found real number
				else if (c == '.') {
					// ..
					if (lookAhead() == '.' || !lookAhead().among!(aliasSeqOf!"_0123456789abcdefABCDEF")) {
						unget('.');
						return token;
					}

					token.str ~= ".";
					token.type = TokenType.real_number;
					
					c = nextChar();
					while (true) {
						if (c.among!(aliasSeqOf!"_0123456789abcdefABCDEF")) {
							token.str ~= c;
						}
						else {
							unget(c);
							return token;
						}
						c = nextChar();
					}
				}
				else {
					unget(c);
					return token;
				}
				c = nextChar();
			}
		}
		// binary
		else if (c == '0' && lookAhead().among!('b', 'B')) {
			auto c2 = nextChar();	// 'b'
			token.str = "0b";
			c = nextChar();
			while (true) {
				if (c.among!(aliasSeqOf!"_01")) {
					token.str ~= c;
				}
				// '.' found real number
				else if (c == '.') {
					// ..
					if (lookAhead() == '.' || !lookAhead().among!(aliasSeqOf!"_01")) {
						unget('.');
						return token;
					}

					token.str ~= ".";
					token.type = TokenType.real_number;

					c = nextChar();
					while (true) {
						if (c.among!(aliasSeqOf!"_01")) {
							token.str ~= c;
						}
						else {
							unget(c);
							return token;
						}
						c = nextChar();
					}
				}
				else {
					unget(c);
					return token;
				}
				c = nextChar();
			}
		}
		// 10
		else {
			token.str = c.to!string;
			c = nextChar();
			while (true) {
				if (c.among!(aliasSeqOf!"_0123456789")) {
					token.str ~= c;
				}
				// '.' found real number
				else if (c == '.') {
					// ..
					if (lookAhead() == '.' || !lookAhead().among!(aliasSeqOf!"_0123456789")) {
						unget('.');
						return token;
					}

					token.str ~= ".";
					token.type = TokenType.real_number;

					c = nextChar();
					while (true) {
						if (c.among!(aliasSeqOf!"_0123456789")) {
							token.str ~= c;
						}
						else {
							unget(c);
							return token;
						}
						c = nextChar();
					}
				}
				else {
					unget(c);
					return token;
				}
				c = nextChar();
			}
		}
	}
	
	// string literal
	else if (c == '"') {
		token.type = TokenType.string_literal, token.loc.line_num = line_num, token.loc.index_num = index_num;
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
				token.type = SElist[0].type, token.str = s[0 .. $-1], token.loc.line_num = line_num, token.loc.index_num = index_num;
				unget(s[$-1]);
				return token;
			}
			SElist = newSElist;
			
			// found
			if (SElist.length == 1 && SElist[0].str.length == s.length /* equivalent to SElist[0].str == s */) {
				//writeln("\t length 1");
				token.type = SElist[0].type, token.str = s, token.loc.line_num = line_num, token.loc.index_num = index_num;
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
	else if (c == EOF) token.type = TokenType.end_of_file, token.str = "__EOF__", token.loc.line_num = line_num, token.loc.index_num = index_num;
	
	return token;
}


/* token pusher */
enum isLexer(T) =
	is(ReturnType!((T t) => t.front) == Token) &&
	//is(ReturnType!((T t) => t.empty) == bool) &&
	is( typeof( { T t; t.popFront(); } ) ) &&
	is(ReturnType!((T t) => t.lookahead) == Token);

class Lexer(R)
	if (isInputRange!R && is(ReturnType!((R r) => r.front) : immutable dchar))
{
	private R source;
	private immutable(dchar)[] char_lookahead;
	ulong line_num = 1, index_num = 1;
	this (R s) {
		source = s;
		token = nextToken(source, char_lookahead, line_num, index_num);
	}

	private Token token;
	private Token token_ahead;
	private bool looked_ahead = false;
	Token front() @property {
		return token;
	}
	void popFront() {
		if (looked_ahead) {
			token = token_ahead;
		}
		else token = nextToken(source, char_lookahead, line_num, index_num);
		looked_ahead = false;
	}
	Token lookahead() @property {
		if (looked_ahead) return token_ahead;
		else return token;
	}
}

static assert (isLexer!(Lexer!string));


