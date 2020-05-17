module oak.lexer.lexer;
import std.stdio, std.range, std.ascii, std.typecons, std.algorithm, std.array;
import std.meta: aliasSeqOf;
import std.conv: to;
import oak.AATree.AATree;

unittest {
	string code = `
		a_2;: 234 {let when} ( ) def 0b111 0xF
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
		
		:;+-*/%+=-=*=/=%= <<=>>= .. ...||&&!!= 
		
		true false if else when let def
	`;
	immutable(dchar)[] lookahead;
	ulong line_num = 1;
	
	while (true) {
		//writeln(code, " |||| lookahead : '", lookahead, "'");
		auto token = nextToken(code, lookahead, line_num);
		writeln("'", token.str, "'\t\t", token.type, "\t\t", token.int_val);
		if (token.type == TokenType.end_of_file) break;
	}
}

enum TokenType {
	error,
	identifier,
	
	// literal
	integer, real_number, character, string_literal,
	true_, false_,
	
	// reserved words
	if_, else_, when, let, def,
    
	// expression symbols
	add, sub, mul, div, mod,    // + - * / %
	and, or, not,				// && || !
	eq, neq, ls, gt, leq, geq,	// ==, !=, <, >, <=, >=, 
	composition,				// .  (composition of functions) 
	dots2,						// .. 
	right_arrow, 				// -> (right_arrow)
	assign,						// =
	add_assign, sub_assign, mul_assign, div_assign, mod_assign, // += -= *= /= %=
	
	// other symbols
	colon, semicolon, 
	lPar, rPar,                 // ( )
	lBrack, rBrack,				// [ ]
	lBrace, rBrace,				// { }
	
	// EOF
	end_of_file,
}

struct Token {
    TokenType type;
    string str;
    ulong line_num;
    
    // literal value
    long int_val;
    double real_val;
    alias string_val = str;
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
	tuple("if", TokenType.if_),
	tuple("else", TokenType.else_),
	tuple("let", TokenType.let),
	tuple("def", TokenType.def),
	tuple("when", TokenType.when),
);

//Tuple!( dchar, immutable(Tuple!(string, "str", TokenType, "type"))[] )
//Tuple!( dchar, immutable(Tuple!(string, "str", TokenType, "type")[]) )

alias SE = Tuple!(string, "str", TokenType, "type");
static const reserved_symbols = new AATree!(dchar, (a,b) => a<b, SE[])(
	tuple(cast(dchar) '+', [SE("+", TokenType.add), SE("+=", TokenType.add_assign)]),
	tuple(cast(dchar) '-', [SE("-", TokenType.sub), SE("-=", TokenType.sub_assign), SE("->", TokenType.right_arrow)]),
	tuple(cast(dchar) '*', [SE("*", TokenType.mul), SE("*=", TokenType.mul_assign)]),
	tuple(cast(dchar) '/', [SE("/", TokenType.div), SE("/=", TokenType.div_assign)]),
	tuple(cast(dchar) '%', [SE("%", TokenType.mod), SE("%=", TokenType.mod_assign)]),
	tuple(cast(dchar) '&', [SE("&&", TokenType.and)]),
	tuple(cast(dchar) '|', [SE("||", TokenType.and)]),
	tuple(cast(dchar) '!', [SE("!", TokenType.not), SE("!=", TokenType.neq)]),
	tuple(cast(dchar) '=', [SE("=", TokenType.assign), SE("==", TokenType.eq)]),
	tuple(cast(dchar) '<', [SE("<", TokenType.ls), SE("<=", TokenType.leq)]),
	tuple(cast(dchar) '>', [SE(">", TokenType.gt), SE(">=", TokenType.geq)]),
	tuple(cast(dchar) '.', [SE(".", TokenType.composition), SE("..", TokenType.dots2)]),
	
	tuple(cast(dchar) ':', [SE(":", TokenType.colon)]),
	tuple(cast(dchar) ';', [SE(";", TokenType.semicolon)]),
	tuple(cast(dchar) '(', [SE("(", TokenType.lPar)]),
	tuple(cast(dchar) ')', [SE(")", TokenType.rPar)]),
	tuple(cast(dchar) '[', [SE("[", TokenType.lBrack)]),
	tuple(cast(dchar) ']', [SE("]", TokenType.rBrack)]),
	tuple(cast(dchar) '{', [SE("{", TokenType.lBrace)]),
	tuple(cast(dchar) '}', [SE("}", TokenType.rBrace)]),
	
);

Token nextToken(Range)(ref Range input, ref immutable(dchar)[] lookahead, ref ulong line_num)	// when characters were looked-ahead, they will be pushed on 'lookahead'
	if (isInputRange!Range && is(typeof(input.front) : immutable dchar))
{
	enum EOF = cast(dchar) -1;
	
	
	// wrapper for EOF and look-ahead
	dchar nextChar() {
		if (lookahead.length > 0) {
			auto c = lookahead[0];
			lookahead = lookahead[1 .. $];
			debug(lexer) writeln("'", c, "'");
			return c;
		}
		else {
			if (input.empty)
				return EOF;
			auto c = input.front;
			if (c == '\n') ++line_num;
			input.popFront();
			debug(lexer) writeln("'", c, "'");
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
					if (c == '\n') ++line_num;
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
		token.type = TokenType.identifier;
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
		token.type = TokenType.integer;
		
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
				else if (c == '_') { token.str ~= c; }
				else { unget(c); break; }
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
				else if (c == '_') { token.str ~= c; }
				else { unget(c); break; }
				c = nextChar();
			}
		}
		else {
			while (true) {
				if (isDigit(c)) {
					token.int_val *= 10;
					token.int_val += c - '0';
					token.str ~= c;
				}
				else if (c == '_') { token.str ~= c; }
				else { unget(c); break; }
				c = nextChar();
			}
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
				token.type = SElist[0].type;
				token.str = s[0 .. $-1];
				unget(s[$-1]);
				return token;
			}
			SElist = newSElist;
			
			// found
			if (SElist.length == 1 && SElist[0].str.length == s.length /* equivalent to SElist[0].str == s */) {
				//writeln("\t length 1");
				token.type = SElist[0].type;
				token.str = s;
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
	else if (c == EOF) token.type = TokenType.end_of_file;
	
	
	return token;
}


