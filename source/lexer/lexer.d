module oak.lexer.lexer;
import std.stdio, std.range, std.ascii;
import std.algorithm: among;
import std.meta: aliasSeqOf;
import std.conv: to;

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
	integer, real_number, character, string_,
	true_, false_,
	
	// reserved words
	if_, else_, when, let, def,
    
	// expression symbols
	add, sub, mul, div, mod,    // + - * / %
	and, or, not,				// && || !
	eq, neq, ls, gt, leq, geq,	// ==, !=, <, >, <=, >=, 
	lPar, rPar,                 // ( )
	composition,				// .  (composition of functions) 
	dots2,						// .. 
	right_arrow, 				// -> (right_arrow)
	assign,						// =
	add_assign, sub_assign, mul_assign, div_assign, mod_assign, // += -= *= /= %=
	
	// other symbols
	colon, semicolon, 
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
    // string string_val; // this is 'str'.
}

Token nextToken(Range)(ref Range input, ref immutable(dchar)[] lookahead, ref ulong line_num)	// when characters were looked-ahead, they will be pushed on 'lookahead'
	if (isInputRange!Range && is(typeof(input.front) : immutable dchar))
{
	enum : dchar { EOF = cast(dchar) -1 }
	/*static immutable reserved_words = [
		"if" : TokenType.if_,
		"else" : TokenType.else_,
		"true" : TokenType.true_,
		"false" : TokenType.false_,
		"when" : TokenType.when,
	];*/
	
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
	
	// identifier
	if (isAlpha(c) || c == '_') {
		token.type = TokenType.identifier;
		while (isAlphaNum(c) || c == '_') {
			token.str ~= c;
			c = nextChar();
		}
		unget(c);
		
		/*auto ptr = token.str in reserved_words;
		if (ptr) token.type = *ptr;*/
		
		with (TokenType) switch (token.str) {
			case "if":		token.type = if_;		break;
			case "else":	token.type = else_;		break;
			case "true":	token.type = true_;		break;
			case "false":	token.type = false_;	break;
			case "when":	token.type = when;		break;
			case "let":		token.type = let;		break;
			case "def":		token.type = def;		break;
			default: break;
		}
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
	else if (c == '+') {
		if (lookAhead() == '=') {
			nextChar();
			token.type = TokenType.add_assign;
			token.str = "+=";
		}
		else {
			token.type = TokenType.add;
			token.str = "+";
		}
	}
	else if (c == '-') {
		if (lookAhead() == '=') {
			nextChar();
			token.type = TokenType.sub_assign;
			token.str = "-=";
		}
		else if (lookAhead() == '>') {
			nextChar();
			token.type = TokenType.right_arrow;
			token.str = "-=";
		}
		else {
			token.type = TokenType.sub;
			token.str = "-";
		}
	}
	else if (c == '*') {
		if (lookAhead() == '=') {
			nextChar();
			token.type = TokenType.mul_assign;
			token.str = "*=";
		}
		else {
			token.type = TokenType.mul_assign;
			token.str = "*";
		}
	}
	else if (c == '/') {
		if (lookAhead() == '=') {
			nextChar();
			token.type = TokenType.div_assign;
			token.str = "/=";
		}
		else {
			token.type = TokenType.div;
			token.str = "/";
		}
	}
	else if (c == '%') {
		if (lookAhead() == '=') {
			nextChar();
			token.type = TokenType.mod_assign;
			token.str = "%=";
		}
		else {
			token.type = TokenType.mod;
			token.str = "%";
		}
	}
	else if (c == '&') {
		if (lookAhead() == '&') {
			c = nextChar();
			token.type = TokenType.and;
			token.str = "&&";
		}
		else {
			token.type = TokenType.error;
			token.str = "Invalid token : '&" ~ c.to!string ~ "'";
		}
	}
	else if (c == '|') {
		if (lookAhead() == '|') {
			c = nextChar();
			token.type = TokenType.or;
			token.str = "||";
		}
		else {
			token.type = TokenType.error;
			token.str = "Invalid token : '|" ~ c.to!string ~ "'";
		}
	}
	else if (c == '!') {
		if (lookAhead() == '=') {
			nextChar();
			token.type = TokenType.neq;
			token.str = "!=";
		}
		else {
			token.type = TokenType.not;
			token.str = "!";
		}
	}
	else if (c == '=') {
		if (lookAhead() == '=') {
			nextChar();
			token.type = TokenType.eq;
			token.str = "==";
		}
		else {
			token.type = TokenType.assign;
			token.str = "=";
		}
	}
	else if (c == '<') {
		if (lookAhead() == '=') {
			nextChar();
			token.type = TokenType.leq;
			token.str = "<=";
		}
		else {
			token.type = TokenType.ls;
			token.str = "<";
		}
	}
	else if (c == '>') {
		if (lookAhead() == '=') {
			nextChar();
			token.type = TokenType.geq;
			token.str = ">=";
		}
		else {
			token.type = TokenType.gt;
			token.str = ">";
		}
	}
	
	else if (c == ';') {
		token.type = TokenType.semicolon;
		token.str = ";";
	}
	else if (c == ':') {
		token.type = TokenType.colon;
		token.str = ":";
	}
	else if (c == '(') {
		token.type = TokenType.lPar;
		token.str = "(";
	}
	else if (c == ')') {
		token.type = TokenType.rPar;
		token.str = ")";
	}
	else if (c == '[') {
		token.type = TokenType.lBrack;
		token.str = "[";
	}
	else if (c == ']') {
		token.type = TokenType.rBrack;
		token.str = "]";
	}
	else if (c == '{') {
		token.type = TokenType.lBrace;
		token.str = "{";
	}
	else if (c == '}') {
		token.type = TokenType.rBrace;
		token.str = "}";
	}
	
	else if (c == '.') {
		if (lookAhead() == '.') {
			nextChar();
			token.type = TokenType.dots2;
			token.str = "..";
		}
		else {
			token.type = TokenType.composition;
			token.str = ".";
		}
	}
	
	// EOF
	else if (c == EOF) token.type = TokenType.end_of_file;
	return token;
}


