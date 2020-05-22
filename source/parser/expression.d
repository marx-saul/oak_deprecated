module parser.expression;

import std.stdio, std.typecons, std.algorithm;
import aatree, parser.lexer, parser.defs;
import parser.defs;

unittest {
	import parser.defs : TokenRange;
	
	//auto token_pusher = new TokenRange!string("f n = n * f (n-1) when n > 0 : 1");
	//auto token_pusher = new TokenRange!string("a = (b1, b2) when c : d,e");
	auto token_pusher = new TokenRange!string("a ^^ -b ^^ -c");
	auto node = getAST(token_pusher);
	node.stringOfExpression.writeln();
	
	token_pusher = new TokenRange!string("a <= b > c");
	node = getAST(token_pusher);
	
	//token_pusher = new TokenRange!string("");
	//node = getAST(token_pusher);
	
	writeln("\n#### parse/expression.d unittest1");
}

// right associative is odd, left associative is even
static const op_ranks = new AATree!(TokenType, (a,b)=>a<b, int)(
	tuple(TokenType.dummy, 0),
	tuple(TokenType.end_of_file, 0),
	
	tuple(TokenType.lPar, 50),
	tuple(TokenType.assign, 101), tuple(TokenType.add_assign, 101), tuple(TokenType.sub_assign, 101), tuple(TokenType.cat_assign, 101),
	tuple(TokenType.mul_assign, 101), tuple(TokenType.div_assign, 101), tuple(TokenType.mod_assign, 101), 
	tuple(TokenType.colon, 150),
	tuple(TokenType.comma, 201),
	tuple(TokenType.sharp, 301),
	tuple(TokenType.when,  401),
	tuple(TokenType.pipeline, 500),
	tuple(TokenType.or,  600),
	tuple(TokenType.and, 700),
	tuple(TokenType.ls,  1100), tuple(TokenType.leq, 1100), tuple(TokenType.gt,  1100),
	tuple(TokenType.geq, 1100), tuple(TokenType.eq,  1100), tuple(TokenType.neq, 1100),
	tuple(TokenType.add, 1200), tuple(TokenType.sub, 1200), tuple(TokenType.cat, 1200),
	tuple(TokenType.mul, 1300), tuple(TokenType.div, 1300), tuple(TokenType.mod, 1300),
	tuple(TokenType.not, 1400),
	tuple(TokenType.pow, 1501),
	tuple(TokenType.unary_op, 1600),
	tuple(TokenType.app, 1700),
	tuple(TokenType.composition, 1800),
	tuple(TokenType.dot, 1900),
	
);

enum OpPrec { shift, equal, reduce, error, accept }

// return which is decendent
OpPrec precedence(TokenType a, TokenType b) {
	with (TokenType) {
		if ( (a == rPar  && b == lPar) ||
			 (a == dummy && b == rPar) ||
			 (a == lPar  && b == end_of_file)
		) return OpPrec.error;
		
		else if ( a == dummy && b == end_of_file ) return OpPrec.accept;
		else if ( a == lPar  && b == rPar )        return OpPrec.equal;
		
		auto ar = op_ranks[a],
			 br = op_ranks[b];
		if (ar < br) 			return OpPrec.shift;
		else if (ar > br) 		return OpPrec.reduce;
		else if (ar % 2 == 1) 	return OpPrec.shift;
		else 					return OpPrec.reduce;
	}
}

bool isFirstOfAtomExpr(TokenType a) {
	with(TokenType)
		return a.among!(add, sub, not, lPar, dollar, identifier, integer, real_number, character, string_literal, true_, false_) != 0;
}

ExprNode getAST(Range)(ref Range input)
	if (isTokenRange!Range)
{
	ExprNode[] nodes;
	ExprNode[] operators = [new ExprNode(TokenType.dummy)];
	bool op_expected = false;
	Token[] pushed;				// for unary operators
	
	Token front() {
		if (pushed.length > 0) return pushed[0];
		else return input.front;
	}
	void popFront() {
		if (pushed.length > 0) pushed = pushed[1 .. $];
		else input.popFront();
	}
	
	while (true) {
		auto token = input.front;
	redo_with_token_indicated:
		// operator expected
		if ( op_expected ) {
			// function application
			if ( !token.type.among!(TokenType.add, TokenType.sub) && isFirstOfAtomExpr(token.type) ) {
				// shift virtual token app
				token.type = TokenType.app, token.str = "app";
				operators ~= new ExprNode(token);
				op_expected = false;
				
				writeln("A shift app");
				nodes.each!(x => write(x.token.str, " "));
				write(" ///// ");
				operators.each!(x => write(x.token.str, " "));
				writeln();
			}
			// `)`
			else if (token.type == TokenType.rPar) {
				// matched with `(`
				if (operators[$-1].token.type == TokenType.lPar) {
					operators.length -= 1;
					writeln("matched with (");
					input.popFront();
				}
				// extra `)` error
				//else if (operators[$-1].token.type == TokenType.rPar) {
				//	writeln("extra ')' was found.");
				//	break;
				//}
				// reduce
				else if (op_ranks.hasKey(operators[$-1].token.type)) {
					auto right = nodes[$-1];
					auto left  = nodes[$-2];
					operators[$-1].left = left, operators[$-1].right = right;
					nodes[$-2] = operators[$-1];
					operators.length -= 1; nodes.length -= 1;
					
					writeln("A reduce");
					nodes.each!(x => write(x.token.str));
					write(" ///// ");
					operators.each!(x => write(x.token.str));
					writeln();
					
					// `,` in ( .. )
					if (nodes[$-1].token.type == TokenType.comma) {
						auto expr_node = nodes[$-1];
						expr_node.tuple_solved = true;
					}
				}
			}
			// binary operator
			else if (op_ranks.hasKey(token.type)) {
				auto cmp = precedence(operators[$-1].token.type, token.type);
				// shift
				if      (cmp == OpPrec.shift) {
					operators ~= new ExprNode(token);
					input.popFront();
					op_expected = false;
					
					writeln("B shift ", token.str);
					nodes.each!(x => write(x.token.str, " "));
					write(" ///// ");
					operators.each!(x => write(x.token.str, " "));
					writeln();
				}
				// reduce
				else if (cmp == OpPrec.reduce) {
					auto right = nodes[$-1];
					auto left  = nodes[$-2];
					
					// check E when E' : E''.
					// it is processed as (E when E') : E''
					if (operators[$-1].token.type == TokenType.colon) {
						// error
						if (left.token.type != TokenType.when) {
							writeln("'when' corresponding to ':' not found.");
							break;
						}
						// correct syntax
						else {
							alias when = left;
							when.center = when.right;
							when.right = right;
							nodes[$-2] = when;
							operators.length -= 1; nodes.length -= 1;
						}
					}
					else {
						operators[$-1].left = left, operators[$-1].right = right;
						// exclude a < b >= c
						with(TokenType)
							if (operators[$-1].token.type.among!(ls, leq, gt, geq) && left.token.type.among!(ls, leq, gt, geq)) {
								writeln("Expressions of the form a " ~ operators[$-1].token.str ~ " b " ~ left.token.str~ " c is invalid");
								break;
							}
						nodes[$-2] = operators[$-1];
						operators.length -= 1; nodes.length -= 1;
					}
					
					writeln("B reduce");
					nodes.each!(x => write(x.token.str, " "));
					write(" ///// ");
					operators.each!(x => write(x.token.str, " "));
					writeln();
				}
				// accept!
				else if (cmp == OpPrec.accept) {
					writeln("accept");
					assert (nodes.length == 1);
					return nodes[0];
				}
				else assert(0);
			}
			// if giving EOF to the parser make it parse successfully, then stop parsing
			else if (operators[$-1].token.type == TokenType.dummy) {
				writeln ("accept");
				assert (nodes.length == 1);
				return nodes[0];
			}
			// error
			else { writeln("Invalid expression syntax.", __LINE__); break; }
		}
		// atom expression expected
		else {
			// unary +, -, ++, --, !
			if (token.type.among!(TokenType.add, TokenType.sub, TokenType.not)) {
				// shift the operator and then read unary 
				// idea is to regard -a as `- unary_op a`
				token.str = "u" ~ token.str;
				nodes ~= new ExprNode(token);
				token.type = TokenType.unary_op, token.str = "unary_op";
				op_expected = true;
				
				writeln("C shift ", token.str);
				nodes.each!(x => write(x.token.str, " "));
				write(" ///// ");
				operators.each!(x => write(x.token.str, " "));
				writeln();
				
				goto redo_with_token_indicated;
			}
			// postfix
			//else if (token.type == TokenType.inc || token.type == TokenType.dec) {
			
			//}
			// shift
			else if (token.type.isFirstOfAtomExpr()) {
				if (token.type == TokenType.lPar) {
					operators ~= new ExprNode(token);
					writeln("D shift ", token.str);
					nodes.each!(x => write(x.token.str, " "));
					write(" ///// ");
					operators.each!(x => write(x.token.str, " "));
					writeln();
				}
				else {
					nodes ~= new ExprNode(token);
					op_expected = true;
					
					writeln("E shift ", token.str);
					nodes.each!(x => write(x.token.str, " "));
					write(" ///// ");
					operators.each!(x => write(x.token.str, " "));
					writeln();
				}
				
				input.popFront();
			}
			// empty tuple ()
			else if ( token.type == TokenType.rPar && operators[$-1].token.type == TokenType.lPar ) {
				Token new_token = { type: TokenType.empty_tuple, str: "()" };
				nodes ~= new ExprNode(new_token);
				operators.length -= 1;
				input.popFront();
				
				writeln("F shift ", token.str);
				nodes.each!(x => write(x.token.str, " "));
				write(" ///// ");
				operators.each!(x => write(x.token.str, " "));
				writeln();
				
				op_expected = true;
			}
			// error
			else { writeln("An atom expression expected. Not " ~ token.str, " ", token.type); break; }
		}
	}
	return null;
}

string stringOfExpression(ExprNode node) {
	if (node is null) return "";
	
	//auto node = cast (ExprNode) n;
	with(TokenType) switch (node.token.type) {
	case when:
		return " ((" ~ stringOfExpression(node.left) ~ " ) when (" ~ stringOfExpression(node.center) ~ " ) : (" ~ stringOfExpression(node.right) ~ " ))" ; 	
	
	case comma:
		if (node.tuple_solved) { goto default; }
		else { node.token.str = ",?"; goto default; }
	
	default:
		if (node.left is null && node.center is null && node.right is null ) return " " ~ node.token.str;
		else return " (" ~ stringOfExpression(node.left) ~ " " ~ node.token.str ~ stringOfExpression(node.right) ~ " )";
	
	}
}
