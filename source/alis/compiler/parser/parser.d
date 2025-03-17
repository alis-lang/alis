/++
Parser Base
+/
module alis.compiler.parser.parser;

import alis.compiler.common,
			 alis.compiler.lexer,
			 alis.compiler.error;

import meta;

import utils.ds;

import std.meta,
			 std.traits,
			 std.typecons,
			 std.algorithm,
			 std.format,
			 std.array,
			 std.conv,
			 std.string;

debug import std.stdio;

/// Returns: CmpErr with UnxpTokErr
private CmpErr unxpTokErr(R...)(Tok tok){
	string msg = "parsing error, unexpected token: Expected one of: " ~
		Join!(", ", staticMap!(StringOf, R)) ~ ". But found: " ~ tok.token;
	return CmpErr(tok.line, tok.col, msg, CmpErr.Type.UnxpTok);
}

/// UDA: tag as Grammar Function when no other UDA is appropriate
enum GFn;

/// UDA: Set Hook token that trigger a persing function
struct Hook{
	/// triggering token type
	TokType hook;
	@disable this();
	/// constructor
	this (TokType hook){
		this.hook = hook;
	}
}

/// UDA: Expression. This should only be used for non-operator expressions
enum Expr;

/// UDA: Tag as Binary operator, with precedences
/// Precedences are set for both left and right sides as same, unless both are
/// specified
struct OpBin{
	/// operator. null if hookless
	string op = null;
	/// precedence
	ubyte preced = ubyte.max;
	/// constructor
	this(string op, ubyte preced){
		this.op = op;
		this.preced = preced;
	}
}

/// UDA: Tag as PostFix Unary operator, with precedence
struct OpPost{
	/// operator. null if hookless
	string op = null;
	/// precedence
	ubyte preced = ubyte.max;
	/// constructor
	this(string op, ubyte preced){
		this.op = op;
		this.preced = preced;
	}
}

/// UDA: Tag as PreFix Unary operator, with precedence
struct OpPre{
	/// operator. null if hookless
	string op = null;
	/// precedence
	ubyte preced = ubyte.max;
	/// constructor
	this(string op, ubyte preced){
		this.op = op;
		this.preced = preced;
	}
}

/// UDA: Tag as a post processing function
enum PFn;

/// Whether a Grammar Function is a Hookless Operator parser
private template GrmrFnOpIsHkls(alias Fn){
	static if ((hasUDA!(Fn, OpPre) && getUDAs!(Fn, OpPre)[0].op == null &&
				getUDAs!(Fn, OpPre)[0].preced == ubyte.max) ||
			(hasUDA!(Fn, OpPost) && getUDAs!(Fn, OpPost)[0].op == null &&
				getUDAs!(Fn, OpPost)[0].preced == ubyte.max) ||
			(hasUDA!(Fn, OpBin) && getUDAs!(Fn, OpBin)[0].op == null &&
				getUDAs!(Fn, OpBin)[0].preced == ubyte.max))
		enum GrmrFnOpIsHkls = true;
	else
		enum GrmrFnOpIsHkls = false;
}

/// If `T` is any Expression (Expr, or OpPre/Post/Bin)
private template IsExpr(alias T){
	enum IsExpr = Instantiate!(HasAnyUDA!(Expr, OpPre, OpPost, OpBin), T);
}
///
unittest{
	@Expr enum E;
	enum NotE;
	assert(IsExpr!E);
	assert(!IsExpr!NotE);
}

/// If `T` is not any Expression (Expr, or OpPre/Post/Bin)
private template IsNotExpr(alias T){
	enum IsNotExpr = !Instantiate!(HasAnyUDA!(Expr, OpPre, OpPost, OpBin), T);
}
///
unittest{
	@Expr enum E;
	enum NotE;
	assert(!IsNotExpr!E);
	assert(IsNotExpr!NotE);
}

/// If `F` is a valid post function for any node
private template IsPostFn(alias F){
	static if (isCallable!F &&
			Parameters!F.length == 1 &&
			is (Parameters!F[0] : ASTNode) &&
			hasUDA!(F, PFn)){
		enum IsPostFn = true;
	} else {
		enum IsPostFn = false;
	}
}

/// If `F` is a valid grammar function
///
/// Template Params:
/// - `E` - Expression Node type
/// - `Err` - Error base class
/// - `F` - symbol to check
private template IsGrmrFn(E, Err) if (is (E : ASTNode)){
	template IsGrmrFn(alias F){
		static if (
				isCallable!F &&
				// first paramater always TokRange
				(Parameters!F.length >= 1 && is (Parameters!F[0] == TokRange)) &&

				(
				 // if only 1 param, then should only be tagged with @Hook
				 (Parameters!F.length == 1 &&
					(hasUDA!(F, Hook) || hasUDA!(F, GFn)) &&
					!hasUDA!(F, Expr) &&
					!hasUDA!(F, OpPre) &&
					!hasUDA!(F, OpPost) &&
					!hasUDA!(F, OpBin)
				 )
				 ||

				 // if 2 params, must be (TokRange, preced), and be tagged with either
				 // but not both of: @Expr, @OpPre
				 (Parameters!F.length == 1 && (
																			 hasUDA!(F, Expr) ||
																			 hasUDA!(F, OpPre)
																			) &&
					(hasUDA!(F, Expr) + hasUDA!(F, OpPre) == 1) &&
					!hasUDA!(F, OpPost) &&
					!hasUDA!(F, OpBin) &&
					(!hasUDA!(F, Expr) || hasUDA!(F, Hook))
				 )
				 ||

				 // if 3 params, must be (TokRange, LHS expr, preced), and be tagged
				 // with either but not both: @OpBin, @OpPost
				 (Parameters!F.length == 2 && (
																			 hasUDA!(F, OpBin) ||
																			 hasUDA!(F, OpPost)
																			) &&
					!hasUDA!(F, Expr) &&
					!hasUDA!(F, Hook) &&
					!hasUDA!(F, OpPre) &&
					(hasUDA!(F, OpBin) + hasUDA!(F, OpPost) == 1) &&
					is (E : Parameters!F[1])
				 )
				) &&

				is (typeof(ReturnType!F.val) : ASTNode) &&
				is (typeof(ReturnType!F.err) : Err)){
			enum IsGrmrFn = true;
		} else {
			enum IsGrmrFn = false;
		}
	}
}

/// Gets subset post processing functions for Node Type N, among `PFns...`
private template PostFnsFor(N, PFns...) if (
		is (N : ASTNode) && allSatisfy!(IsPostFn, PFns)){
	alias PostFnsFor = AliasSeq!();
	static foreach (T; AliasSeq!(N, BaseClassesTuple!N)){
		static if (staticIndexOf!(T, FirstParamsOf!PFns) != -1){
			PostFnsFor = AliasSeq!(PostFnsFor,
					PFns[staticIndexOf!(T, FirstParamsOf!PFns)]);
		}
	}
}

/// Whether a node type `N` has any post processing functions among `PFns...`
private template HasPFn(N, PFns...) if (
		is (N : ASTNode) && allSatisfy!(IsPostFn, PFns)){
	enum HasPFn = PostFnsFor!(N, PFns).length > 0;
}

///
unittest{
	class A{}
	class B : A{}
	class C : B{}
	class D : A{}
	class E : B{}
	assert(is (CommonParent!(A, B, C, D, E) == A));
	assert(is (CommonParent!(B, C, D, E) == A));
	assert(is (CommonParent!(C, D, E) == A));
	assert(is (CommonParent!(C, E) == B));
}

/// if a type is a TokeType
private enum IsTokType(alias T) = is (typeof(T) == TokType);

/// If `F` does not have any `@Hook`
private enum HasNoHook(alias F) = getUDAs!(F, Hook).length == 0;

/// If a hook token is present in a `Hook` UDA on a symbol
private template HasHook(H...) {
	template HasHook(alias F){
		enum HasHook = hasHook();
		private bool hasHook(){
			static foreach (h; H){
				static foreach (Hook hook; getUDAs!(F, Hook)){
					static if (hook.hook == h.hook)
						return true;
				}
			}
			return false;
		}
	}
}

/// Hook TokTypes of T
private template HooksOf(T...){
	alias HooksOf = AliasSeq!();
	static foreach (Fn; T){
		static foreach (Hook hook; getUDAs!(Fn, Hook))
			HooksOf = AliasSeq!(HooksOf, hook.hook);
	}
	HooksOf = NoDuplicates!HooksOf;
}

/// Whether an `OpPre/Post/Bin(Op)` tag is found on a Grammar Function `F`
private template HasOp(string Op){
	template HasOp(alias F){
		enum HasOp = doTheCheck;
		bool doTheCheck(){
			static foreach (op; getUDAs!(F, OpPre)){
				static if (op.op == Op)
					return true;
			}
			static foreach (op; getUDAs!(F, OpPost)){
				static if (op.op == Op)
					return true;
			}
			static foreach (op; getUDAs!(F, OpBin)){
				static if (op.op == Op)
					return true;
			}
			return false;
		}
	}
}

/// non-erroneous return type for `F`
private template GrmrFnType(alias F) if (isCallable!F){
	alias GrmrFnType = typeof(ReturnType!F.val);
}

/// If any of any among `R` is the return type for Grammar Function `F`
private template GrmrFnIsOfType(R...){
	private template GrmrFnIsOfType(alias F){
		enum GrmrFnIsOfType = doTheCheck();
		bool doTheCheck(){
			static foreach (RT; R){
				static if (is (GrmrFnType!F : RT))
					return true;
			}
			return false;
		}
	}
}

/// Sequence of operators (`opPre`, `opPost`, & `opBin`) found across symbols
/// `T`
private template OpStrs(T...){
	alias OpStrs = AliasSeq!();
	static foreach (sym; T){
		static foreach(OpPre op; getUDAs!(sym, OpPre)){
			static if (op.op !is null)
				OpStrs = AliasSeq!(OpStrs, op.op);
		}
		static foreach(OpPost op; getUDAs!(sym, OpPost)){
			static if (op.op !is null)
				OpStrs = AliasSeq!(OpStrs, op.op);
		}
		static foreach(OpBin op; getUDAs!(sym, OpBin)){
			static if (op.op !is null)
				OpStrs = AliasSeq!(OpStrs, op.op);
		}
	}
	OpStrs = NoDuplicates!OpStrs;
}

/// Finds precedence of `Op` in `F` functions
private template PrecedOfOp(string Op, F...){
	enum PrecedOfOp = findPreced();
	ubyte findPreced(){
		static foreach (sym; F){
			static foreach(OpPre op; getUDAs!(sym, OpPre)){
				static if (op.op == Op)
					return op.preced;
			}
			static foreach(OpPost op; getUDAs!(sym, OpPost)){
				static if (op.op == Op)
					return op.preced;
			}
			static foreach(OpBin op; getUDAs!(sym, OpBin)){
				static if (op.op == Op)
					return op.preced;
			}
		}
		return ubyte.max;
	}
}

/// Whether precedence is greater than `P`, in `F`, of `Op`
/// Intended for use with Operators
private template IsPrecedOp(ubyte P, F...){
	enum IsPrecedOp(string Op) = PrecedOfOp!(Op, F) > P;
}

/// Whether precedence is greater than or equal to `P`, in `F`, of `Op`
/// Intended for use with Operators
private template IsPrecedOpRA(P, F...){
	enum IsPrecedOpRA(string Op) = PrecedOfOp!(Op, F) >= P;
}

/// Sequence of Grammar Functions in a container (module or anything)
///
/// Templata Params:
/// `E` - Expression Node base type
/// `Err` - Compiler Error Type
/// `M` - container to look for Grammar Functions in
private template GrmrFnsOf(E, Err, alias M){
	alias GrmrFnsOf = AliasSeq!();
	static foreach (string N; __traits(allMembers, M)){
		static if (Instantiate!(IsGrmrFn!(E, Err), __traits(getMember, M, N)))
			GrmrFnsOf = AliasSeq!(GrmrFnsOf, __traits(getMember, M, N));
	}
}

/// Sequence of Post Processing Functions in a container
private template PostFnsOf(alias M){
	alias PostFnsOf = AliasSeq!();
	static foreach (string N; __traits(allMembers, M)){
		static if (IsPostFn!(__traits(getMember, M, N))){
			PostFnsOf = AliasSeq!(PostFnsOf, __traits(getMember, M, N));
		}
	}
}

/// Parser options
public struct ParserOpts{
	/// enable parsing complexity info at compile time
	bool complexityInfo = false;

	this(bool complexityInfo){
		this.complexityInfo = complexityInfo;
	}
}

/// Parser.
///
/// Template params:
/// - `E` - Expression base class
/// - `M` - Module to look for Grammar Functions in
public struct Parser(E, alias M, ParserOpts O = ParserOpts()) if (
		is (E == class) && is (E : ASTNode) && is (M == module)){

	private alias G = GrmrFnsOf!(E, CmpErr, M);

	private alias P = PostFnsOf!M;

	/// Whether a class is inherited from `ASTNode`
	private enum IsA(T) = is (T : ASTNode);

	/// parses prefix operator, limiting itself to Grammar Functions `Fns`
	/// Returns: parsed node, or error
	private static CmpErrVal!E _parseOpPre(Fns...)(ref TokRange toks){
		Tok front = toks.front;
		assert(front.type.get!(TokType.OpPre));
Switch:
		switch (front.token){
			static foreach (string Op; OpStrs!Fns){
				case Op:
					static if (O.complexityInfo && Filter!(HasOp!Op, Fns).length > 1){
						pragma(msg, Op.format!
								("ComplexityInfo: _parseOpPre: Op `%s` has multiple GFns, " ~
								"only first will be called!"));
					}
					static foreach (Fn; Filter!(HasOp!Op, Fns)[0 .. min(1, $)]){
						TokRange branch = toks.save;
						CmpErrVal!(GrmrFnType!Fn) ret = Fn(toks);
						if (!ret.isErr){
							if (ret.val)
								postFn(ret.val, front.line, front.col);
							return CmpErrVal!E(ret.val);
						}
						toks = branch;
						return CmpErrVal!E(ret.err);
					}
					break Switch;
			}
			default:
		}
		// look for Hookless Fns
		alias HklsFns = Filter!(GrmrFnOpIsHkls, Fns);
		static if (O.complexityInfo && HklsFns.length > 1){
			pragma(msg, [staticMap!(StringOf,
						Filter!(IsNotAbstractClass,
							staticMap!(GrmrFnType, HklsFns)))].to!string
					.format!
					("ComplexityInfo: parse: Hookless for %s has multiple GFns, " ~
					 "only first will be called!"));
		}
		static foreach (Fn; HklsFns[0 .. min(1, $)]){
			TokRange branch = toks.save;
			CmpErrVal!(GrmrFnType!Fn) ret = Fn(toks);
			if (!ret.isErr){
				if (ret.val)
					postFn(ret.val, front.line, front.col);
				return CmpErrVal!E(ret.val);
			}
			toks = branch;
			return CmpErrVal!E(ret.err);
		}
		return CmpErrVal!E(errUnxpTok(front, [OpStrs!Fns]));
	}

	/// parses expr, limiting itself to Grammar Functions `Fns`
	/// Returns: parsed node, or error
	private static CmpErrVal!E _parseExpr(Fns...)(ref TokRange toks){
		Tok front = toks.front;
		foreach (Hook H; HooksOf!Fns){
			if (front.type.get!(H.hook)){
				static if (O.complexityInfo && Filter!(HasHook!H, Fns).length > 1){
					pragma(msg, H.stringof.format!
							("ComplexityInfo: _parseExpr: Hook `%s` has multiple GFns, " ~
							"only first will be called!"));
				}
				static foreach (F; Filter!(HasHook!H, Fns)[0 .. min(1, $)]){
					TokRange branch = toks.save;
					CmpErrVal!(GrmrFnType!F) ret = F(toks);
					if (!ret.isErr){
						if (ret.val)
							postFn(ret.val, front.line, front.col);
						return CmpErrVal!E(ret.val);
					}
					toks = branch;
					return CmpErrVal!E(ret.err);
				}
			}
		}
		// look for Hookless Fns
		static foreach (Fn; Filter!(HasNoHook, Fns)[0 .. min(1, $)]){
			TokRange branch = toks.save;
			CmpErrVal!(GrmrFnType!Fn) ret = Fn(toks);
			if (!ret.isErr){
				if (ret.val)
					postFn(ret.val, front.line, front.col);
				return CmpErrVal!E(ret.val);
			}
			toks = branch;
			return CmpErrVal!E(ret.err);
		}
		return CmpErrVal!E(errUnxpTok(front, [OpStrs!Fns]));
	}

	/// parses OpBin or OpPost, limiting to Precedence `P`, and Grammar Functions
	/// `Fns`
	/// Returns: parsed node, or error
	private static CmpErrVal!E _parseBinPost(ubyte P, Fns...)(ref TokRange toks,
			E expr){
		Tok front = toks.front;
		assert(front.type.get!(TokType.OpPost) || front.type.get!(TokType.OpBin));
		static if (O.complexityInfo){
			pragma(msg,
					format!"ComplexityInfo: _parseBinPost: P %d has candidates: %s"(
						P, cast(string[])[Filter!(IsPrecedOp!(P, Fns), OpStrs!Fns)]));
		}
Switch:
		switch (front.token){
			static foreach (string Op; Filter!(IsPrecedOp!(P, Fns), OpStrs!Fns)){
				case Op:
					static if (O.complexityInfo && Filter!(HasOp!Op, Fns).length > 1){
						pragma(msg, Op.format!
								("ComplexityInfo: _parseBinPost: Op `%s` has multiple GFns, " ~
								"only first will be called!"));
					}
					static foreach (F; Filter!(HasOp!Op, Fns)[0 .. min(1, $)]){
						TokRange branch = toks.save;
						CmpErrVal!(GrmrFnType!F) ret = F(toks, expr);
						if (!ret.isErr){
							if (ret.val)
								postFn(ret.val, front.line, front.col);
							return CmpErrVal!E(ret.val);
						}
						toks = branch;
						return CmpErrVal!E(ret.err);
					}
					break Switch;
			}
			default:
				break;
		}
		return CmpErrVal!E(cast(E)null);
		return CmpErrVal!E(errUnxpTok(toks.front, [OpStrs!Fns]));
	}

	private pragma(inline, true) static void postFn(N)(N node,
			size_t line, size_t col){
		node.pos.line = line;
		node.pos.col = col;
		static foreach (F; PostFnsFor!(N, P))
			F(node);
	}

	/// parses TokRange based on hook token, expecting certain types only (`R`).
	/// Will ignore `Expr`, `opPre`, `opPost`, and `opBin`
	/// Returns: parsed node or error
	public static CmpErrVal!(CommonParent!R) parse(R...)(ref TokRange toks) if (
			allSatisfy!(IsA, R)){
		if (toks.empty)
			return CmpErrVal!(CommonParent!R)(errEOF);
		alias Fns = Filter!(GrmrFnIsOfType!R, Filter!(IsNotExpr, G));
		Tok front = toks.front;
		static foreach (Hook H; HooksOf!Fns){
			if (front.type.get!(H.hook)){
				static if (O.complexityInfo && Filter!(HasHook!H, Fns).length > 1){
					pragma(msg, H.stringof.format!
							("ComplexityInfo: parse: Hook `%s` has multiple GFns, " ~
							"only first will be called!"));
				}
				static foreach (F; Filter!(HasHook!H, Fns)[0 .. min(1, $)]){
					TokRange branch = toks.save;
					CmpErrVal!(GrmrFnType!F) ret = F(toks);
					if (!ret.isErr){
						if (ret.val !is null)
							postFn(ret.val, front.line, front.col);
						return CmpErrVal!(CommonParent!R)(ret.val);
					}
					toks = branch;
					return CmpErrVal!(CommonParent!R)(ret.err);
				}
			}
		}
		// try Hookless functions
		alias HklsFns = Filter!(HasNoHook, Fns);
		static if (O.complexityInfo && HklsFns.length > 1){
			pragma(msg, [staticMap!(StringOf,
						Filter!(IsNotAbstractClass,
							staticMap!(GrmrFnType, Filter!(HasNoHook, Fns))))].to!string
					.format!
					("ComplexityInfo: parse: Hookless for %s has multiple GFns, " ~
					 "only first will be called!"));
		}
		static foreach (Fn; Filter!(HasNoHook, Fns)[0 .. min($, 1)]){
			TokRange branch = toks.save;
			CmpErrVal!(GrmrFnType!Fn) ret = Fn(toks);
			if (!ret.isErr){
				if (ret.val !is null)
					postFn(ret.val, front.line, front.col);
				return CmpErrVal!(CommonParent!R)(ret.val);
			}
			toks = branch;
			return CmpErrVal!(CommonParent!R)(ret.err);
		}
		return CmpErrVal!(CommonParent!R)(errUnxpTok(front,
					[staticMap!(MatcherDesc, HooksOf!Fns),
					staticMap!(StringOf,
						Filter!(IsNotAbstractClass,
							staticMap!(GrmrFnType, Filter!(HasNoHook, Fns))))]));
	}

	/// parses an expression, with precedence of `P`, possible return types `R`
	/// Returns: parsed node or error
	public static CmpErrVal!E parseExpr(ubyte P, R...)(ref TokRange toks) if(
				allSatisfy!(IsA, R)){
		if (toks.empty)
			return CmpErrVal!E(errEOF);
		alias Fns = Filter!(GrmrFnIsOfType!R, G);
		alias ExprFns = Filter!(HasAnyUDA!Expr, Fns);
		alias PreFns = Filter!(HasAnyUDA!OpPre, Fns);
		alias BinPostFns = Filter!(HasAnyUDA!(OpBin, OpPost), Fns);
		Tok front = toks.front;

		// try OpPre
		CmpErrVal!E ret = front.type.get!(TokType.OpPre)
			? _parseOpPre!PreFns(toks)
			: _parseExpr!ExprFns(toks);
		if (ret.isErr)
			return ret; // sad :(
		E expr = ret.val;

		while (true){
			// keep consuming OpPost and OpBin as long as precedence allows
			front = toks.front;
			if (!front.type.get!(TokType.OpPost) &&
					!front.type.get!(TokType.OpBin))
				break;
			TokRange branch = toks.save;
			CmpErrVal!E next = _parseBinPost!(P, BinPostFns)(toks, expr);
			if (next.isErr){
				toks = branch;
				return next;
			}
			if (next.val is null){
				toks = branch;
				return CmpErrVal!E(expr);
			}
			expr = next.val;
		}
		return CmpErrVal!E(expr);
	}

	/// Expects one of given TokTypes
	/// does not pop
	/// Returns: true if found, false if not
	public static bool expect(T...)(ref TokRange toks) if (
			allSatisfy!(IsTokType, T)){
		enum Flags!TokType match = Flags!TokType(T);
		if (toks.empty)
			return false;
		// TODO: this can be optimized, & and == true should be 1 step
		// do that optimization in utils package
		return cast(bool)(match & toks.front.type) == true;
	}

	/// Expects one of given TokTypes, pops it if found
	/// Returns: true if found, false if not
	public static bool expectPop(T...)(ref TokRange toks) if (
			allSatisfy!(IsTokType, T)){
		if (!expect!T(toks))
			return false;
		toks.popFront();
		return true;
	}

	/// Parse for types `R` in a sequence. If any one fails, toks range is
	/// reverted back to state before calling this.
	/// Will default to simple `parse`, and if `R[i] : E` is possible, will use
	/// the `parseExpr`, with `precedence=0`
	/// Returns: tuple of parsed nodes, or error
	public static CmpErrVal!(Tuple!R) parseSeq(R...)(ref TokRange toks) if (
			allSatisfy!(IsA, R)){
		TokRange branch = toks.save;
		Tuple!R tup;
		static foreach (i, Rt; R){{
			static if (is (Rt : E)){
				// parse as Expression
				CmpErrVal!E ret = parseExpr!(0, Rt)(toks);
				if (ret.isErr){
					toks = branch;
					return CmpErrVal!(Tuple!R)(ret.err);
				}
				tup[i] = ret.val;
			} else {
				// simple parse
				CmpErrVal!R ret = parse!Rt(toks);
				if (ret.isErr){
					toks = branch;
					return CmpErrVal!(Tuple!R)(ret.err);
				}
				ret[i] = ret.val;
			}
		}}
		return CmpErrVal!(Tuple!R)(tup);
	}

	/// Precedence of a Prefix operator
	public enum PrecedOfPre(string Op) = PrecedOfOp!(Op,
			Filter!(HasAnyUDA!OpPre, G));
	/// Precedence of a Binary operator
	public enum PrecedOfBin(string Op) = PrecedOfOp!(Op,
			Filter!(HasAnyUDA!OpBin, G));
	/// Precedence of a Postfix operator
	public enum PrecedOfPost(string Op) = PrecedOfOp!(Op,
			Filter!(HasAnyUDA!OpPost, G));
}
