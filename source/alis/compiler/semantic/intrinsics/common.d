/++
Intrinsics common stuff
+/
module alis.compiler.semantic.intrinsics.common;

import alis.common,
			 alis.compiler.common,
			 alis.compiler.semantic.common,
			 alis.compiler.semantic.error,
			 alis.compiler.ast,
			 alis.compiler.ast.rst;

import std.meta,
			 std.traits;

debug import std.stdio;

import meta;

/// tag as callabality check
/// parameters must be:
/// - `AValCT[]` OR `RExpr[]` for params
package enum CallabilityChecker;

/// tag as AST -> RST translater
/// parameters must be:
/// - `string` name
/// - `Location`
/// - `STab`
/// - `IdentU[]` ctx
/// - `void[0][ASymbol*]` dep
/// - `RFn[string]` fns
/// - `AValCT[]` params
/// must return `SmErrsVal!RExpr`
package enum ExprTranslator;

/// tag with intrinsic name
package struct Intr{
	@disable this();
public:
	string name;
	this(string name){
		this.name = name;
	}
}

/// Whether a function is a callabality checker
private template IsCallabilityChecker(alias Fn){
	enum IsCallabilityChecker =
		isCallable!Fn &&
		hasUDA!(Fn, CallabilityChecker) &&
		hasUDA!(Fn, Intr) && Parameters!Fn.length == 1 &&
		(
		 is (AValCT[] : Parameters!Fn[0]) ||
		 is (RExpr[] : Parameters!Fn[0])
		) &&
		is (ReturnType!Fn : size_t);
}

/// Whether a function is a ExprTranslator
private template IsExprTranslator(alias Fn){
	enum IsExprTranslator =
		isCallable!Fn &&
		hasUDA!(Fn, ExprTranslator) &&
		hasUDA!(Fn, Intr) && Parameters!Fn.length == 7 &&
		(
		 is (string : Parameters!Fn[0]) &&
		 is (Location : Parameters!Fn[1]) &&
		 is (STab : Parameters!Fn[2]) &&
		 is (IdentU[] : Parameters!Fn[3]) &&
		 is (void[0][ASymbol*] : Parameters!Fn[4]) &&
		 is (RFn[string] : Parameters!Fn[5]) &&
		 is (AValCT[] : Parameters!Fn[6])
		) &&
		is (ReturnType!Fn : SmErrsVal!RExpr);
}

/// CallabilityCheckers found in a module
package template CallabilityCheckersOf(alias M){
	alias CallabilityCheckersOf = AliasSeq!();
	static foreach (string N; __traits(allMembers, M)){
		static if (AliasSeq!(__traits(getMember, M, N)).length == 1 &&
				IsCallabilityChecker!(__traits(getMember, M, N))){
			CallabilityCheckersOf = AliasSeq!(CallabilityCheckersOf,
					__traits(getMember, M, N));
		}
	}
}

/// ExprTranslators found in a module
package template ExprTranslatorsOf(alias M){
	alias ExprTranslatorsOf = AliasSeq!();
	static foreach (string N; __traits(allMembers, M)){
		static if (AliasSeq!(__traits(getMember, M, N)).length == 1 &&
				IsExprTranslator!(__traits(getMember, M, N))){
			ExprTranslatorsOf = AliasSeq!(ExprTranslatorsOf,
					__traits(getMember, M, N));
		}
	}
}

/// calculates callability score.
/// `size_t.max` -> not callable
/// `0` -> highest callability
/// Returns: callability score
public size_t callabilityOf(F...)(string intrN, AValCT[] params) if (
		allSatisfy!(IsCallabilityChecker, F)){
	switch (intrN){
		static foreach (Fn; F){
			static foreach (Intr i; getUDAs!(Fn, Intr)){
				case i.name:
			}
			return Fn(params) == true ? 0 : size_t.max;
		}
	default:
		return size_t.max;
	}
}

/// resolves an intrinsic, provided its params (if any)
public SmErrsVal!RExpr resolveIntrN(F...)(string name, Location pos,
		AValCT[] params, STab stabR, IdentU[] ctx, void[0][ASymbol*] dep,
		RFn[string] fns) if (allSatisfy!(IsExprTranslator, F)){
	switch (name){
		static foreach (Fn; F){
			static foreach (Intr i; getUDAs!(Fn, Intr)){
				case i.name:
			}
			return Fn(name, pos, stabR, ctx, dep, fns, params);
		}
	default:
		return SmErrsVal!RExpr([errIntrUnk(pos, name)]);
	}
}
