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

/// state passed to `ExprTranslator` function
package struct IntrSt{
	/// intrinsic name
	string name;
	/// location
	Location pos;
	/// root level symbol table
	STab stabR;
	/// context
	IdentU[] ctx;
	/// dependent symbols
	void[0][ASymbol*] dep;
	/// functions
	RFn[string] fns;
	/// params passed to intrinsic
	AValCT[] params;
}

/// Whether a function is a callabality checker
private template IsCallabilityChecker(alias Fn){
	enum IsCallabilityChecker =
		isCallable!Fn &&
		hasUDA!(Fn, CallabilityChecker) &&
		hasUDA!(Fn, Intr) && Parameters!Fn.length == 2 &&
		is (AValCT[] : Parameters!Fn[0]) &&
		is (IdentU[] : Parameters!Fn[1]) &&
		is (ReturnType!Fn : size_t);
}

/// Whether a function is a ExprTranslator
private template IsExprTranslator(alias Fn){
	enum IsExprTranslator =
		isCallable!Fn &&
		hasUDA!(Fn, ExprTranslator) &&
		hasUDA!(Fn, Intr) && Parameters!Fn.length == 1 &&
		is (IntrSt : Parameters!Fn[0]) &&
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
public size_t callabilityOf(F...)(string intrN, AValCT[] params, IdentU[] ctx)
		if (allSatisfy!(IsCallabilityChecker, F)){
	switch (intrN){
		static foreach (Fn; F){
			static foreach (Intr i; getUDAs!(Fn, Intr)){
				case i.name:
			}
			return Fn(params, ctx) == true ? 0 : size_t.max;
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
			return Fn(IntrSt(name, pos, stabR, ctx, dep, fns, params));
		}
	default:
		return SmErrsVal!RExpr([errIntrUnk(pos, name)]);
	}
}
