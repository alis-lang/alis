/++
Expression (RExpr) Compile Time Evaluation
+/
module alis.compiler.semantic.eval;

import alis.common,
			 alis.compiler.common,
			 alis.compiler.semantic.common,
			 alis.compiler.semantic.error,
			 alis.compiler.semantic.types,
			 alis.compiler.ast,
			 alis.compiler.ast.iter,
			 alis.compiler.ast.rst;

import alis.compiler.semantic.expr : resolve;

import meta;

import std.algorithm,
			 std.range,
			 std.format;

debug import std.stdio,
			std.conv;

private alias It = RtL!(mixin(__MODULE__), 0);

struct St{
	/// errors
	SmErr[] errs;
	/// root STab
	STab stabR;
	/// local STab
	STab stab;
	/// context ctx
	IdentU[] ctx;
	/// Result
	AValCT res;
}

@ItFn @ITL(0) {
	void literalIter(RLiteralExpr node, ref St st){
		st.res = node.val.AValCT;
	}

	void avalCtIter(RAValCTExpr node, ref St st){
		st.res = node.res;
	}

	void exprIter(RExpr node, ref St st){
		st.errs ~= errUnsup(node);
	}
}

/// Evaluates an `expr`. Resulting AVAlCT can be any of 3 `AValCT.Type`,
/// in case something is suitable as `Type.Symbol` and something else,
/// `Type.Symbol` will be preferred.
/// Params:
/// - `expr` - The expression to resolve
/// - `stab` - The root level Symbol Table
/// - `ctx` - Context where the `expr` occurs
/// Returns: AValCT, or SmErr[]
package SmErrsVal!AValCT eval(RExpr expr, STab stabR, IdentU[] ctx){
	St st;
	st.stabR = stabR;
	st.stab = stabR.findSt(ctx, ctx);
	st.ctx = ctx.dup;
	It.exec(expr, st);
	if (st.errs.length)
		return SmErrsVal!AValCT(st.errs);
	return SmErrsVal!AValCT(st.res);
}

/// ditto
package SmErrsVal!AValCT eval(Expression expr, STab stab, IdentU[] ctx,
		void[0][ASymbol*] dep, RFn[string] fns, AValCT[] params = null){
	SmErrsVal!RExpr resolved = resolve(expr, stab, ctx, dep, fns, params);
	if (resolved.isErr)
		return SmErrsVal!AValCT(resolved.err);
	return eval(resolved.val, stab, ctx);
}


/// Evaluates an RExpr expecting a value. See `eval`
/// Returns: AValCT with Type.Literal, or SmErr[]
package SmErrsVal!AVal eval4Val(RExpr expr, STab stab, IdentU[] ctx){
	SmErrsVal!AValCT res = eval(expr, stab, ctx);
	if (res.isErr)
		return res.err.SmErrsVal!AVal;
	if (res.val.type != AValCT.Type.Literal)
		return SmErrsVal!AVal([errExprValExpected(expr.pos)]);
	return res.val.val.SmErrsVal!AVal;
}

/// ditto
package SmErrsVal!AVal eval4Val(Expression expr, STab stab, IdentU[] ctx,
		void[0][ASymbol*] dep, RFn[string] fns, AValCT[] params = null){
	SmErrsVal!RExpr resolved = resolve(expr, stab, ctx, dep, fns, params);
	if (resolved.isErr)
		return SmErrsVal!AVal(resolved.err);
	return eval4Val(resolved.val, stab, ctx);
}

/// Evaluates an RExpr expecting a type. See `eval`
/// Returns: ADataType or SmErr[]
package SmErrsVal!ADataType eval4Type(RExpr expr, STab stab, IdentU[] ctx){
	SmErrsVal!AValCT ret = eval(expr, stab, ctx);
	if (ret.isErr)
		return SmErrsVal!ADataType(ret.err);
	if (!ret.val.asType.isVal ||
			(ret.val.type != AValCT.Type.Type &&
			 ret.val.type != AValCT.Type.Symbol))
		return SmErrsVal!ADataType([errExprTypeExpected(expr.pos)]);
	return SmErrsVal!ADataType(ret.val.asType.val);
}

/// ditto
package SmErrsVal!ADataType eval4Type(Expression expr, STab stab, IdentU[] ctx,
		void[0][ASymbol*] dep, RFn[string] fns, AValCT[] params = null){
	SmErrsVal!RExpr resolved = resolve(expr, stab, ctx, dep, fns, params);
	if (resolved.isErr)
		return SmErrsVal!ADataType(resolved.err);
	return eval4Type(resolved.val, stab, ctx);
}

/// Evaluates an RExpr expecting a symbol. See `eval`
/// Returns: ASymbol* or SmErr[]
package SmErrsVal!(ASymbol*) eval4Sym(RExpr expr, STab stab, IdentU[] ctx){
	SmErrsVal!AValCT ret = eval(expr, stab, ctx);
	if (ret.isErr)
		return SmErrsVal!(ASymbol*)(ret.err);
	if (ret.val.type != AValCT.Type.Symbol)
		return SmErrsVal!(ASymbol*)([errExprSymExpected(expr.pos)]);
	return SmErrsVal!(ASymbol*)(ret.val.symS);
}

/// ditto
package SmErrsVal!(ASymbol*) eval4Sym(Expression expr, STab stab, IdentU[] ctx,
		void[0][ASymbol*] dep, RFn[string] fns, AValCT[] params = null){
	SmErrsVal!RExpr resolved = resolve(expr, stab, ctx, dep, fns, params);
	if (resolved.isErr)
		return SmErrsVal!(ASymbol*)(resolved.err);
	return eval4Sym(resolved.val, stab, ctx);
}
