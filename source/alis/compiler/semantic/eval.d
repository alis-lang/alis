/++
Expression (RExpr) Compile Time Evaluation
+/
module alis.compiler.semantic.eval;

import alis.common,
			 alis.compiler.common,
			 alis.compiler.semantic.common,
			 alis.compiler.semantic.error,
			 alis.compiler.ast,
			 alis.compiler.ast.iter,
			 alis.compiler.rst;

import meta;

private alias It = ItL!(mixin(__MODULE__), 0);

struct St{
	/// errors
	SmErr[] errs;
	/// main STab, for lookups
	STab stabMain;
	/// local STab
	STab stab;
	/// context ctx
	IdentU[] ctx;
	/// Result
	AValCT res;
}

@ItFn @ITL(0) {
	@ItFn void rIdentIter(RIdentExpr expr, ref St st){
		st.errs ~= errUnsup(expr);
	}
}

/// Evaluates an RExpr
/// Params:
/// - `expr` - The expression to resolve
/// - `stab` - The root level Symbol Table
/// - `ctx` - Context where the `expr` occurs
/// Returns: AValCT, or SmErr[]
package SmErrsVal!AValCT eval(RExpr expr, STab stab, IdentU[] ctx){
	St st;
	st.stab = stab;
	st.stabMain = stab;
	st.ctx = ctx.dup;
	It.exec(expr, st);
	if (st.errs.length)
		return SmErrsVal!AValCT(st.errs);
	return SmErrsVal!AValCT(st.res);
}

/// Evaluates an RExpr expecting a value. See `eval`
/// Returns: AValCT with Type.Literal, or SmErr[]
package SmErrsVal!AValCT eval4Val(RExpr expr, STab stab, IdentU[] ctx){
	SmErrsVal!AValCT ret = eval(expr, stab, ctx);
	if (ret.isErr)
		return ret;
	if (ret.val.type != AValCT.Type.Literal)
		return SmErrsVal!AValCT([errExprValExpected(expr)]);
	return ret;
}

/// Evaluates an RExpr expecting a type. See `eval`
/// Returns: AValCT with Type.Type, or SmErr[]
package SmErrsVal!AValCT eval4Type(RExpr expr, STab stab, IdentU[] ctx){
	SmErrsVal!AValCT ret = eval(expr, stab, ctx);
	if (ret.isErr)
		return ret;
	if (ret.val.type != AValCT.Type.Type)
		return SmErrsVal!AValCT([errExprTypeExpected(expr)]);
	return ret;
}

/// Evaluates an RExpr expecting a symbol. See `eval`
/// Returns: AValCT with Type.Type, or SmErr[]
package SmErrsVal!AValCT eval4Sym(RExpr expr, STab stab, IdentU[] ctx){
	SmErrsVal!AValCT ret = eval(expr, stab, ctx);
	if (ret.isErr)
		return ret;
	if (ret.val.type != AValCT.Type.Symbol)
		return SmErrsVal!AValCT([errExprSymExpected(expr)]);
	return ret;
}

/// Evaluates an Expression
/// Params:
/// - `expr` - The expression to resolve
/// - `stab` - The root level Symbol Table
/// - `ctx` - Context where the `expr` occurs
/// - `params` - Parameters for `expr` if any
/// Returns: AValCT or SmErr[]
package SmErrsVal!AValCT eval(Expression expr, STab stab, IdentU[] ctx,
		AValCT[] params = null){
	import alis.compiler.semantic.expr : resolve;
	SmErrsVal!RExpr resolved = resolve(expr, stab, ctx, params);
	if (resolved.isErr)
		return SmErrsVal!AValCT(resolved.err);
	return eval(resolved.val, stab, ctx);
}

/// Evaluates an Expression, expecting a value. See `eval`
/// Returns: AValCT with Type.Literal, or SmErr[]
package SmErrsVal!AValCT eval4Val(Expression expr, STab stab, IdentU[] ctx,
		AValCT[] params = null){
	SmErrsVal!AValCT ret = eval(expr, stab, ctx, params);
	if (ret.isErr)
		return ret;
	if (ret.val.type != AValCT.Type.Literal)
		return SmErrsVal!AValCT([errExprValExpected(expr)]);
	return ret;
}

/// Evaluates an Expression, expecting a type. See `eval`
/// Returns: AValCT with Type.Literal, or SmErr[]
package SmErrsVal!AValCT eval4Type(Expression expr, STab stab, IdentU[] ctx,
		AValCT[] params = null){
	SmErrsVal!AValCT ret = eval(expr, stab, ctx, params);
	if (ret.isErr)
		return ret;
	if (ret.val.type != AValCT.Type.Type)
		return SmErrsVal!AValCT([errExprTypeExpected(expr)]);
	return ret;
}

/// Evaluates an Expression, expecting a symbol. See `eval`
/// Returns: AValCT with Type.Literal, or SmErr[]
package SmErrsVal!AValCT eval4Sym(Expression expr, STab stab, IdentU[] ctx,
		AValCT[] params = null){
	SmErrsVal!AValCT ret = eval(expr, stab, ctx, params);
	if (ret.isErr)
		return ret;
	if (ret.val.type != AValCT.Type.Symbol)
		return SmErrsVal!AValCT([errExprSymExpected(expr)]);
	return ret;
}
