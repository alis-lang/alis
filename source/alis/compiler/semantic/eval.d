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
pragma(inline, true)
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
