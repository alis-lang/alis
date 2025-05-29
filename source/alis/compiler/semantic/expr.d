/++
Expression Resolution (Expression -> RExpr conversion)
+/
module alis.compiler.semantic.expr;

import alis.common,
			 alis.compiler.common,
			 alis.compiler.semantic.common,
			 alis.compiler.semantic.error,
			 alis.compiler.semantic.sym0,
			 alis.compiler.ast,
			 alis.compiler.ast.iter,
			 alis.compiler.rst;

import meta;

private struct St{
	/// errors
	SmErr[] errs;
	/// main STab, for lookups
	STab stabMain;
	/// local STab
	STab stab;
	/// context
	IdentU[] ctx;
	/// symbols dependent on current call
	void[0][ASymbol*] dep;
	/// resulting expression
	RExpr res;
	/// parameter types if resuslting expression is expected to be callable
	AValCT[] params;
}

private alias It = ItL!(mixin(__MODULE__), 0);

@ItFn @ITL(0){
	void identIter(IdentExpr node, ref St st){

	}

	void callIter(OpCallExpr node, ref St st){
		if (auto sub = cast(IntrinsicExpr)node.callee){
			//intrExpr(sub, node.params, st);
			return;
		}
	}

	void dotIter(OpDotBin node, ref St st){
		if (auto sub = cast(IntrinsicExpr)node.rhs){
			Expression[] params;
			if (auto cExpr = cast(CommaExpr)node.lhs){
				params = cExpr.exprs;
			} else {
				params = [node.lhs];
			}
			//intrExpr(sub, params, st);
		}
	}
}

/// Resolves Expression to RExpr
/// Params:
/// - `expr` - The expression to resolve
/// - `stab` - The root level Symbol Table
/// - `ctx` - Context where the `expr` occurs
/// - `params` - Parameters for `expr`, if any
/// Returns: RExpr or SmErr[]
pragma(inline, true)
package SmErrsVal!RExpr resolve(Expression expr, STab stab, IdentU[] ctx,
		void[0][ASymbol*] dep, AValCT[] params = null){
	St st;
	st.dep = dep;
	st.ctx = ctx.dup;
	st.stabMain = stab;
	st.stab = stab;
	st.params = params.dup;
	It.exec(expr, st);
	if (st.errs.length)
		return SmErrsVal!RExpr(st.errs);
	return SmErrsVal!RExpr(st.res);
}
