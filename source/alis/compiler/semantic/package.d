/++
Semantic Analysis Package
+/
module alis.compiler.semantic;

import alis.common,
			 alis.compiler.common,
			 alis.compiler.semantic.common,
			 alis.compiler.semantic.error,
			 alis.compiler.semantic.eval,
			 alis.compiler.semantic.expr,
			 alis.compiler.semantic.sym0,
			 alis.compiler.semantic.sym1,
			 alis.compiler.semantic.types,
			 alis.compiler.ast.rst;

/// Builds symbol table for ASTNode.
/// Returns: Symbol Table, mapping of AFn.uid to RFn, and test expressions, or
/// SmErr[]
public SmErrsVal!S1R stabOf(ASTNode node, STab stabR = null,
		IdentU[] ctx = null){
	SmErrsVal!S0R s0Res = node.stab0Of(stabR, ctx);
	if (s0Res.isErr)
		return SmErrsVal!S1R(s0Res.err);
	if (stabR is null)
		stabR = s0Res.val.stab;
	return node.stab1Of(stabR, s0Res.val.sMap, null, ctx);
}

/// Fully converts an ASymbol. Intended to be called in middle of `stabOf`, to
/// build an ASymbol out-of-normal-order.
/// Returns: SmErr[], which will be zero length or null if success
public SmErr[] symDo(ASymbol* sym, STab stabR,
		void[0][ASymbol*] dep, RFn[string] fns){
	SmErrsVal!S1R ret = alis.compiler.semantic.sym1.symDo(sym, stabR, dep, fns);
	if (ret.isErr)
		return ret.err;
	return null;
}
