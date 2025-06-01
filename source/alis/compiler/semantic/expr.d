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
	/// root stab
	STab stabR;
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
	void identExprIter(IdentExpr node, ref St st){
		// TODO: implement
	}
	void blockExprIter(BlockExpr node, ref St st){
		// TODO: implement
	}
	void intrinsicExprIter(IntrinsicExpr node, ref St st){
		// TODO: implement
	}
	void commaExprIter(CommaExpr node, ref St st){
		// TODO: implement
	}
	void structAnonIter(StructAnon node, ref St st){
		// TODO: implement
	}
	void unionAnonIter(UnionAnon node, ref St st){
		// TODO: implement
	}
	void fnAnonExprIter(FnAnonExpr node, ref St st){
		// TODO: implement
	}
	void structLiteralExprIter(StructLiteralExpr node, ref St st){
		// TODO: implement
	}
	void boolLiteralExprIter(BoolLiteralExpr node, ref St st){
		// TODO: implement
	}
	void literalIntExprIter(LiteralIntExpr node, ref St st){
		// TODO: implement
	}
	void literalFloatExprIter(LiteralFloatExpr node, ref St st){
		// TODO: implement
	}
	void literalStringExprIter(LiteralStringExpr node, ref St st){
		// TODO: implement
	}
	void literalCharExprIter(LiteralCharExpr node, ref St st){
		// TODO: implement
	}
	void literalArrayExprIter(LiteralArrayExpr node, ref St st){
		// TODO: implement
	}
	void autoExprIter(AutoExpr node, ref St st){
		// TODO: implement
	}
	void thisExprIter(ThisExpr node, ref St st){
		// TODO: implement
	}
	void intExprIter(IntExpr node, ref St st){
		// TODO: implement
	}
	void uIntExprIter(UIntExpr node, ref St st){
		// TODO: implement
	}
	void floatExprIter(FloatExpr node, ref St st){
		// TODO: implement
	}
	void charExprIter(CharExpr node, ref St st){
		// TODO: implement
	}
	void stringExprIter(StringExpr node, ref St st){
		// TODO: implement
	}
	void boolExprIter(BoolExpr node, ref St st){
		// TODO: implement
	}
	void opPostExprIter(OpPostExpr node, ref St st){
		// TODO: implement
	}
	void opPostExprOverridableIter(OpPostExprOverridable node, ref St st){
		// TODO: implement
	}
	void opPreExprIter(OpPreExpr node, ref St st){
		// TODO: implement
	}
	void opPreExprOverridableIter(OpPreExprOverridable node, ref St st){
		// TODO: implement
	}
	void opBinExprIter(OpBinExpr node, ref St st){
		// TODO: implement
	}
	void opBinExprOverridableIter(OpBinExprOverridable node, ref St st){
		// TODO: implement
	}
	void opCallExprIter(OpCallExpr node, ref St st){
		// TODO: implement
	}
	void opIndexExprIter(OpIndexExpr node, ref St st){
		// TODO: implement
	}
	void opAssignBinIter(OpAssignBin node, ref St st){
		// TODO: implement
	}
	void opAssignRefBinIter(OpAssignRefBin node, ref St st){
		// TODO: implement
	}
	void opRefPostIter(OpRefPost node, ref St st){
		// TODO: implement
	}
	void opDotsPostIter(OpDotsPost node, ref St st){
		// TODO: implement
	}
	void opIsPreIter(OpIsPre node, ref St st){
		// TODO: implement
	}
	void opNotIsPreIter(OpNotIsPre node, ref St st){
		// TODO: implement
	}
	void opConstPreIter(OpConstPre node, ref St st){
		// TODO: implement
	}
	void opRefPreIter(OpRefPre node, ref St st){
		// TODO: implement
	}
	void opTagPreIter(OpTagPre node, ref St st){
		st.errs ~= errUnsup(node.pos, "`#` operator in expressions");
	}
	void opArrowBinIter(OpArrowBin node, ref St st){
		// TODO: implement
	}
	void opCommaBinIter(OpCommaBin node, ref St st){
		// TODO: implement
	}
	void opDotBinIter(OpDotBin node, ref St st){
		// TODO: implement
	}
	void opColonBinIter(OpColonBin node, ref St st){
		// TODO: implement
	}
	void opIsBinIter(OpIsBin node, ref St st){
		// TODO: implement
	}
	void opNotIsBinIter(OpNotIsBin node, ref St st){
		// TODO: implement
	}
	void opNotPostIter(OpNotPost node, ref St st){
		// TODO: implement
	}
	void opQPostIter(OpQPost node, ref St st){
		// TODO: implement
	}
	void opNotNotBinIter(OpNotNotBin node, ref St st){
		// TODO: implement
	}
	void opQQBinIter(OpQQBin node, ref St st){
		// TODO: implement
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
package SmErrsVal!RExpr resolve(Expression expr, STab stabR, IdentU[] ctx,
		void[0][ASymbol*] dep, AValCT[] params = null){
	St st;
	st.dep = dep;
	st.ctx = ctx.dup;
	st.stabR = stabR;
	st.stab = stabR.findSt(ctx, ctx);
	st.params = params.dup;
	It.exec(expr, st);
	if (st.errs.length)
		return SmErrsVal!RExpr(st.errs);
	return SmErrsVal!RExpr(st.res);
}
