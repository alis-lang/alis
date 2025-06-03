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

import std.algorithm,
			 std.array,
			 std.range;

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
		st.errs ~= errUnsup(node); // TODO: implement
	}
	void blockExprIter(BlockExpr node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}
	void intrinsicExprIter(IntrinsicExpr node, ref St st){
		RIntrinsicExpr r = new RIntrinsicExpr;
		r.pos = node.pos;
		r.name = node.name;
		st.res = r;
	}

	void commaExprIter(CommaExpr node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}
	void structAnonIter(StructAnon node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}
	void unionAnonIter(UnionAnon node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}
	void fnAnonExprIter(FnAnonExpr node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}
	void structLiteralExprIter(StructLiteralExpr node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}
	void boolLiteralExprIter(BoolLiteralExpr node, ref St st){
		RLiteralExpr r = new RLiteralExpr;
		r.pos = node.pos;
		r.type = ADataType.ofBool;
		r.value = node.val.asBytes;
		st.res = r;
	}
	void literalIntExprIter(LiteralIntExpr node, ref St st){
		RLiteralExpr r = new RLiteralExpr;
		r.pos = node.pos;
		r.type = ADataType.ofInt;
		r.value = node.val.asBytes;
		st.res = r;
	}
	void literalFloatExprIter(LiteralFloatExpr node, ref St st){
		RLiteralExpr r = new RLiteralExpr;
		r.pos = node.pos;
		r.type = ADataType.ofFloat;
		r.value = node.val.asBytes;
		st.res = r;
	}
	void literalStringExprIter(LiteralStringExpr node, ref St st){
		RLiteralExpr r = new RLiteralExpr;
		r.pos = node.pos;
		r.type = ADataType.ofString;
		r.value = cast(ubyte[])(node.val.dup);
		st.res = r;
	}
	void literalCharExprIter(LiteralCharExpr node, ref St st){
		RLiteralExpr r = new RLiteralExpr;
		r.pos = node.pos;
		r.type = ADataType.ofChar(8);
		r.value = [cast(ubyte)node.val];
		st.res = r;
	}
	void literalArrayExprIter(LiteralArrayExpr node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}
	void autoExprIter(AutoExpr node, ref St st){
		st.errs ~= errUnsup(node); // what even is `auto` doing here
	}
	void thisExprIter(ThisExpr node, ref St st){
		st.errs ~= errUnsup(node); // TODO: what to do with `this`
	}
	void intExprIter(IntExpr node, ref St st){
		RDTypeExpr r = new RDTypeExpr;
		r.pos = node.pos;
		r.type = ADataType.ofInt;
		st.res = r;
	}
	void uIntExprIter(UIntExpr node, ref St st){
		RDTypeExpr r = new RDTypeExpr;
		r.pos = node.pos;
		r.type = ADataType.ofUInt;
		st.res = r;
	}
	void floatExprIter(FloatExpr node, ref St st){
		RDTypeExpr r = new RDTypeExpr;
		r.pos = node.pos;
		r.type = ADataType.ofFloat;
		st.res = r;
	}
	void charExprIter(CharExpr node, ref St st){
		RDTypeExpr r = new RDTypeExpr;
		r.pos = node.pos;
		r.type = ADataType.ofChar(1);
		st.res = r;
	}
	void stringExprIter(StringExpr node, ref St st){
		RDTypeExpr r = new RDTypeExpr;
		r.pos = node.pos;
		r.type = ADataType.ofString;
		st.res = r;
	}
	void boolExprIter(BoolExpr node, ref St st){
		RDTypeExpr r = new RDTypeExpr;
		r.pos = node.pos;
		r.type = ADataType.ofBool;
		st.res = r;
	}
	void opPostExprOverridableIter(OpPostExprOverridable node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}
	void opPreExprOverridableIter(OpPreExprOverridable node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}
	void opBinExprOverridableIter(OpBinExprOverridable node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}
	void opCallExprIter(OpCallExpr node, ref St st){
		if (IntrinsicExpr intr = cast(IntrinsicExpr)node.callee){
			RIntrinsicCallExpr r = new RIntrinsicCallExpr;
			r.pos = node.pos;
			r.name = intr.name;
			r.params = node.params
				.map!(p => resolve(p, st.stabR, st.ctx, st.dep))
				.tee!((SmErrsVal!RExpr p){
						if (p.isErr)
							st.errs ~= p.err;
						})
				.filter!(p => !p.isErr)
				.map!(p => p.val).array;
			st.res = r;
			return;
		}
		st.errs ~= errUnsup(node); // TODO: implement
	}
	void opIndexExprIter(OpIndexExpr node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}
	void opAssignBinIter(OpAssignBin node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}
	void opAssignRefBinIter(OpAssignRefBin node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}
	void opRefPostIter(OpRefPost node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}
	void opDotsPostIter(OpDotsPost node, ref St st){
		st.errs ~= errUnsup(node);
	}
	void opIsPreIter(OpIsPre node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}
	void opNotIsPreIter(OpNotIsPre node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}
	void opConstPreIter(OpConstPre node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}
	void opRefPreIter(OpRefPre node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}
	void opTagPreIter(OpTagPre node, ref St st){
		st.errs ~= errUnsup(node.pos, "`#` operator in expressions");
	}
	void opArrowBinIter(OpArrowBin node, ref St st){
		st.errs ~= errUnsup(node.pos, "vtable");
	}
	void opCommaBinIter(OpCommaBin node, ref St st){
		st.errs ~= errUnxp(node.pos, "OpCommaBin should not have happened");
	}
	void opDotBinIter(OpDotBin node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}
	void opColonBinIter(OpColonBin node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}
	void opIsBinIter(OpIsBin node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}
	void opNotIsBinIter(OpNotIsBin node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}
	void opNotPostIter(OpNotPost node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}
	void opQPostIter(OpQPost node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}
	void opNotNotBinIter(OpNotNotBin node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}
	void opQQBinIter(OpQQBin node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
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
	if (st.res is null)
		return SmErrsVal!RExpr([errUnxp(expr.pos, "resolve expr -> null")]);
	return SmErrsVal!RExpr(st.res);
}
