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

import alis.compiler.semantic.expr : resolve;

import meta;

private alias It = ItL!(mixin(__MODULE__), 0);

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
	@ItFn void rIdentIter(RIdentExpr expr, ref St st){
		st.errs ~= errUnsup(expr);
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
	if (st.errs.length){
		debug{
			import std.stdio;
			stderr.writefln!"STUB: eval(RExpr) errored %s, returning 5.int"(st.errs);
			return SmErrsVal!AValCT(AValCT(ADataType.ofInt, 5.asBytes));
		}
		return SmErrsVal!AValCT(st.errs);
	}
	return SmErrsVal!AValCT(st.res);
}

/// ditto
package SmErrsVal!AValCT eval(Expression expr, STab stab, IdentU[] ctx,
		void[0][ASymbol*] dep, AValCT[] params = null){
	SmErrsVal!RExpr resolved = resolve(expr, stab, ctx, dep, params);
	if (resolved.isErr){
		debug{
			import std.stdio;
			stderr.writefln!"STUB: eval(Expression) errored %s, returning 5.int"(
					resolved.err);
			return SmErrsVal!AValCT(AValCT(ADataType.ofInt, 5.asBytes));
		}
		return SmErrsVal!AValCT(resolved.err);
	}
	return eval(resolved.val, stab, ctx);
}

/// Evaluates an RExpr expecting a value. See `eval`
/// Returns: AValCT with Type.Literal, or SmErr[]
package SmErrsVal!AValCT eval4Val(RExpr expr, STab stab, IdentU[] ctx){
	SmErrsVal!AValCT ret = eval(expr, stab, ctx);
	if (ret.isErr){
		debug{
			import std.stdio;
			stderr.writefln!"STUB: eval4Val errored %s, returning 5.int"(ret.err);
			return SmErrsVal!AValCT(AValCT(ADataType.ofInt, 5.asBytes));
		}
		return ret;
	}
	if (ret.val.type != AValCT.Type.Literal)
		return SmErrsVal!AValCT([errExprValExpected(expr)]);
	return ret;
}

/// ditto
package SmErrsVal!AValCT eval4Val(Expression expr, STab stab, IdentU[] ctx,
		void[0][ASymbol*] dep, AValCT[] params = null){
	SmErrsVal!RExpr resolved = resolve(expr, stab, ctx, dep, params);
	if (resolved.isErr){
		debug{
			import std.stdio;
			stderr.writefln!"STUB: eval4Val errored %s, returning 5.int"(
					resolved.err);
			return SmErrsVal!AValCT(AValCT(ADataType.ofInt, 5.asBytes));
		}
		return SmErrsVal!AValCT(resolved.err);
	}
	return eval4Val(resolved.val, stab, ctx);
}

/// Evaluates an RExpr expecting a type. See `eval`
/// Returns: ADataType or SmErr[]
package SmErrsVal!ADataType eval4Type(RExpr expr, STab stab, IdentU[] ctx){
	SmErrsVal!AValCT ret = eval(expr, stab, ctx);
	if (ret.isErr){
		debug{
			import std.stdio;
			stderr.writefln!"STUB: eval4Type errored %s, returning $int(64)"(ret.err);
			return SmErrsVal!ADataType(ADataType.ofInt);
		}
		return SmErrsVal!ADataType(ret.err);
	}
	if (ret.val.type != AValCT.Type.Type)
		return SmErrsVal!ADataType([errExprTypeExpected(expr)]);
	return SmErrsVal!ADataType(ret.val.typeT);
}

/// ditto
package SmErrsVal!ADataType eval4Type(Expression expr, STab stab, IdentU[] ctx,
		void[0][ASymbol*] dep, AValCT[] params = null){
	SmErrsVal!RExpr resolved = resolve(expr, stab, ctx, dep, params);
	if (resolved.isErr){
		debug{
			import std.stdio;
			stderr.writefln!"STUB: eval4Type errored %s, returning $int(64)"(
					resolved.err);
			return SmErrsVal!ADataType(ADataType.ofInt);
		}
		return SmErrsVal!ADataType(resolved.err);
	}
	return eval4Type(resolved.val, stab, ctx);
}

/// Evaluates an RExpr expecting a symbol. See `eval`
/// Returns: ASymbol* or SmErr[]
package SmErrsVal!(ASymbol*) eval4Sym(RExpr expr, STab stab, IdentU[] ctx){
	SmErrsVal!AValCT ret = eval(expr, stab, ctx);
	if (ret.isErr)
		return SmErrsVal!(ASymbol*)(ret.err);
	if (ret.val.type != AValCT.Type.Symbol)
		return SmErrsVal!(ASymbol*)([errExprSymExpected(expr)]);
	return SmErrsVal!(ASymbol*)(ret.val.symS);
}

/// ditto
package SmErrsVal!(ASymbol*) eval4Sym(Expression expr, STab stab, IdentU[] ctx,
		void[0][ASymbol*] dep, AValCT[] params = null){
	SmErrsVal!RExpr resolved = resolve(expr, stab, ctx, dep, params);
	if (resolved.isErr)
		return SmErrsVal!(ASymbol*)(resolved.err);
	return eval4Sym(resolved.val, stab, ctx);
}
