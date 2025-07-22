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

	/*void intrExpr(RIntrinsicExpr node, ref St st){
		switch (node.name){
			case IntrN.NoInit:
				st.res = AValCT(ADataType.ofNoInit); return;
			case IntrN.NoInitVal:
				st.res = AValCT(ADataType.ofNoInit, cast(ubyte[])null); return;
			case IntrN.Debug:
				debug{
					st.res = AValCT(ADataType.ofBool, true.asBytes);
				} else {
					st.res = AValCT(ADataType.ofBool, false.asBytes);
				}
				return;
			default:
				st.errs ~= errUnsup(node.pos, node.name.format!"intrinsic $%s");
		}
	}

	void intrCallExpr(RIntrinsicCallExpr node, ref St st){
		switch (node.name){
			case IntrN.Int, IntrN.UInt, IntrN.Float, IntrN.Char:
				intrXBitType(node, st); break;
			case IntrN.CTWrite:
				intrCTWrite(node, st); break;
			default:
				st.errs ~= errUnsup(node.pos, node.name.format!"intrinsic call $%s");
		}
	}*/

	void exprIter(RExpr node, ref St st){
		st.errs ~= errUnsup(node);
	}
}

/*private void intrXBitType(RIntrinsicCallExpr node, ref St st){
	AValCT[] params;
	params = node.params
		.map!(p => eval(p, st.stabR, st.ctx))
		.tee!((SmErrsVal!AValCT p){
				if (p.isErr)
					st.errs ~= p.err;
				})
		.filter!(p => !p.isErr)
		.map!(p => p.val).array;
	if (params.length != node.params.length)
		return;
	switch (node.name){
		case IntrN.Int:
			st.res = ADataType.ofInt(cast(ubyte)params[0].dataL.as!int).AValCT;
			break;
		case IntrN.UInt:
			st.res = ADataType.ofUInt(cast(ubyte)params[0].dataL.as!int).AValCT;
			break;
		case IntrN.Float:
			st.res = ADataType.ofFloat(cast(ubyte)params[0].dataL.as!int).AValCT;
			break;
		case IntrN.Char:
			st.res = ADataType.ofChar(cast(ubyte)params[0].dataL.as!int).AValCT;
			break;
		default:
			st.errs ~= errUnsup(node.pos, node.name.format!"intrinsic call $%s");
	}
}

private void intrCTWrite(RIntrinsicCallExpr node, ref St st){
	AValCT[] params;
	params = node.params
		.map!(p => eval(p, st.stabR, st.ctx))
		.tee!((SmErrsVal!AValCT p){
				if (p.isErr)
					st.errs ~= p.err;
				})
		.filter!(p => !p.isErr)
		.map!(p => p.val).array;
	foreach (AValCT p; params){
		import std.stdio : writefln;
		string s;
		if (p.typeL == ADataType.ofString){
			s = cast(immutable char[])p.dataL;
		} else
		if (p.typeL == ADataType.ofInt){
			s = p.dataL.as!int.to!string;
		} else {
			st.errs ~= errUnsup(node.params[0].pos, "$ctWrite on non-string non-int");
			return;
		}
		writefln!"CTWRITE: %s"(s);
	}
	st.res = ADataType.ofNoInit.AValCT;
}*/

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
			return SmErrsVal!AValCT(5.AVal.AValCT);
		}
		return SmErrsVal!AValCT(st.errs);
	}
	return SmErrsVal!AValCT(st.res);
}

/// ditto
package SmErrsVal!AValCT eval(Expression expr, STab stab, IdentU[] ctx,
		void[0][ASymbol*] dep, RFn[string] fns, AValCT[] params = null){
	SmErrsVal!RExpr resolved = resolve(expr, stab, ctx, dep, fns, params);
	if (resolved.isErr){
		debug{
			import std.stdio;
			stderr.writefln!"STUB: eval(Expression) errored %s, returning 5.int"(
					resolved.err);
			return SmErrsVal!AValCT(5.AVal.AValCT);
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
			return SmErrsVal!AValCT(5.AVal.AValCT);
		}
		return ret;
	}
	if (ret.val.type != AValCT.Type.Literal)
		return SmErrsVal!AValCT([errExprValExpected(expr)]);
	return ret;
}

/// ditto
package SmErrsVal!AValCT eval4Val(Expression expr, STab stab, IdentU[] ctx,
		void[0][ASymbol*] dep, RFn[string] fns, AValCT[] params = null){
	SmErrsVal!RExpr resolved = resolve(expr, stab, ctx, dep, fns, params);
	if (resolved.isErr){
		debug{
			import std.stdio;
			stderr.writefln!"STUB: eval4Val errored %s, returning 5.int"(
					resolved.err);
			return SmErrsVal!AValCT(5.AVal.AValCT);
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
		void[0][ASymbol*] dep, RFn[string] fns, AValCT[] params = null){
	SmErrsVal!RExpr resolved = resolve(expr, stab, ctx, dep, fns, params);
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
		void[0][ASymbol*] dep, RFn[string] fns, AValCT[] params = null){
	SmErrsVal!RExpr resolved = resolve(expr, stab, ctx, dep, fns, params);
	if (resolved.isErr)
		return SmErrsVal!(ASymbol*)(resolved.err);
	return eval4Sym(resolved.val, stab, ctx);
}
