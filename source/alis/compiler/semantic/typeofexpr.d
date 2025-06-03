/++
Alis `$typeOf` implementation
+/
module alis.compiler.semantic.typeofexpr;

import alis.common,
			 alis.compiler.common,
			 alis.compiler.semantic.common,
			 alis.compiler.semantic.error,
			 alis.compiler.ast,
			 alis.compiler.ast.rst;

debug import std.stdio;

private alias It = ItL!(mixin(__MODULE__), 0);

private struct St{
	SmErr[] errs;
	STab stabR;
	STab stab;
	IdentU[] ctx;
	ADataType res;
}

@ItFn @ITL(0){
	void iterLiteral(RLiteralExpr node, ref St st){
		st.res = node.type;
	}
	void iterIntrCall(RIntrinsicCallExpr, ref St st){
		debug stderr.writefln!"STUB: RIntrinsicCallExpr typeOf -> struct{}";
		st.res = ADataType.init;
	}
	void iterExpr(RExpr node, ref St st){
		st.errs ~= errUnsup(node);
	}
}

/// finds data type of RExpr
/// Returns: data type or SmErr[]
package SmErrsVal!ADataType typeOf(RExpr expr, STab stabR, IdentU[] ctx){
	St st = St(null, stabR, stabR.findSt(ctx, ctx), ctx, ADataType.ofNoInit);
	It.exec(expr, st);
	if (st.errs.length){
		debug {
			stderr.writefln!"STUB: typeOf returning type = int";
			return SmErrsVal!ADataType(ADataType.ofInt);
		}
		return SmErrsVal!ADataType(st.errs);
	}
	return SmErrsVal!ADataType(st.res);
}
