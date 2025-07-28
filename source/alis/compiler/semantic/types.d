/++
Alis Data Types semantics
+/
module alis.compiler.semantic.types;

import alis.common,
			 alis.compiler.common,
			 alis.compiler.semantic.common,
			 alis.compiler.semantic.error,
			 alis.compiler.ast,
			 alis.compiler.ast.rst;

debug import std.stdio;

/// finds a single Data Type among many, which all can cast to.
/// Returns: found data type, or `ADataType.ofNoInit` if none
package ADataType commonType(ADataType[] types){
	if (types.length == 0)
		return ADataType.ofNoInit;
	foreach (size_t i; 0 .. types.length){
		bool isCommonType = true;
		foreach (size_t j; 0 .. types.length){
			if (i == j) continue;
			if (!types[j].canCastTo(types[i])){
				isCommonType = false;
				break;
			}
		}
		if (isCommonType)
			return types[i];
	}
	return ADataType.ofNoInit;
}

/// casts RExpr to `target` ADataType, creating a new RExpr around it, which
/// does the casting
package SmErrsVal!RExpr to(RExpr expr, ADataType type){
	debug stderr.writefln!"STUB: to(ADataType: %s) returning as-is `%s`"(
			type, expr);
	// TODO: use AValCT.Type.Expr's code here
	return SmErrsVal!RExpr(expr);
}
