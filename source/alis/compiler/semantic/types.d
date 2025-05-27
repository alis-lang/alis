/++
Alis Data Types semantics
+/
module alis.compiler.semantic.types;

import alis.common,
			 alis.compiler.common,
			 alis.compiler.semantic.common,
			 alis.compiler.semantic.error,
			 alis.compiler.ast,
			 alis.compiler.rst;

debug import std.stdio;

/// Returns: true if type A can be implicitly casted to B, or A is B
package bool canCastTo()(const auto ref ADataType a,
		const auto ref ADataType b) pure {
	if (a == b)
		return true;
	debug stderr.writefln!"STUB: canCastTo(%s, %s) -> false"(a, b);
	return false;
}

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

/// finds data type of RExpr
/// Returns: data type or SmErr[]
package SmErrsVal!ADataType typeOf(RExpr expr, STab stab, IdentU[] ctx){
	debug stderr.writefln!"STUB: typeOf returning type = int";
	return SmErrsVal!ADataType(ADataType.ofInt);
}
