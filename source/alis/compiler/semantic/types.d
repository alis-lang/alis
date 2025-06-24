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

/// casts AValCT (must be of AValCT.Type.Literal) to `target` ADataType
/// Returns: AValCT containing casted value, or SmErr[]
package SmErrsVal!AValCT to(AValCT val, ADataType type){
	debug stderr.writefln!"STUB: to(ADataType: %s) returning as-is `%s`"(
			type, val);
	return SmErrsVal!AValCT(val);
}

/// converts a type to const
/// Returns: cont type
package ADataType constOf()(const auto ref ADataType type) pure {
	ADataType ret = type.copy;
	ret.isConst = true;
	return ret;
}
