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
/// Returns: pointer to the common type, or null if none
package ADataType* commonType(ADataType[] types, IdentU[] ctx){
	if (types.length == 0)
		return null;
	foreach (size_t i; 0 .. types.length){
		bool isCommonType = true;
		foreach (size_t j; 0 .. types.length){
			if (i == j) continue;
			if (!types[j].canCastTo(types[i], ctx)){
				isCommonType = false;
				break;
			}
		}
		if (isCommonType)
			return &types[i];
	}
	return null;
}
