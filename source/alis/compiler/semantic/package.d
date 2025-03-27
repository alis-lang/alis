/++
Semantic Analysis Package
+/
module alis.compiler.semantic;

import std.typecons : Tuple;

import alis.common,
			 alis.compiler.common,
			 alis.compiler.error,
			 alis.compiler.rst,
			 alis.compiler.ast,
			 alis.compiler.semantic.symbols;

/// Does semantic analysis
package CmpMErrVal!(Tuple!(RModule, AModule)) analyse(Module moduleNode){
	CmpMErrVal!(Tuple!(RModule, AModule)) ret;
	STState symResState;
	STIter.iterate(moduleNode, symResState);
	// TODO: extract result from symResState into ret
	return ret;
}
