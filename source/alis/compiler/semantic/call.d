/++
Dealing with callables (functions, templates)
+/
module alis.compiler.semantic.call;

import alis.common,
			 alis.compiler.common,
			 alis.compiler.semantic.common,
			 alis.compiler.semantic.error;

/// calculates callability score.
/// `size_t.max` -> not callable
/// `0` -> highest callability
package size_t callabilityOf(ASymbol* sym, AValCT[] params){
	return size_t.max; // TODO: kinda sad tbh, nothing is callable :(
}

/// ditto
package size_t callabilityOf(ADataType type, AValCT[] params){
	return size_t.max; // TODO: kinda sad tbh, nothing is callable :(
}

/// ditto
package size_t callabilityOf(AValCT val, AValCT[] params){
	return size_t.max; // TODO: kinda sad tbh, nothing is callable :(
}
