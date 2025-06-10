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

/// finds symbol with highest callability given params and name
/// Returns: ASymbol* to callable or null if none
package ASymbol* callableTop(STab stabR, IdentU[] ctx,
		IdentU[] id, AValCT[] params){
	return null; // TODO: implement
}
