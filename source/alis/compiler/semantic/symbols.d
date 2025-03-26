/++
Symbols Resolution
+/
module alis.compiler.semantic.symbols;

import alis.common,
			 alis.compiler.common,
			 alis.compiler.semantic.common,
			 alis.compiler.ast,
			 alis.compiler.ast.iter;

/// symbol table builder iterator
package alias STIter = ASTIter!(ItFnsOf!STFns);

/// Iteration State for SymIter
package struct STState{
	/// symbol table. ASymbol against Ident.toString
	ASymbol[string] st;
	/// module info
	AModule mod;
}

/// symbol table builder functions
private struct STFns{
	@ItPre @ItTerm
	private static void root(Module node, STState state){

	}
}
