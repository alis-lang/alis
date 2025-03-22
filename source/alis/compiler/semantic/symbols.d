/++
Symbols Resolution
+/
module alis.compiler.semantic.symbols;

import alis.common,
			 alis.compiler.common,
			 alis.compiler.semantic.common,
			 alis.compiler.ast,
			 alis.compiler.ast.iter;

private alias Fns = ItFnsOf!SymIterFns;
package alias SymIter = ASTIter!Fns;

/// Iteration State for SymIter
package struct State{
	/// symbol table. ASymbol against Ident.toString
	ASymbol[string] st;
}

/// Iterator
private struct SymIterFns{
	/// Builds global level symbol table
	@ItPre @ItTerm
	private static void root(Module node, State state){
		// only need to control order of iteration
	}
}
