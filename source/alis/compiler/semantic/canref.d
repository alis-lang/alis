/++
Can an RExpr be referenced?
+/
module alis.compiler.semantic.canref;

import alis.common,
			 alis.compiler.common,
			 alis.compiler.semantic.common,
			 alis.compiler.semantic.error,
			 alis.compiler.ast,
			 alis.compiler.ast.iter,
			 alis.compiler.ast.rst;

import meta;

private struct St{
	bool res;
}

private alias It = RtL!(mixin(__MODULE__), 0);

@ItFn @ITL(0){
	void varIter(RVarExpr, ref St st){
		st.res = true;
	}
	// TODO: implement the rest of all this
}

/// Returns: true whether an expression can be referenced
package bool canRef(RExpr expr){
	St st = St(false);
	It.exec(expr, st);
	return st.res;
}
