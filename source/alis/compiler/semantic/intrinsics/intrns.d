/++
Intrinsics implementations
+/
module alis.compiler.semantic.intrinsics.intrns;

import alis.common,
			 alis.compiler.common,
			 alis.compiler.semantic.common,
			 alis.compiler.semantic.intrinsics.common,
			 alis.compiler.semantic.error,
			 alis.compiler.semantic.eval,
			 alis.compiler.ast,
			 alis.compiler.ast.rst;

package alias CallabilityCheckers = CallabilityCheckersOf!(mixin(__MODULE__));
package alias ExprTranslators = ExprTranslatorsOf!(mixin(__MODULE__));

@Intr("ctWrite"){
	@CallabilityChecker
	bool ctWriteCanCall(AValCT[]) pure {
		return true;
	}
	@ExprTranslator
	SmErrsVal!RExpr ctWriteTranslate(string, Location, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
		import std.stdio : writefln;
		writefln!"CTWRITE: %(%s%)"(params);
		return SmErrsVal!RExpr(RNoOpExpr.instance);
	}
}
