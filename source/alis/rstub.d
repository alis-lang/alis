/++
stub outputs for semantic analysis phase
+/
module alis.rstub;

import alis.common,
			 alis.compiler.common,
			 alis.compiler.rst;

/// Returns: hello world program
RModule rstub_helloWorld(){
	enum STR = "Hello World!";
	RModule ret = new RModule;
	RFn fn = new RFn;
	ret.fns ~= fn;
	ret.fnIsPublic ~= true;
	RIntrinsicCallExpr expr = new RIntrinsicCallExpr;
	expr.name = "writeln";
	RLiteralExpr literal = new RLiteralExpr;
	literal.type = ADataType.ofString(STR.length);
	literal.value = (cast(ubyte[])STR).dup;
	expr.params = [literal];
	return ret;
}
