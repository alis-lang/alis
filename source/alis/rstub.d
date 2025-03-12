/++
stub outputs for semantic analysis phase
+/
module alis.rstub;

import alis.common,
			 alis.utils,
			 alis.compiler.common,
			 alis.compiler.rst;

/// Returns: hello world program
package RModule rstub_helloWorld(){
	enum STR = "Hello World!";
	RModule ret = new RModule;
	RFn fn = new RFn;
	fn.ident = "main.main";
	ret.fns ~= fn;
	ret.fnIsPublic ~= true;
	RIntrinsicCallExpr expr = new RIntrinsicCallExpr;
	expr.name = "writeln";
	RLiteralExpr literal = new RLiteralExpr;
	literal.type = ADataType.ofString(STR.length);
	literal.value = (cast(ubyte[])STR).dup;
	expr.params = [literal];
	fn.body = expr;
	ret.fns ~= fn;
	return ret;
}

/// Returns: simple for loop program:
/// ```alis
/// fn main () -> void{
///   var int i = 0;
///   while $is8($cmpI64(i, 10), -1){
///     write(i);
///     $incI64(i);
///   }
/// }
/// fn write (int i) -> $writeln(i);
/// ```
package RModule rstub_test(){
	RModule ret = new RModule;
	RFn main = new RFn;
	ret.fns ~= main;
	RBlockExpr blockExpr = new RBlockExpr;
	main.ident = "main.main";
	main.body = blockExpr;
	blockExpr.type.type = ADataType.Type.Struct; // void
	blockExpr.block = new RBlock;
	RBlock b = blockExpr.block;
	b.localsN = ["main.main.i"];
	b.localsT = [ADataType.ofInt];
	RAssignExpr initI = new RAssignExpr;
	RWhile iter = new RWhile;
	b.statements = [initI, iter];
	RIdentExpr iExpr = new RIdentExpr;
	iExpr.ident = "main.main.i";
	initI.lhs = iExpr;
	RLiteralExpr litZero = new RLiteralExpr, litTen = new RLiteralExpr,
							 litNegOne = new RLiteralExpr;
	litZero.value = 0L.asBytes;
	litZero.type = ADataType.ofInt;
	litTen.value = 10L.asBytes;
	litTen.type = ADataType.ofInt;
	litNegOne.value = (-1L).asBytes;
	litNegOne.type = ADataType.ofInt;
	initI.rhs = litZero;

	RIntrinsicCallExpr isI8 = new RIntrinsicCallExpr,
										 cmpI64 = new RIntrinsicCallExpr;
	isI8.name = "isI8";
	isI8.params = [cmpI64, litNegOne];
	cmpI64.name = "cmpI64";
	cmpI64.params = [iExpr, litTen];
	iter.condition = isI8;

	RIntrinsicCallExpr incI = new RIntrinsicCallExpr;
	incI.name = "incI64";
	incI.params = [iExpr];
	RFnCallExpr call = new RFnCallExpr;
	RIdentExpr writeIdent = new RIdentExpr;
	writeIdent.ident = "main.write";
	call.callee = writeIdent;
	call.params = [iExpr];
	iter.body = new RBlock;
	(cast(RBlock)iter.body).statements = [call, incI];

	RFn write = new RFn;
	ret.fns ~= write;
	write.ident = "main.write";
	write.paramCount = 1;
	write.localsT = [ADataType.ofInt];
	write.localsN = ["main.write.i"];
	iExpr = new RIdentExpr;
	iExpr.ident = "main.write.i";
	RIntrinsicCallExpr writeln = new RIntrinsicCallExpr;
	writeln.name = "writeln";
	writeln.params = [iExpr];
	write.body = writeln;
	ret.fnIsPublic = [true, false];
	return ret;
}
