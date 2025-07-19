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
			 alis.compiler.semantic.types,
			 alis.compiler.ast,
			 alis.compiler.ast.rst;

import std.format;

package alias CallabilityCheckers = CallabilityCheckersOf!(mixin(__MODULE__));
package alias ExprTranslators = ExprTranslatorsOf!(mixin(__MODULE__));

@Intr(IntrN.CTWrite){
	@CallabilityChecker
	bool ctWriteCanCall(AValCT[] params) pure {
		return params.length == 0;
	}
	@ExprTranslator
	SmErrsVal!RExpr ctWriteTranslate(string, Location, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
		import std.stdio : writefln;
		writefln!"CTWRITE: %(%s%)"(params);
		return SmErrsVal!RExpr(RNoOpExpr.instance);
	}
}

@Intr(IntrN.Type){
	@CallabilityChecker
	bool typeCanCall(AValCT[]) pure {
		return false;
	}
	@ExprTranslator
	SmErrsVal!RExpr typeTranslate(string, Location pos, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[]){
		return SmErrsVal!RExpr([errIntrUnk(pos, IntrN.Type)]);
	}
}

@Intr(IntrN.NoInit){
	@CallabilityChecker
	bool noinitCanCall(AValCT[] params) pure {
		return params.length == 0;
	}
	@ExprTranslator
	SmErrsVal!RExpr noinitTranslate(string, Location pos, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[]){
		RAValCTExpr r = new RAValCTExpr(ADataType.ofNoInit.AValCT);
		r.pos = pos;
		return SmErrsVal!RExpr(r);
	}
}

@Intr(IntrN.NoInitVal){
	@CallabilityChecker
	bool noinitvalCanCall(AValCT[] params){
		return params.length == 0;
	}
	@ExprTranslator
	SmErrsVal!RExpr noinitvalTranslate(string, Location pos, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[]){
		RAValCTExpr r = new RAValCTExpr(
				AVal(ADataType.ofNoInit, cast(void[])null).AValCT);
		r.pos = pos;
		return SmErrsVal!RExpr(r);
	}
}

@CallabilityChecker
@Intr(IntrN.Int)
@Intr(IntrN.UInt)
@Intr(IntrN.Float)
bool bitXCanCall(AValCT[] params){
	if (params.length > 1)
		return false;
	AValCT p = params[0];
	if (p.type != AValCT.Type.Literal)
		return false;
	OptVal!size_t x = p.val.as!size_t;
	if (!x.isVal || x.val <= 0)
		return false;
	switch (x.val){
		case 8, 16, 32:
			static if (size_t.sizeof * 8 == 64){
				case 64:
					return true;
			}
		return true;
		default:
			return false;
	}
}

@ExprTranslator{
	@Intr(IntrN.Int)
	SmErrsVal!RExpr intTranslate(string, Location pos, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
		ubyte x = size_t.sizeof * 8;
		if (params.length == 0)
			x = cast(ubyte)params[0].val.as!size_t.val;
		RAValCTExpr r = new RAValCTExpr(ADataType.ofInt(x).AValCT);
		r.pos = pos;
		return SmErrsVal!RExpr(r);
	}

	@Intr(IntrN.UInt)
	SmErrsVal!RExpr uintTranslate(string, Location pos, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
		ubyte x = size_t.sizeof * 8;
		if (params.length == 0)
			x = cast(ubyte)params[0].val.as!size_t.val;
		RAValCTExpr r = new RAValCTExpr(ADataType.ofUInt(x).AValCT);
		r.pos = pos;
		return SmErrsVal!RExpr(r);
	}

	@Intr(IntrN.Float)
	SmErrsVal!RExpr floatTranslate(string, Location pos, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
		ubyte x = size_t.sizeof * 8;
		if (params.length == 0)
			x = cast(ubyte)params[0].val.as!size_t.val;
		RAValCTExpr r = new RAValCTExpr(ADataType.ofFloat(x).AValCT);
		r.pos = pos;
		return SmErrsVal!RExpr(r);
	}

	@Intr(IntrN.Char)
	SmErrsVal!RExpr charTranslate(string, Location pos, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[]){
		RAValCTExpr r = new RAValCTExpr(ADataType.ofChar.AValCT);
		r.pos = pos;
		return SmErrsVal!RExpr(r);
	}
}

@Intr(IntrN.Slice){

}
