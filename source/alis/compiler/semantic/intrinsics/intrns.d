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
			 alis.compiler.semantic.typeofexpr,
			 alis.compiler.ast,
			 alis.compiler.ast.rst;

import std.format,
			 std.algorithm;

package:
alias CallabilityCheckers = CallabilityCheckersOf!(mixin(__MODULE__));
alias ExprTranslators = ExprTranslatorsOf!(mixin(__MODULE__));

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

@CallabilityChecker
@Intr(IntrN.Slice)
@Intr(IntrN.Array)
bool firstAndOnlyParamShallBeADataType(AValCT[] params){
	if (params.length != 1)
		return false;
	if (params[0].type == AValCT.Type.Type)
		return true;
	if (params[0].type == AValCT.Type.Symbol &&
			params[0].symS.isDType)
		return true;
	return false;
}

@Intr(IntrN.Slice) @ExprTranslator
SmErrsVal!RExpr sliceTranslate(string, Location pos, STab,
		IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
	RAValCTExpr r = new RAValCTExpr(
			ADataType.ofSlice(params[0].asType.val).AValCT);
	r.pos = pos;
	return SmErrsVal!RExpr(r);
}

@Intr(IntrN.Array) @ExprTranslator
SmErrsVal!RExpr arrayTranslate(string, Location pos, STab,
		IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
	RAValCTExpr r = new RAValCTExpr(
			ADataType.ofArray(params[0].asType.val).AValCT);
	r.pos = pos;
	return SmErrsVal!RExpr(r);
}

@Intr(IntrN.Vt){
	@CallabilityChecker
	bool vtCanCall(AValCT[] params){
		return params.length == 0;
	}
	@ExprTranslator
	SmErrsVal!RExpr vtTranslate(string, Location pos, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[]){
		return SmErrsVal!RExpr([errUnsup(pos, "$vt")]);
	}
}

@Intr(IntrN.IsType){
	@CallabilityChecker
	bool isTypeCanCall(AValCT[] params){
		return params.length == 1;
	}
	@ExprTranslator
	SmErrsVal!RExpr isTypeTranslate(string, Location pos, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
		AValCT p = params[0];
		RLiteralExpr r = new RLiteralExpr;
		r.pos = pos;
		final switch (p.type){
			case AValCT.Type.Symbol:
				r.val = p.symS.isDType.AVal;
				break;
			case AValCT.Type.Type:
				r.val = true.AVal;
				break;
			case AValCT.Type.Literal:
			case AValCT.Type.Expr:
				r.val = false.AVal;
				break;
			case AValCT.Type.Seq:
				return SmErrsVal!RExpr([errUnsup(pos, "AValCT.Type.Seq in $isType")]);
		}
		return SmErrsVal!RExpr(r);
	}
}

@Intr(IntrN.TypeOf){
	@CallabilityChecker
	bool typeOfCanCall(AValCT[] params){
		return !(typeOfTranslate(string.init, Location.init,
					null, null, null, null, params).isErr);
	}
	@ExprTranslator
	SmErrsVal!RExpr typeOfTranslate(string, Location pos, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
		if (params.length != 1)
			return SmErrsVal!RExpr([
					errParamCount(pos, IntrN.TypeOf.format!"$%s", 1, params.length)]);
		ADataType type;
		final switch (params[0].type){
			case AValCT.Type.Symbol:
				ASymbol* sym = params[0].symS;
				final switch (sym.type){
					case ASymbol.Type.Struct:
					case ASymbol.Type.Union:
					case ASymbol.Type.Enum:
					case ASymbol.Type.Import:
					case ASymbol.Type.UTest:
					case ASymbol.Type.Template:
						return SmErrsVal!RExpr([errCallableIncompat(pos,
									IntrN.TypeOf.format!"$%s", params.map!(p => p.toString))]);
					case ASymbol.Type.EnumConst:
						type = sym.enumCS.type;
						break;
					case ASymbol.Type.Fn:
						type = ADataType.ofFn(sym.fnS.retT, sym.fnS.paramsT);
						break;
					case ASymbol.Type.Var:
						type = sym.varS.type;
						break;
					case ASymbol.Type.Alias:
						// TODO: decide on $typeOf(alias)
						return SmErrsVal!RExpr([errUnsup(pos, "$isType(alias)")]);
				}
				break;
			case AValCT.Type.Type:
				return SmErrsVal!RExpr([errCallableIncompat(pos,
							IntrN.TypeOf.format!"$%s", params.map!(p => p.toString))]);
				break;
			case AValCT.Type.Literal:
				type = params[0].val.type;
				break;
			case AValCT.Type.Expr:
				SmErrsVal!ADataType res = typeOf(params[0].expr);
				if (res.isErr)
					return SmErrsVal!RExpr(res.err);
				type = res.val;
				break;
			case AValCT.Type.Seq:
				return SmErrsVal!RExpr([errUnsup(pos, "$isType(sequence)")]);
		}
		RAValCTExpr r = new RAValCTExpr(type.AValCT);
		r.pos = pos;
		return SmErrsVal!RExpr(r);
	}
}

@Intr(IntrN.ArrayLen){
	@CallabilityChecker
	bool arrLenCanCall(AValCT[] params){
		if (params.length == 0 || !params[0].isVal)
			return false;
		ADataType type = params[0].valType.val;
		if (params.length == 1){
			if (type.type == ADataType.Type.Ref)
				type = *type.refT;
			if (type.type != ADataType.Type.Array &&
					type.type != ADataType.Type.Slice)
				return false;
			return true;
		}
		if (params.length == 2){
			if (type.type != ADataType.Type.Ref)
				return false;
			if (type.refT.type != ADataType.Type.Array)
				return false;
			if (!params[1].valType.val.canCastTo(ADataType.ofUInt))
				return false;
			return true;
		}
		return false;
	}

	@ExprTranslator
	SmErrsVal!RExpr arrLenTranslate(string, Location pos, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
		if (params.length == 1){
			RArrayLenExpr r = new RArrayLenExpr;
			r.pos = pos;
			r.arr = params[0].toRExpr;
			return SmErrsVal!RExpr(r);
		}
		RArrayLenSetExpr r = new RArrayLenSetExpr;
		r.pos = pos;
		r.arr = params[0].toRExpr;
		r.len = params[1].toRExpr;
		return SmErrsVal!RExpr(r);
	}
}
