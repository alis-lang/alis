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

import std.format,
			 std.array,
			 std.algorithm;

debug import std.stdio;

package:
alias CallabilityCheckers = CallabilityCheckersOf!(mixin(__MODULE__));
alias ExprTranslators = ExprTranslatorsOf!(mixin(__MODULE__));

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
		if (params.length != 0)
			x = cast(ubyte)params[0].val.as!size_t.val;
		RAValCTExpr r = new RAValCTExpr(ADataType.ofInt(x).AValCT);
		r.pos = pos;
		return SmErrsVal!RExpr(r);
	}

	@Intr(IntrN.UInt)
	SmErrsVal!RExpr uintTranslate(string, Location pos, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
		ubyte x = size_t.sizeof * 8;
		if (params.length != 0)
			x = cast(ubyte)params[0].val.as!size_t.val;
		RAValCTExpr r = new RAValCTExpr(ADataType.ofUInt(x).AValCT);
		r.pos = pos;
		return SmErrsVal!RExpr(r);
	}

	@Intr(IntrN.Float)
	SmErrsVal!RExpr floatTranslate(string, Location pos, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
		ubyte x = size_t.sizeof * 8;
		if (params.length != 0)
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

@Intr(IntrN.IsType){
	@CallabilityChecker
	bool isTypeCanCall(AValCT[] params){
		return params.length == 1;
	}
	@ExprTranslator
	SmErrsVal!RExpr isTypeTranslate(string, Location pos, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
		AValCT p = params[0];
		RLiteralExpr r;
		final switch (p.type){
			case AValCT.Type.Symbol:
				r = new RLiteralExpr(p.symS.isDType.AVal);
				break;
			case AValCT.Type.Type:
				r = new RLiteralExpr(true.AVal);
				break;
			case AValCT.Type.Literal:
			case AValCT.Type.Expr:
				r = new RLiteralExpr(false.AVal);
				break;
			case AValCT.Type.Seq:
				return SmErrsVal!RExpr([errUnsup(pos, "AValCT.Type.Seq in $isType")]);
		}
		r.pos = pos;
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
				type = params[0].expr.type;
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
			if (!params[1].canCastTo(ADataType.ofUInt))
				return false;
			return true;
		}
		return false;
	}

	@ExprTranslator
	SmErrsVal!RExpr arrLenTranslate(string, Location pos, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
		if (params.length == 1){
			RArrayLenExpr r = new RArrayLenExpr(params[0].toRExpr);
			r.pos = pos;
			return SmErrsVal!RExpr(r);
		}
		RArrayLenSetExpr r = new RArrayLenSetExpr(
				params[0].toRExpr,
				params[1].toRExpr);
		r.pos = pos;
		return SmErrsVal!RExpr(r);
	}
}

@Intr(IntrN.ArrayInd){
	@CallabilityChecker
	bool arrIndCanCall(AValCT[] params){
		if (params.length != 2)
			return false;
		if (!params[0].isVal || !params[1].isVal)
			return false;
		ADataType type = params[0].valType.val;
		if (type.type != ADataType.Type.Array &&
				type.type != ADataType.Type.Slice)
			return false;
		if (!params[1].canCastTo(ADataType.ofUInt))
			return false;
		return true;
	}

	@ExprTranslator
	SmErrsVal!RExpr arrIndTranslate(string, Location pos, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
		RArrayIndexExpr r = new RArrayIndexExpr(
				params[0].toRExpr,
				params[1].toRExpr);
		r.pos = pos;
		return SmErrsVal!RExpr(r);
	}
}

@Intr(IntrN.SeqLen){
	@CallabilityChecker
	bool seqLenCanCall(AValCT[]){
		return true;
	}
	@ExprTranslator
	SmErrsVal!RExpr seqLenTranslate(string, Location, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
		return SmErrsVal!RExpr(new RLiteralExpr(params.length.AVal));
	}
}

@Intr(IntrN.SeqInd){
	@CallabilityChecker
	bool seqIndCanCall(AValCT[] params){
		return params.length >= 2 &&
			params[$ - 1].isVal &&
			params[$ - 1].canCastTo(ADataType.ofUInt);
	}

	@ExprTranslator
	SmErrsVal!RExpr seqIndTranslate(string, Location pos, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
		OptVal!size_t index = params[$ - 1].val.as!size_t;
		if (!index.isVal)
			return SmErrsVal!RExpr([
					errIncompatType(pos, ADataType.ofUInt.toString,
						params[$ - 1].valType.val.toString)]);
		if (index.val + 1 > params.length)
			return SmErrsVal!RExpr([
					errBounds(pos, cast(ptrdiff_t)params.length - 1, index.val)]);
		return SmErrsVal!RExpr(params[index.val].toRExpr);
	}
}

@Intr(IntrN.UnionIs){
	@CallabilityChecker
	bool unionIsCanCall(AValCT[] params){
		if (params.length != 1)
			return false;
		RUnionMemberGetExpr p = cast(RUnionMemberGetExpr)(params[0].toRExpr);
		if (p is null)
			return false;
		return true;
	}
	@ExprTranslator
	SmErrsVal!RExpr unionIsTranslate(string, Location, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
		RUnionMemberGetExpr p = cast(RUnionMemberGetExpr)(params[0].toRExpr);
		assert (p !is null);
		RUnionIsExpr r = new RUnionIsExpr(p.val, p.memId);
		r.pos = p.pos;
		return SmErrsVal!RExpr(p);
	}
}

@Intr(IntrN.Members){
	@CallabilityChecker
	bool membersCanCall(AValCT[] params){
		if (params.length != 1)
			return false;
		AValCT p = params[0];
		final switch (p.type){
			case AValCT.Type.Type:
				final switch (p.typeT.type){
					case ADataType.Type.Seq:
					case ADataType.Type.IntX:
					case ADataType.Type.UIntX:
					case ADataType.Type.FloatX:
					case ADataType.Type.Char:
					case ADataType.Type.Bool:
					case ADataType.Type.Slice:
					case ADataType.Type.Array:
					case ADataType.Type.Ref:
					case ADataType.Type.NoInit:
					case ADataType.Type.Fn:
						return false;
					case ADataType.Type.Struct:
					case ADataType.Type.Union:
					case ADataType.Type.Enum:
						return true;
				}
			case AValCT.Type.Expr:
			case AValCT.Type.Seq:
				return false;
			case AValCT.Type.Symbol:
				final switch (p.symS.type){
					case ASymbol.Type.Struct:
					case ASymbol.Type.Union:
					case ASymbol.Type.Enum:
					case ASymbol.Type.Import:
						return true;
					case ASymbol.Type.Fn:
					case ASymbol.Type.Var:
					case ASymbol.Type.EnumConst:
					case ASymbol.Type.UTest:
					case ASymbol.Type.Alias:
					case ASymbol.Type.Template:
						return false;
				}
			case AValCT.Type.Literal:
				return false;
		}
	}

	@ExprTranslator
	SmErrsVal!RExpr membersTranslate(string, Location pos, STab,
			IdentU[] ctx, void[0][ASymbol*], RFn[string], AValCT[] params){
		AValCT p = params[0];
		AValCT[] names;
		final switch (p.type){
			case AValCT.Type.Type:
				final switch (p.typeT.type){
					case ADataType.Type.Seq:
					case ADataType.Type.IntX:
					case ADataType.Type.UIntX:
					case ADataType.Type.FloatX:
					case ADataType.Type.Char:
					case ADataType.Type.Bool:
					case ADataType.Type.Slice:
					case ADataType.Type.Array:
					case ADataType.Type.Ref:
					case ADataType.Type.NoInit:
					case ADataType.Type.Fn:
						assert (false);
					case ADataType.Type.Struct:
						AStruct* symC = p.typeT.structS;
						if (symC is null)
							break;
						names = symC.names.byKey
							.filter!(s => symC.exists(s, ctx))
							.map!(s => s.AVal.AValCT)
							.array;
						break;
					case ADataType.Type.Union:
						AUnion* symC = p.typeT.unionS;
						names = symC.names.byKey
							.filter!(s => symC.exists(s, ctx))
							.map!(s => s.AVal.AValCT)
							.array;
						break;
					case ADataType.Type.Enum:
						AEnum* symC = p.typeT.enumS;
						names = symC.memId
							.map!(s => s.AVal.AValCT)
							.array;
						break;
				}
				break;
			case AValCT.Type.Expr:
			case AValCT.Type.Seq:
				assert (false); // CallabilityChecker should've stopped this
			case AValCT.Type.Symbol:
				final switch (p.symS.type){
					case ASymbol.Type.Struct:
						AStruct* symC = &p.symS.structS;
						names = symC.names.byKey
							.filter!(s => symC.exists(s, ctx))
							.map!(s => s.AVal.AValCT)
							.array;
						break;
					case ASymbol.Type.Union:
						AUnion* symC = &p.symS.unionS;
						names = symC.names.byKey
							.filter!(s => symC.exists(s, ctx))
							.map!(s => s.AVal.AValCT)
							.array;
						break;
					case ASymbol.Type.Enum:
						AEnum* symC = &p.symS.enumS;
						names = symC.memId
							.map!(s => s.AVal.AValCT)
							.array;
						break;
					case ASymbol.Type.Import:
						return SmErrsVal!RExpr([errUnsup(pos, "$members(import)")]);
					case ASymbol.Type.Fn:
					case ASymbol.Type.Var:
					case ASymbol.Type.EnumConst:
					case ASymbol.Type.UTest:
					case ASymbol.Type.Alias:
					case ASymbol.Type.Template:
						assert (false); // CallabilityChecker should've stopped this
				}
				break;
			case AValCT.Type.Literal:
				assert (false); // CallabilityChecker should've stopped this
		}
		RAValCTExpr r = new RAValCTExpr(names.AValCT);
		r.pos = pos;
		return SmErrsVal!RExpr(r);
	}
}

@Intr(IntrN.MemberField){
	@CallabilityChecker
	bool memberFieldCanCall(AValCT[] params){
		if (params.length != 2)
			return false;
		if (!params[0 .. 1].membersCanCall)
			return false;
		if (params[1].type != AValCT.Type.Literal ||
				params[1].val.type != ADataType.ofString)
			return false;
		return true;
	}

	@ExprTranslator
	SmErrsVal!RExpr memberFieldTranslate(string, Location pos, STab,
			IdentU[] ctx, void[0][ASymbol*], RFn[string], AValCT[] params){
		AValCT p = params[0];
		assert (params[1].type == AValCT.Type.Literal);
		assert (params[1].val.type == ADataType.ofString);
		string name = params[1].val.as!string.val;
		string res;
		final switch (p.type){
			case AValCT.Type.Type:
				final switch (p.typeT.type){
					case ADataType.Type.Seq:
					case ADataType.Type.IntX:
					case ADataType.Type.UIntX:
					case ADataType.Type.FloatX:
					case ADataType.Type.Char:
					case ADataType.Type.Bool:
					case ADataType.Type.Slice:
					case ADataType.Type.Array:
					case ADataType.Type.Ref:
					case ADataType.Type.NoInit:
					case ADataType.Type.Fn:
						assert (false);
					case ADataType.Type.Struct:
						AStruct* symC = p.typeT.structS;
						if (symC is null || !symC.exists(name, ctx))
							return SmErrsVal!RExpr([errMemberNoExist(pos, p.toString, name)]);
						immutable size_t target = symC.names[name];
						foreach (string n, size_t id; symC.names){
							if (id == target){
								res = n;
								break;
							}
						}
						break;
					case ADataType.Type.Union:
						AUnion* symC = p.typeT.unionS;
						if (!symC.exists(name, ctx))
							return SmErrsVal!RExpr([errMemberNoExist(pos, p.toString, name)]);
						immutable size_t target = symC.names[name];
						foreach (string n, size_t id; symC.names){
							if (id == target){
								res = n;
								break;
							}
						}
						break;
					case ADataType.Type.Enum:
						res = name;
						break;
				}
				break;
			case AValCT.Type.Expr:
			case AValCT.Type.Seq:
				assert (false); // CallabilityChecker should've stopped this
			case AValCT.Type.Symbol:
				final switch (p.symS.type){
					case ASymbol.Type.Struct:
						AStruct* symC = &p.symS.structS;
						if (!symC.exists(name, ctx))
							return SmErrsVal!RExpr([errMemberNoExist(pos, p.toString, name)]);
						immutable size_t target = symC.names[name];
						foreach (string n, size_t id; symC.names){
							if (id == target){
								res = n;
								break;
							}
						}
						break;
					case ASymbol.Type.Union:
						AUnion* symC = &p.symS.unionS;
						if (!symC.exists(name, ctx))
							return SmErrsVal!RExpr([errMemberNoExist(pos, p.toString, name)]);
						immutable size_t target = symC.names[name];
						foreach (string n, size_t id; symC.names){
							if (id == target){
								res = n;
								break;
							}
						}
						break;
					case ASymbol.Type.Enum:
						res = name;
						break;
					case ASymbol.Type.Import:
						return SmErrsVal!RExpr([errUnsup(pos, "$members(import)")]);
					case ASymbol.Type.Fn:
					case ASymbol.Type.Var:
					case ASymbol.Type.EnumConst:
					case ASymbol.Type.UTest:
					case ASymbol.Type.Alias:
					case ASymbol.Type.Template:
						assert (false); // CallabilityChecker should've stopped this
				}
				break;
			case AValCT.Type.Literal:
				assert (false); // CallabilityChecker should've stopped this
		}
		RAValCTExpr r = new RAValCTExpr(res.AVal.AValCT);
		r.pos = pos;
		return SmErrsVal!RExpr(r);
	}
}

@Intr(IntrN.Member){
	@CallabilityChecker
	bool memberCanCall(AValCT[] params){
		if (params.length != 2)
			return false;
		if (params[1].type != AValCT.Type.Literal ||
				params[1].val.type != ADataType.ofString)
			return false;
		if (params[0].type == AValCT.Type.Symbol)
			return params[0].symS.type == ASymbol.type.Enum;
		ADataType type = params[0].valType.val;
		if (type.type == ADataType.Type.Ref){
			if (type.refT is null)
				return false;
			type = *(type.refT);
		}
		return type.type == ADataType.Type.Struct ||
			type.type == ADataType.Type.Union;
	}

	@ExprTranslator
	SmErrsVal!RExpr memberTranslate(string, Location pos, STab,
			IdentU[] ctx, void[0][ASymbol*], RFn[string], AValCT[] params){
		assert (params.length == 2);
		assert (params[1].type == AValCT.Type.Literal);
		assert (params[1].val.type == ADataType.ofString);
		string name = params[1].val.as!string.val;
		if (params[0].type == AValCT.Type.Symbol){
			assert (params[0].symS.type == ASymbol.Type.Enum);
			AEnum* enumS = &params[0].symS.enumS;
			immutable ptrdiff_t index = enumS.memId.countUntil(name);
			if (index < 0){
				return SmErrsVal!RExpr([
						errMemberNoExist(pos, enumS.ident.toString, name)]);
			}
			REnumMemberGetExpr r = new REnumMemberGetExpr(
					AVal(enumS.type, enumS.memVal[index]), enumS, name);
			r.pos = pos;
			return SmErrsVal!RExpr(r);
		}

		RExpr lhsExpr = params[0].toRExpr;
		ADataType type = lhsExpr.type;
		RExpr r;
		assert (type.type == ADataType.Type.Struct ||
				type.type == ADataType.Type.Union ||
				(type.type == ADataType.Type.Ref && type.refT !is null &&
				 (type.refT.type == ADataType.Type.Struct ||
					type.refT.type == ADataType.Type.Union)));
		if (type.type == ADataType.Type.Ref){
			type = *(type.refT);
			if (type.type == ADataType.Type.Struct){
				AStruct* structS = type.structS;
				if (type.type == ADataType.Type.Struct)
					r = new RStructRefMemberGetExpr(lhsExpr,
							structS.names[name],
							type.isConst || (
								structS.ident.length && ctx.length &&
								ctx[0] != structS.ident[0] &&
								structS.nameVis[name] == Visibility.IPub)
							);
			} else
			if (type.type == ADataType.Type.Union){
				AUnion* unionS = type.unionS;
				r = new RUnionRefMemberGetExpr(lhsExpr,
						unionS.names[name],
						type.isConst || (
							unionS.ident.length && ctx.length &&
							ctx[0] != unionS.ident[0] &&
							unionS.nameVis[name] == Visibility.IPub)
						);
			}
		} else
		if (type.type == ADataType.Type.Struct){
			AStruct* structS = type.structS;
			if (structS !is null && structS.exists(name, ctx))
				r = new RStructMemberGetExpr(lhsExpr,
						structS.names[name],
						type.isConst || (
							structS.ident.length && ctx.length &&
							ctx[0] != structS.ident[0] &&
							structS.nameVis[name] == Visibility.IPub)
						);
		} else
		if (type.type == ADataType.Type.Union){
			AUnion* unionS = type.unionS;
			if (unionS.exists(name, ctx))
				r = new RUnionMemberGetExpr(lhsExpr,
						unionS.names[name],
						type.isConst || (
							unionS.ident.length && ctx.length &&
							ctx[0] != unionS.ident[0] &&
							unionS.nameVis[name] == Visibility.IPub)
						);
		}
		if (r is null){
			return SmErrsVal!RExpr([
					errMemberNoExist(pos, type.toString, name)]);
		}
		r.pos = pos;
		return SmErrsVal!RExpr(r);
	}
}

@Intr(IntrN.AttrsOf){
	@CallabilityChecker
	bool attrsOfCanCall(AValCT[] params){
		if (params.length != 1)
			return false;
		if (params[0].type != AValCT.Type.Symbol)
			return false;
		return true;
	}
	@ExprTranslator
	SmErrsVal!RExpr attrsOfTranslate(string, Location pos, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[]){
		return SmErrsVal!RExpr([
				errUnsup(pos, "$attrsOf")]);
	}
}

@Intr(IntrN.ByAttrs){
	@CallabilityChecker byAttrsCanCall(AValCT[] params){
		if (params.length != 2)
			return false;
		if (params[0].type != AValCT.Type.Symbol)
			return false;
		// TODO: test params[1]
		return true;
	}
	@ExprTranslator
	SmErrsVal!RExpr byAttrsTranslate(string, Location pos, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[]){
		return SmErrsVal!RExpr([
				errUnsup(pos, "$byAttrs")]);
	}
}

@Intr(IntrN.Debug){
	@CallabilityChecker
	bool debugCanCall(AValCT[] params){
		return params.length == 0;
	}
	@ExprTranslator
	SmErrsVal!RExpr debugTranslate(string, Location pos, STab stabR,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[]){
		RLiteralExpr r = new RLiteralExpr(
				stabR.canFind("$debug".IdentU, [IdentU.init]).AVal);
		r.pos = pos;
		return SmErrsVal!RExpr(r);
	}
}

@Intr(IntrN.StackTrace){
	@CallabilityChecker
	bool stackTraceCanCall(AValCT[] params){
		return params.length == 0;
	}
	@ExprTranslator
	SmErrsVal!RExpr stackTraceTranslate(string, Location pos, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[]){
		RStackTraceExpr r = new RStackTraceExpr();
		r.pos = pos;
		return SmErrsVal!RExpr(r);
	}
}

@Intr(IntrN.Err){
	@CallabilityChecker
	bool errCanCall(AValCT[] params){
		return params.length == 1 &&
			params[0].type == AValCT.Type.Literal &&
			params[0].val.type == ADataType.ofString;
	}
	@ExprTranslator
	SmErrsVal!RExpr errTranslate(string, Location pos, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
		return SmErrsVal!RExpr([
				errErr(pos, params[0].val.as!string.val)]);
	}
}

@Intr(IntrN.CTWrite){
	@CallabilityChecker
	bool ctWriteCanCall(AValCT[] params) pure {
		return params.length != 0;
	}
	@ExprTranslator
	SmErrsVal!RExpr ctWriteTranslate(string, Location, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
		import std.stdio : writefln;
		writefln!"CTWRITE: %(%s%)"(params);
		return SmErrsVal!RExpr(RNoOpExpr.instance);
	}
}

@Intr(IntrN.RTWrite){
	@CallabilityChecker
	bool rtWriteCanCall(AValCT[] params) pure {
		return params.length == 1;
	}
	@ExprTranslator
	SmErrsVal!RExpr rtWriteTranslate(string, Location pos, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
		RTWriteExpr r = new RTWriteExpr;
		r.val = params[0].toRExpr;
		r.pos = pos;
		return SmErrsVal!RExpr(r);
	}
}

@Intr(IntrN.Negate){
	@CallabilityChecker
	bool negateCanCall(AValCT[] params){
		if (params.length != 1 || !params[0].isVal)
			return false;
		ADataType type = params[0].valType.val;
		return type.type == ADataType.Type.FloatX ||
			type.type == ADataType.Type.IntX;
	}
	@ExprTranslator
	SmErrsVal!RExpr negateTranslate(string, Location pos, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
		RNegExpr r = new RNegExpr(params[0].toRExpr);
		r.pos = pos;
		return SmErrsVal!RExpr(r);
	}
}

@Intr(IntrN.BitNot){
	@CallabilityChecker
	bool bitNotCanCall(AValCT[] params){
		if (params.length != 1 || !params[0].isVal)
			return false;
		ADataType type = params[0].valType.val;
		return type.type == ADataType.Type.FloatX ||
			type.type == ADataType.Type.IntX ||
			type.type == ADataType.Type.UIntX;
	}
	@ExprTranslator
	SmErrsVal!RExpr bitNotTranslate(string, Location pos, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
		RBitNotExpr r = new RBitNotExpr(params[0].toRExpr);
		r.pos = pos;
		return SmErrsVal!RExpr(r);
	}
}

@CallabilityChecker
@Intr(IntrN.BitAnd)
@Intr(IntrN.BitOr)
@Intr(IntrN.BitXor)
bool bitBinCanCall(AValCT[] params){
	return params.length == 2 &&
		params[0].isVal &&
		params[1].isVal &&
		params[0].valType.val == params[1].valType.val;
}

@ExprTranslator
@Intr(IntrN.BitAnd)
@Intr(IntrN.BitOr)
@Intr(IntrN.BitXor)
SmErrsVal!RExpr bitBinTranslate(string name, Location pos, STab,
		IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
	RExpr r;
	switch (name){
		case IntrN.BitAnd:
			r = new RBitAndExpr(params[0].toRExpr, params[1].toRExpr);
			break;
		case IntrN.BitOr:
			r = new RBitOrExpr(params[0].toRExpr, params[1].toRExpr);
			break;
		case IntrN.BitXor:
			r = new RBitXorExpr(params[0].toRExpr, params[1].toRExpr);
			break;
		default:
			assert (false);
	}
	r.pos = pos;
	return SmErrsVal!RExpr(r);
}

@CallabilityChecker
@Intr(IntrN.Add)
@Intr(IntrN.Sub)
@Intr(IntrN.Mul)
@Intr(IntrN.Div)
bool arithBinCanCall(AValCT[] params){
	if (params.length != 2 || !params[0].isVal || !params[1].isVal)
		return false;
	ADataType type = params[0].valType.val;
	if (type.type != ADataType.Type.FloatX &&
			type.type != ADataType.Type.IntX &&
			type.type != ADataType.Type.UIntX)
		return false;
	return type == params[1].valType.val;
}

@ExprTranslator
@Intr(IntrN.Add)
@Intr(IntrN.Sub)
@Intr(IntrN.Mul)
@Intr(IntrN.Div)
SmErrsVal!RExpr arithBinTranslate(string name, Location pos, STab,
		IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
	RExpr r;
	switch (name){
		case IntrN.Add:
			r = new RAddExpr(params[0].toRExpr, params[1].toRExpr);
			break;
		case IntrN.Sub:
			r = new RSubExpr(params[0].toRExpr, params[1].toRExpr);
			break;
		case IntrN.Mul:
			r = new RMulExpr(params[0].toRExpr, params[1].toRExpr);
			break;
		case IntrN.Div:
			r = new RDivExpr(params[0].toRExpr, params[1].toRExpr);
			break;
		default:
			assert (false);
	}
	r.pos = pos;
	return SmErrsVal!RExpr(r);
}

@Intr(IntrN.Mod){
	@CallabilityChecker
	bool modCanCall(AValCT[] params){
		if (params.length != 2 || !params[0].isVal || !params[1].isVal)
			return false;
		ADataType type = params[0].valType.val;
		if (type.type != ADataType.Type.IntX &&
				type.type != ADataType.Type.UIntX)
			return false;
		return type == params[1].valType.val;
	}
	@ExprTranslator
	SmErrsVal!RExpr modTranslate(string, Location pos, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
		RModExpr r = new RModExpr(params[0].toRExpr, params[1].toRExpr);
		r.pos = pos;
		return SmErrsVal!RExpr(r);
	}
}

@CallabilityChecker
@Intr(IntrN.ShiftL)
@Intr(IntrN.ShiftR)
bool shiftCanCall(AValCT[] params){
	if (params.length != 2 || !params[0].isVal || !params[1].isVal)
		return false;
	ADataType type = params[0].valType.val;
	if (type.type != ADataType.Type.FloatX &&
			type.type != ADataType.Type.IntX &&
			type.type != ADataType.Type.UIntX)
		return false;
	type = params[1].valType.val;
	return type.type == ADataType.Type.UIntX;
}

@ExprTranslator
@Intr(IntrN.ShiftL)
@Intr(IntrN.ShiftR)
SmErrsVal!RExpr shiftTranslate(string name, Location pos, STab,
		IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
	RExpr r;
	switch (name){
		case IntrN.ShiftL:
			r = new RShiftLExpr(params[0].toRExpr, params[1].toRExpr);
			break;
		case IntrN.ShiftR:
			r = new RShiftRExpr(params[0].toRExpr, params[1].toRExpr);
			break;
		default:
			assert (false);
	}
	r.pos = pos;
	return SmErrsVal!RExpr(r);
}

@CallabilityChecker
@Intr(IntrN.Is)
@Intr(IntrN.IsNot)
@Intr(IntrN.IsLess)
bool cmpCanCall(AValCT[] params){
	if (params.length != 2 ||
			!params[0].isVal ||
			!params[1].isVal)
		return false;
	return params[0].valType.val == params[1].valType.val;
}

@ExprTranslator
@Intr(IntrN.Is)
@Intr(IntrN.IsNot)
@Intr(IntrN.IsLess)
SmErrsVal!RExpr cmpTranslate(string name, Location, STab,
		IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
	RExpr r;
	switch (name){
		case IntrN.Is:
			r = new RCmpIsExpr(params[0].toRExpr, params[1].toRExpr);
			break;
		case IntrN.IsNot:
			r = new RCmpNotIsExpr(params[0].toRExpr, params[1].toRExpr);
			break;
		case IntrN.IsLess:
			r = new RCmpLessExpr(params[0].toRExpr, params[1].toRExpr);
			break;
		default:
			assert (false);
	}
	return SmErrsVal!RExpr(r);
}

@Intr(IntrN.Not){
	@CallabilityChecker
	bool notCanCall(AValCT[] params){
		return params.length == 1 &&
			params[0].isVal &&
			params[0].valType.val.type == ADataType.Type.Bool;
	}
	@ExprTranslator
	SmErrsVal!RExpr notTranslate(string, Location pos, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
		RNotExpr r = new RNotExpr(params[0].toRExpr);
		r.pos = pos;
		return SmErrsVal!RExpr(r);
	}
}

@Intr(IntrN.To){
	@CallabilityChecker
	bool toCanCall(AValCT[] params){
		if (params.length != 2 ||
				!params[0].isVal ||
				!(!params[1].isVal && params[1].asType.isVal))
			return false;
		ADataType from = params[0].valType.val;
		ADataType target = params[1].asType.val;
		return from.canCastTo(target); // TODO: get ctx here
	}
	@ExprTranslator
	SmErrsVal!RExpr toTranslate(string, Location pos, STab,
			IdentU[], void[0][ASymbol*], RFn[string], AValCT[] params){
		RExpr param = params[0].toRExpr;
		RToExpr r = new RToExpr(param, params[1].asType.val);
		r.pos = pos;
		return SmErrsVal!RExpr(r);
	}
}
