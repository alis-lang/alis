/++
Expression Resolution (Expression -> RExpr conversion)
+/
module alis.compiler.semantic.expr;

import alis.common,
			 alis.compiler.common,
			 alis.compiler.semantic.common,
			 alis.compiler.semantic.error,
			 alis.compiler.semantic.sym0,
			 alis.compiler.semantic.eval,
			 alis.compiler.semantic.stmnt,
			 alis.compiler.semantic.typeofexpr,
			 alis.compiler.semantic.types,
			 alis.compiler.semantic.call,
			 alis.compiler.ast,
			 alis.compiler.ast.iter,
			 alis.compiler.ast.rst;

import alis.compiler.semantic.sym1 : unionNamedDo, unionUnnamedDo, structDo;

import meta;

import std.algorithm,
			 std.array,
			 std.range,
			 std.conv,
			 std.format;

debug import std.stdio;

private struct St{
	/// errors
	SmErr[] errs;
	/// root stab
	STab stabR;
	/// local STab
	STab stab;
	/// context
	IdentU[] ctx;
	/// symbols dependent on current call
	void[0][ASymbol*] dep;
	/// resulting expression
	RExpr res;
	/// parameter types if resuslting expression is expected to be callable
	AValCT[] params;
	/// if there is any expected type
	bool isExpT = false;
	/// expected type, if `isExpT`
	ADataType expT;
	/// `RFn` for each `AFn.uid`
	RFn[string] fns;
}

private alias It = ItL!(mixin(__MODULE__), 0);

/// Verifies if return type of expression matches expected type, adding
/// appropriate errors
/// Returns: true if error-free, false if error added
private bool expT(Location pos, RExpr expr, ref St st){
	if (!st.isExpT) return true;
	SmErrsVal!ADataType typeRes = typeOf(expr, st.stabR, st.ctx);
	if (typeRes.isErr){
		st.errs ~= typeRes.err;
		return false;
	}
	return expT(pos, typeRes.val, st);
}

/// ditto
private bool expT(Location pos, ADataType type, ref St st){
	if (!st.isExpT) return true;
	if (!type.canCastTo(st.expT)){
		st.errs ~= errIncompatType(pos, st.expT.toString,
				type.toString);
		return false;
	}
	return true;
}

@ItFn @ITL(0){
	void identExprIter(IdentExpr node, ref St st){
		ASymbol* res;
		if (st.params.length){
			foreach (range; st.stabR.find(node.ident.IdentU, st.ctx)){
				size_t best = size_t.max;
				size_t count = 0;
				foreach (ASymbol* sym; range.filter!(s => s.isCallable)){
					immutable size_t callability = callabilityOf(sym, st.params);
					if (callability == size_t.max) continue;
					if (callability == best){
						count ++;
					} else
					if (callability < best){
						best = callability;
						count = 1;
						res = sym;
					}
				}
				if (count == 0){
					st.errs ~= errUndef(node.pos, node.ident);
					return;
				}
				if (count > 1){
					st.errs ~= errCallableConflict(node.pos, node.ident,
							st.params.map!(p => p.toString));
				}
			}
		} else {
			bool found = false;
			foreach (symR; st.stabR.find(node.ident.IdentU, st.ctx)){
				auto syms = symR;
				foreach (ASymbol* s; symR){
					if (found){
						st.errs ~= errIdentAmbig(node.pos, node.ident,
								syms.map!(s => s.ident.toString));
						return;
					}
					found = true;
					res = s;
				}
			}
		}
		if (res is null){
			st.errs ~= errUndef(node.pos, node.ident);
			return;
		}
		// convert res to RExpr
		RExpr r;
		switch (res.type){
			case ASymbol.Type.Struct:
				r = new RAValCTExpr(ADataType.of(&res.structS).AValCT);
				break;
			case ASymbol.Type.Union:
				r = new RAValCTExpr(ADataType.of(&res.unionS).AValCT);
				break;
			case ASymbol.Type.Enum:
				r = new RAValCTExpr(ADataType.of(&res.enumS).AValCT);
				break;
			case ASymbol.Type.EnumConst:
				r = new RAValCTExpr(AValCT(res.enumCS.type, res.enumCS.data));
				break;
			case ASymbol.Type.Fn:
				r = new RFnExpr(res.fnS);
				break;
			case ASymbol.Type.Var:
				r = new RVarExpr(res.varS);
				break;
			case ASymbol.Type.Alias:
			case ASymbol.Type.Import:
			case ASymbol.Type.Template:
				st.errs ~= errUnsup(node);
				return;
			default:
				st.errs ~= errUnsup(node.pos, res.type.to!string);
				return;
		}
		if (expT(node.pos, r, st)){} // the empty if is to make LSP shut up
	}

	void blockExprIter(BlockExpr node, ref St st){
		RBlockExpr r = new RBlockExpr;
		r.pos = node.pos;
		r.block = new RBlock;
		r.block.pos = node.block.pos;
		immutable bool isAuto = cast(AutoExpr)node.type !is null;
		foreach (Statement stmnt; node.block.statements){
			SmErrsVal!RStatement stmntRes = resolveStmnt(stmnt, st.stabR, st.ctx,
					st.dep, st.fns);
			if (stmntRes.isErr){
				st.errs ~= stmntRes.err;
				continue;
			}
			r.block.statements ~= stmntRes.val;
		}
		SmErrsVal!ADataType typeRes = typeOf(r, st.stabR, st.ctx);
		if (typeRes.isErr){
			st.errs ~= typeRes.err;
			return;
		}
		if (isAuto){
			r.type = typeRes.val;
		} else {
			SmErrsVal!ADataType xtypeRes = eval4Type(node.type, st.stabR, st.ctx,
					st.dep, st.fns);
			if (xtypeRes.err){
				st.errs ~= xtypeRes.err;
				return;
			}
			r.type = xtypeRes.val;
			if (!typeRes.val.canCastTo(r.type)){
				st.errs ~= errIncompatType(node.pos, xtypeRes.val.toString,
						typeRes.val.toString);
				return;
			}
		}
		if (st.params.length && r.type.callabilityOf(st.params) == size_t.max){
			st.errs ~= errCallableIncompat(node.pos, r.type.toString,
					st.params.map!(p => p.toString));
			return;
		}
		if (!expT(node.pos, r, st)) return;
		st.res = r;
	}

	void commaExprIter(CommaExpr node, ref St st){
		if (st.params.length){
			st.errs ~= errCallableIncompat(node.pos, "expression list",
					st.params.map!(p => p.toString));
			return;
		}
		RCommaExpr r = new RCommaExpr;
		r.pos = node.pos;
		foreach (Expression expr; node.exprs){
			SmErrsVal!RExpr exprRes = resolve(expr, st.stabR, st.ctx, st.dep,
					st.fns);
			if (exprRes.isErr){
				st.errs ~= exprRes.err;
				return;
			}
			r.exprs ~= exprRes.val;
		}
		if (!expT(node.pos, r, st)) return;
		st.res = r;
	}

	void structAnonIter(StructAnon node, ref St st){
		if (st.params.length){
			st.errs ~= errCallableIncompat(node.pos, "struct",
					st.params.map!(p => p.toString));
			return;
		}
		immutable string name = format!"struct$_%d_%d_$"(
				node.pos.line, node.pos.col);
		ASymbol *sym = new ASymbol(AStruct(st.ctx ~ name.IdentU));
		st.stab.add(name.IdentU, sym, st.ctx);
		sym.structS.vis = Visibility.Default;
		SmErr[] errs = structDo(node.val, &sym.structS, st.stabR, st.ctx, st.dep);
		if (errs.length){
			st.errs ~= errs;
			return;
		}
		sym.isComplete = true;
		RExpr r = new RAValCTExpr(ADataType.of(&sym.structS).AValCT);
		if (!expT(node.pos, r, st)) return;
		st.res = r;
	}

	void unionAnonIter(UnionAnon node, ref St st){
		if (st.params.length){
			st.errs ~= errCallableIncompat(node.pos, "union",
					st.params.map!(p => p.toString));
			return;
		}
		immutable string name = format!"union$_%d_%d_$"(
				node.pos.line, node.pos.col);
		ASymbol *sym = new ASymbol(AUnion(st.ctx ~ name.IdentU));
		st.stab.add(name.IdentU, sym, st.ctx);
		sym.structS.vis = Visibility.Default;
		SmErr[] errs;
		if (NamedUnion sub = cast(NamedUnion)node.val){
			errs = unionNamedDo(sub, sym, st.stabR, st.ctx, st.dep);
		} else
		if (UnnamedUnion sub = cast(UnnamedUnion)node.val){
			errs = unionUnnamedDo(sub, sym, st.stabR, st.ctx, st.dep);
		}
		if (errs.length){
			st.errs ~= errs;
			return;
		}
		sym.isComplete = true;
		RExpr r = new RAValCTExpr(ADataType.of(&sym.unionS).AValCT);
		if (!expT(node.pos, r, st)) return;
		st.res = r;
	}

	void fnAnonExprIter(FnAnonExpr node, ref St st){
		immutable string name = format!"fn$_%d_%d_$"(node.pos.line, node.pos.col);
		ASymbol *sym = new ASymbol(AFn(st.ctx ~ name.IdentU));
		AFn *symC = &sym.fnS;
		symC.vis = Visibility.Default;
		symC.uid = st.ctx.toString ~ name;
		symC.isAlisFn = true;

		void[0][string] nameSet;
		foreach (size_t i, FParam param; node.params.params){
			if (param.name != "_" && param.name in nameSet){
				st.errs ~= errIdentReuse(param.pos, param.name);
				continue;
			}
			nameSet[param.name] = (void[0]).init;
			immutable bool isAuto =
				cast(AutoExpr)param.type !is null || param.type is null;
			ADataType type;
			if (!isAuto){
				SmErrsVal!ADataType typeRes = eval4Type(param.type, st.stabR, st.ctx,
						st.dep, st.fns);
				if (typeRes.isErr)
					st.errs ~= typeRes.err;
				type = typeRes.val;
			}
			if (param.val !is null){
				st.errs ~= errFnAnonParamDef(param.pos, param.name);
				continue;
			}
			if (isAuto){
				st.errs ~= errUnsup(param.pos,
						"inferring type for anonymous function params");
				continue;
			}
			symC.paramsV ~= type.initB;
			symC.paramsN ~= param.name;
			symC.paramsT ~= type;
		}
		RFn r = new RFn;
		r.pos = node.pos;
		r.ident = symC.ident.toString;
		r.paramsT = symC.paramsT;
		r.paramsN = symC.paramsN;
		symC.uid = fnNameEncode(symC.ident.toString, symC.paramsT);
		st.fns[symC.uid] = r;
		st.stab.add(name.IdentU, sym, st.ctx);

		if (st.params.length && sym.callabilityOf(st.params) == size_t.max){
			st.errs ~= errCallableIncompat(node.pos, sym.ident.toString,
					st.params.map!(p => p.toString));
			return;
		}
		RExpr res = new RFnExpr(*symC);
		if (!expT(node.pos, res, st)) return;
		st.res = res;
	}

	void structLiteralExprIter(StructLiteralExpr node, ref St st){
		if (st.params.length){
			st.errs ~= errCallableIncompat(node.pos, "struct literal",
					st.params.map!(p => p.toString));
			return;
		}
		RStructLiteralExpr r = new RStructLiteralExpr;
		r.pos = node.pos;
		void[0][string] nameSet;
		foreach (KeyVal kv; node.keyVals){
			if (kv.key in nameSet){
				st.errs ~= errIdentReuse(kv.pos, kv.key);
				continue;
			}
			nameSet[kv.key] = (void[0]).init;
			SmErrsVal!RExpr exprRes = resolve(kv.val, st.stabR, st.ctx, st.dep,
					st.fns);
			if (exprRes.isErr){
				st.errs ~= exprRes.err;
				continue;
			}
			r.names ~= kv.key;
			r.vals ~= exprRes.val;
		}
		if (!expT(node.pos, r, st)) return;
		st.res = r;
	}

	void boolLiteralExprIter(BoolLiteralExpr node, ref St st){
		if (st.params.length){
			st.errs ~= errCallableIncompat(node.pos, "bool literal",
					st.params.map!(p => p.toString));
			return;
		}
		RLiteralExpr r = new RLiteralExpr;
		r.pos = node.pos;
		r.type = ADataType.ofBool;
		r.value = node.val.asBytes;
		if (!expT(node.pos, r, st)) return;
		st.res = r;
	}

	void literalIntExprIter(LiteralIntExpr node, ref St st){
		if (st.params.length){
			st.errs ~= errCallableIncompat(node.pos, "int literal",
					st.params.map!(p => p.toString));
			return;
		}
		RLiteralExpr r = new RLiteralExpr;
		r.pos = node.pos;
		r.type = ADataType.ofInt;
		r.value = node.val.asBytes;
		if (!expT(node.pos, r, st)) return;
		st.res = r;
	}

	void literalFloatExprIter(LiteralFloatExpr node, ref St st){
		if (st.params.length){
			st.errs ~= errCallableIncompat(node.pos, "float literal",
					st.params.map!(p => p.toString));
			return;
		}
		RLiteralExpr r = new RLiteralExpr;
		r.pos = node.pos;
		r.type = ADataType.ofFloat;
		r.value = node.val.asBytes;
		if (!expT(node.pos, r, st)) return;
		st.res = r;
	}

	void literalStringExprIter(LiteralStringExpr node, ref St st){
		if (st.params.length){
			st.errs ~= errCallableIncompat(node.pos, "string literal",
					st.params.map!(p => p.toString));
			return;
		}
		RLiteralExpr r = new RLiteralExpr;
		r.pos = node.pos;
		r.type = ADataType.ofString;
		r.value = cast(ubyte[])(node.val.dup);
		if (!expT(node.pos, r, st)) return;
		st.res = r;
	}

	void literalCharExprIter(LiteralCharExpr node, ref St st){
		if (st.params.length){
			st.errs ~= errCallableIncompat(node.pos, "char literal",
					st.params.map!(p => p.toString));
			return;
		}
		RLiteralExpr r = new RLiteralExpr;
		r.pos = node.pos;
		r.type = ADataType.ofChar(8);
		r.value = [cast(ubyte)node.val];
		if (!expT(node.pos, r, st)) return;
		st.res = r;
	}

	void literalArrayExprIter(LiteralArrayExpr node, ref St st){
		if (st.params.length){
			st.errs ~= errCallableIncompat(node.pos, "array literal",
					st.params.map!(p => p.toString));
			return;
		}
		RArrayLiteralExpr r = new RArrayLiteralExpr;
		r.pos = node.pos;
		foreach (Expression elem; node.elements){
			SmErrsVal!RExpr exprRes = resolve(elem, st.stabR, st.ctx, st.dep,
					st.fns);
			if (exprRes.isErr){
				st.errs ~= exprRes.err;
				return;
			}
			r.elements ~= exprRes.val;
		}
		if (!expT(node.pos, r, st)) return;
		st.res = r;
	}

	void autoExprIter(AutoExpr node, ref St st){
		st.errs ~= errUnsup(node); // what even is `auto` doing here
	}

	void thisExprIter(ThisExpr node, ref St st){
		st.errs ~= errUnsup(node.pos, "vtable");
	}

	void intExprIter(IntExpr node, ref St st){
		if (st.params.length > 1){
			st.errs ~= errCallableIncompat(node.pos, node.ident,
					st.params.map!(p => p.toString));
			return;
		}
		RAValCTExpr r = new RAValCTExpr;
		r.pos = node.pos;
		r.res = ADataType.ofInt.AValCT;
		if (!expT(node.pos, r, st)) return;
		st.res = r;
	}

	void uIntExprIter(UIntExpr node, ref St st){
		if (st.params.length > 1){
			st.errs ~= errCallableIncompat(node.pos, node.ident,
					st.params.map!(p => p.toString));
			return;
		}
		RAValCTExpr r = new RAValCTExpr;
		r.pos = node.pos;
		r.res = ADataType.ofUInt.AValCT;
		if (!expT(node.pos, r, st)) return;
		st.res = r;
	}

	void floatExprIter(FloatExpr node, ref St st){
		if (st.params.length > 1){
			st.errs ~= errCallableIncompat(node.pos, node.ident,
					st.params.map!(p => p.toString));
			return;
		}
		RAValCTExpr r = new RAValCTExpr;
		r.pos = node.pos;
		r.res = ADataType.ofFloat.AValCT;
		if (!expT(node.pos, r, st)) return;
		st.res = r;
	}

	void charExprIter(CharExpr node, ref St st){
		if (st.params.length > 1){
			st.errs ~= errCallableIncompat(node.pos, node.ident,
					st.params.map!(p => p.toString));
			return;
		}
		RAValCTExpr r = new RAValCTExpr;
		r.pos = node.pos;
		r.res = ADataType.ofChar(8).AValCT;
		if (!expT(node.pos, r, st)) return;
		st.res = r;
	}

	void stringExprIter(StringExpr node, ref St st){
		if (st.params.length > 1){
			st.errs ~= errCallableIncompat(node.pos, node.ident,
					st.params.map!(p => p.toString));
			return;
		}
		RAValCTExpr r = new RAValCTExpr;
		r.pos = node.pos;
		r.res = ADataType.ofString.AValCT;
		if (!expT(node.pos, r, st)) return;
		st.res = r;
	}

	void boolExprIter(BoolExpr node, ref St st){
		if (st.params.length > 1){
			st.errs ~= errCallableIncompat(node.pos, node.ident,
					st.params.map!(p => p.toString));
			return;
		}
		RAValCTExpr r = new RAValCTExpr;
		r.pos = node.pos;
		r.res = ADataType.ofBool.AValCT;
		if (!expT(node.pos, r, st)) return;
		st.res = r;
	}

	void opPostExprOverridableIter(OpPostExprOverridable node, ref St st){
		OpCallExpr call = new OpCallExpr;
		call.pos = node.pos;
		IdentExpr callee = new IdentExpr;
		callee.pos = node.pos;
		callee.ident = "opPost";
		call.callee = callee;
		LiteralStringExpr op = new LiteralStringExpr;
		op.pos = node.pos;
		op.val = node.op;
		call.params = [op, node.operand];
		opCallExprIter(call, st);
	}

	void opPreExprOverridableIter(OpPreExprOverridable node, ref St st){
		OpCallExpr call = new OpCallExpr;
		call.pos = node.pos;
		IdentExpr callee = new IdentExpr;
		callee.pos = node.pos;
		callee.ident = "opPre";
		call.callee = callee;
		LiteralStringExpr op = new LiteralStringExpr;
		op.pos = node.pos;
		op.val = node.op;
		call.params = [op, node.operand];
		opCallExprIter(call, st);
	}

	void opBinExprOverridableIter(OpBinExprOverridable node, ref St st){
		OpCallExpr call = new OpCallExpr;
		call.pos = node.pos;
		IdentExpr callee = new IdentExpr;
		callee.pos = node.pos;
		callee.ident = "opPre";
		call.callee = callee;
		LiteralStringExpr op = new LiteralStringExpr;
		op.pos = node.pos;
		op.val = node.op;
		call.params = [op, node.lhs, node.rhs];
		opCallExprIter(call, st);
	}

	void intrinsicExprIter(IntrinsicExpr node, ref St st){
		RIntrinsicExpr r = new RIntrinsicExpr;
		r.pos = node.pos;
		r.name = node.name;
		st.res = r;
		// TODO: error out if intrinsic not callable && st.params.length
	}

	void opCallExprIter(OpCallExpr node, ref St st){
		AValCT[] params;
		RExpr[] paramsExpr;
		foreach (Expression p; node.params){
			SmErrsVal!RExpr exprRes = resolve(p, st.stabR, st.ctx, st.dep, st.fns);
			if (exprRes.isErr){
				st.errs ~= exprRes.err;
				continue;
			}
			paramsExpr ~= exprRes.val;
			SmErrsVal!AValCT pRes = eval(paramsExpr[$ - 1], st.stabR, st.ctx);
			if (pRes.isErr){
				st.errs ~= pRes.err;
				continue;
			}
			params ~= pRes.val;
		}
		if (params.length != node.params.length) return;
		params = params.flatten;
		RExpr callee; {
			SmErrsVal!RExpr calleeRes = resolve(node.callee, st.stabR, st.ctx,
					st.dep, st.fns, params);
			if (calleeRes.isErr){
				st.errs ~= calleeRes.err;
				return;
			}
			callee = calleeRes.val;
		}

		RExpr r;
		if (RFnPartCallExpr pFnCall = cast(RFnPartCallExpr)callee){
			pFnCall.params ~= paramsExpr;
			r = pFnCall;
			st.res = pFnCall;
		} else
		if (RTmPartInitExpr pTmCall = cast(RTmPartInitExpr)callee){
			st.errs ~= errUnsup(node.pos, "Template instantiation");
			// add node.params to pTmCall.tparams
			// if st.params, result should be callable with st.params:
			// pTmCall(.., node.params)(st.params)
			r = pTmCall;
		} else
		if (RIntrinsicPartCallExpr iCall = cast(RIntrinsicPartCallExpr)callee){
			iCall.params ~= paramsExpr;
			r = iCall;
		} else {
			RFnCallExpr call = new RFnCallExpr;
			call.pos = node.pos;
			call.callee = callee;
			call.params = paramsExpr;
			r = call;
		}
		if (!expT(node.pos, r, st)) return;
		SmErrsVal!ADataType typeRes = typeOf(r, st.stabR, st.ctx);
		if (typeRes.isErr){
			st.errs ~= typeRes.err;
			return;
		}
		if (st.params.length && typeRes.val.callabilityOf(st.params) == size_t.max){
			st.errs ~= errCallableIncompat(node.pos, typeRes.val.toString,
					st.params.map!(p => p.toString));
			return;
		}
		st.res = r;
	}

	void opDotBinIter(OpDotBin node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}

	void opIndexExprIter(OpIndexExpr node, ref St st){
		RExpr sub; {
			SmErrsVal!RExpr subRes = resolve(node.lhs, st.stabR, st.ctx, st.dep,
					st.fns);
			if (subRes.isErr){
				st.errs ~= subRes.err;
				return;
			}
			sub = subRes.val;
		}
		ADataType subType; {
			SmErrsVal!ADataType subTypeRes = typeOf(sub, st.stabR, st.ctx);
			if (subTypeRes.isErr){
				st.errs ~= subTypeRes.err;
				return;
			}
			subType = subTypeRes.val;
		}

		if (subType.type == ADataType.Type.Seq){
			if (node.indexes.length != 0){
				st.errs ~= errParamCount(node, "index", 1, node.indexes.length);
				return;
			}
			AValCT ind; {
				SmErrsVal!AValCT indRes = eval(node.indexes[0], st.stabR, st.ctx,
						st.dep, st.fns);
				if (indRes.isErr){
					st.errs ~= indRes.err;
					return;
				}
				ind = indRes.val;
			}
			if (ind.type != AValCT.Type.Literal){
				st.errs ~= errIncompatType(node.indexes[0].pos, "uint literal",
						ind.toString);
				return;
			}
			if (!ind.typeT.canCastTo(ADataType.ofUInt)){
				st.errs ~= errIncompatType(node.indexes[0].pos, "uint",
						ind.typeT.toString);
				return;
			}
			AValCT subVal; {
				SmErrsVal!AValCT subValRes = eval(sub, st.stabR, st.ctx);
				if (subValRes.isErr){
					st.errs ~= subValRes.err;
					return;
				}
				subVal = subValRes.val;
			}
			if (subVal.type != AValCT.Type.Seq){
				st.errs ~= errIncompatType(node.lhs.pos, "sequence", sub.toString);
				return;
			}
			ind = ind.to(ADataType.ofUInt).val;
			size_t indI = ind.dataL.as!size_t;
			if (indI >= subVal.seq.length){
				st.errs ~= errBounds(node.indexes[0].pos, subVal.seq.length, indI);
				return;
			}
			if (st.params.length &&
					subVal.seq[indI].callabilityOf(st.params) == size_t.max){
				st.errs ~= errCallableIncompat(node.pos, subVal.seq[indI].toString,
						st.params.map!(p => p.toString));
				return;
			}
			RAValCTExpr r = new RAValCTExpr;
			r.pos = node.pos;
			r.res = subVal.seq[indI];
			if (!expT(node.pos, r, st)) return;
			st.res = r;
			return;
		}

		RExpr[] params;
		ADataType[] paramsT;
		foreach (Expression index; node.indexes){
			SmErrsVal!RExpr exprRes = resolve(index, st.stabR, st.ctx, st.dep,
					st.fns);
			if (exprRes.isErr){
				st.errs ~= exprRes.err;
				continue;
			}
			params ~= exprRes.val;
			SmErrsVal!ADataType typeRes = typeOf(params[$ - 1], st.stabR, st.ctx);
			if (typeRes.isErr){
				st.errs ~= typeRes.err;
				continue;
			}
			paramsT ~= typeRes.val;
		}
		if (params.length != node.indexes.length ||
				params.length != paramsT.length)
			return;

		if ((subType.type == ADataType.Type.Array ||
					subType.type == ADataType.Type.Slice) &&
				node.indexes.length == 1 &&
				paramsT[0].canCastTo(ADataType.ofUInt)){
			if (st.params.length &&
					(*subType.refT).callabilityOf(st.params) == size_t.max){
				st.errs ~= errCallableIncompat(node.pos, subType.refT.toString,
						st.params.map!(p => p.toString));
				return;
			}
			RIntrinsicCallExpr r = new RIntrinsicCallExpr;
			r.pos = node.pos;
			r.name = "arrInd";
			r.params = [sub, params[0]];
			if (!expT(node.pos, r, st)) return;
			st.res = r;
			return;
		}

		OpCallExpr call = new OpCallExpr;
		call.pos = node.pos;
		IdentExpr callee = new IdentExpr;
		callee.pos = node.pos;
		callee.ident = "opIndex";
		call.callee = callee;
		call.params = node.lhs ~ node.indexes;
		opCallExprIter(call, st);
	}

	void opAssignBinIter(OpAssignBin node, ref St st){
		if (st.params.length){
			st.errs ~= errNotCallable(node.pos, "assignment operation");
			return;
		}
		if (st.isExpT){
			st.errs ~= errIncompatType(node.pos, st.expT.toString, "struct{}");
			return;
		}
		st.errs ~= errUnsup(node); // TODO: implement
	}

	void opAssignRefBinIter(OpAssignRefBin node, ref St st){
		if (st.params.length){
			st.errs ~= errNotCallable(node.pos, "reference assignment operation");
			return;
		}
		if (st.isExpT){
			st.errs ~= errIncompatType(node.pos, st.expT.toString, "struct{}");
			return;
		}
		st.errs ~= errUnsup(node); // TODO: implement
	}

	void opRefPostIter(OpRefPost node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}

	void opDotsPostIter(OpDotsPost node, ref St st){
		st.errs ~= errUnsup(node);
	}

	void opIsPreIter(OpIsPre node, ref St st){
		if (st.params.length){
			st.errs ~= errNotCallable(node.pos, "is prefix");
			return;
		}
		st.errs ~= errUnsup(node); // TODO: implement
	}

	void opNotIsPreIter(OpNotIsPre node, ref St st){
		if (st.params.length){
			st.errs ~= errNotCallable(node.pos, "!is prefix");
			return;
		}
		st.errs ~= errUnsup(node); // TODO: implement
	}

	void opConstPreIter(OpConstPre node, ref St st){
		if (st.params.length){
			st.errs ~= errNotCallable(node.pos, "const prefix");
			return;
		}
		st.errs ~= errUnsup(node); // TODO: implement
	}

	void opRefPreIter(OpRefPre node, ref St st){
		if (st.params.length){
			st.errs ~= errNotCallable(node.pos, "reference");
			return;
		}
		st.errs ~= errUnsup(node); // TODO: implement
	}

	void opTagPreIter(OpTagPre node, ref St st){
		st.errs ~= errUnsup(node.pos, "`#` operator in expressions");
	}

	void opArrowBinIter(OpArrowBin node, ref St st){
		st.errs ~= errUnsup(node.pos, "vtable");
	}

	void opCommaBinIter(OpCommaBin node, ref St st){
		st.errs ~= errUnxp(node.pos, "OpCommaBin should not have happened");
	}

	void opColonBinIter(OpColonBin node, ref St st){
		if (st.params.length){
			st.errs ~= errNotCallable(node.pos, "type-cast check operation");
			return;
		}
		st.errs ~= errUnsup(node); // TODO: implement
	}

	void opIsBinIter(OpIsBin node, ref St st){
		if (st.params.length){
			st.errs ~= errNotCallable(node.pos, "is comparison");
			return;
		}
		st.errs ~= errUnsup(node); // TODO: implement
	}

	void opNotIsBinIter(OpNotIsBin node, ref St st){
		if (st.params.length){
			st.errs ~= errNotCallable(node.pos, "!is comparison");
			return;
		}
		st.errs ~= errUnsup(node); // TODO: implement
	}

	void opNotPostIter(OpNotPost node, ref St st){
		st.errs ~= errUnsup(node.pos, "error handling"); // TODO: implement
	}

	void opQPostIter(OpQPost node, ref St st){
		st.errs ~= errUnsup(node.pos, "error handling"); // TODO: implement
	}

	void opNotNotBinIter(OpNotNotBin node, ref St st){
		st.errs ~= errUnsup(node.pos, "error handling"); // TODO: implement
	}

	void opQQBinIter(OpQQBin node, ref St st){
		st.errs ~= errUnsup(node.pos, "error handling"); // TODO: implement
	}
}

/// Resolves Expression to RExpr
/// Params:
/// - `expr` - The expression to resolve
/// - `stab` - The root level Symbol Table
/// - `ctx` - Context where the `expr` occurs
/// - `params` - Ensure result is callable with `params`
/// Returns: RExpr or SmErr[]
pragma(inline, true)
package SmErrsVal!RExpr resolve(Expression expr, STab stabR, IdentU[] ctx,
		void[0][ASymbol*] dep, RFn[string] fns, AValCT[] params){
	assert (fns);
	St st;
	st.dep = dep;
	st.ctx = ctx.dup;
	st.stabR = stabR;
	st.stab = stabR.findSt(ctx, ctx);
	st.fns = fns;
	st.params = params.flatten;
	It.exec(expr, st);
	if (st.errs.length)
		return SmErrsVal!RExpr(st.errs);
	if (st.res is null)
		return SmErrsVal!RExpr([errUnxp(expr.pos, "resolve expr -> null")]);
	return SmErrsVal!RExpr(st.res);
}

/// Resolves Expression to RExpr
/// Params:
/// - `expr` - The expression to resolve
/// - `stab` - The root level Symbol Table
/// - `ctx` - Context where the `expr` occurs
/// - `expT` - Expected Data Type for resulting expression
/// Returns: RExpr or SmErr[]
pragma(inline, true)
package SmErrsVal!RExpr resolve(Expression expr, STab stabR, IdentU[] ctx,
		void[0][ASymbol*] dep, RFn[string] fns, ADataType expT){
	assert (fns);
	St st;
	st.dep = dep;
	st.ctx = ctx.dup;
	st.stabR = stabR;
	st.stab = stabR.findSt(ctx, ctx);
	st.fns = fns;
	st.isExpT = true;
	st.expT = expT;
	It.exec(expr, st);
	if (st.errs.length)
		return SmErrsVal!RExpr(st.errs);
	if (st.res is null)
		return SmErrsVal!RExpr([errUnxp(expr.pos, "resolve expr -> null")]);
	return SmErrsVal!RExpr(st.res);
}

/// Resolves Expression to RExpr
/// Params:
/// - `expr` - The expression to resolve
/// - `stab` - The root level Symbol Table
/// - `ctx` - Context where the `expr` occurs
/// Returns: RExpr or SmErr[]
pragma(inline, true)
package SmErrsVal!RExpr resolve(Expression expr, STab stabR, IdentU[] ctx,
		void[0][ASymbol*] dep, RFn[string] fns){
	assert (fns);
	St st;
	st.dep = dep;
	st.ctx = ctx.dup;
	st.stabR = stabR;
	st.stab = stabR.findSt(ctx, ctx);
	st.fns = fns;
	It.exec(expr, st);
	if (st.errs.length)
		return SmErrsVal!RExpr(st.errs);
	if (st.res is null)
		return SmErrsVal!RExpr([errUnxp(expr.pos, "resolve expr -> null")]);
	return SmErrsVal!RExpr(st.res);
}
