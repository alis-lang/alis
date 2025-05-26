/++
Symbols Resolution, Level 1
+/
module alis.compiler.semantic.sym1;

import std.algorithm,
			 std.typecons,
			 std.range,
			 std.traits,
			 std.format,
			 std.meta;

import utils.ds;

debug import std.stdio;

import alis.common,
			 alis.compiler.common,
			 alis.compiler.semantic.common,
			 alis.compiler.semantic.error,
			 alis.compiler.semantic.expr,
			 alis.compiler.semantic.eval,
			 alis.compiler.semantic.types,
			 alis.compiler.ast,
			 alis.compiler.ast.iter,
			 alis.compiler.rst;

import meta;

private alias It = ItL!(mixin(__MODULE__), 1);

private struct St{
	/// errors
	SmErr[] errs;
	/// symbol table root
	STab stabR;
	/// symbol table, local
	STab stab;
	/// current context
	IdentU[] ctx;
	/// Symbol ASTNode to ASymbol* in STab mapping
	ASymbol*[ASTNode] sMap;
	/// set of symbols dependent on current call
	void[0][ASymbol*] dep;
}

/// Checks for recursive dependecy before processing an ASTNode
/// Returns: tre if recursive dependecy will happen after an ASTNode
private bool isRecDep(ASTNode node, ref St st){
	if (node !in st.sMap || st.sMap[node] !in st.dep)
		return false;
	st.errs ~= errRecDep(node.pos, st.sMap[node].ident.toString);
	return true;
}

@ItFn @ITL(1){
	void fnIter(FnDef node, ref St st){
		if (isRecDep(node, st))
			return;
		ASymbol* sym = st.sMap[node];
		assert (sym);
		st.dep[sym] = (void[0]).init;
		scope(exit) st.dep.remove(sym);

		if (node.vt !is null){
			st.errs ~= errUnsup(node.pos, "$vt");
		}
		// TODO: convert FnDef to RFn and AFn
	}

	void enumConstIter(EnumConstDef node, ref St st){
		if (node.isRecDep(st))
			return;
		ASymbol* sym = st.sMap[node];
		assert (sym);
		st.dep[sym] = (void[0]).init;
		scope(exit) st.dep.remove(sym);
		AEnumConst* symC = &sym.enumCS;

		immutable bool isAuto = cast(AutoExpr)node.type !is null;
		if (!isAuto){
			SmErrsVal!ADataType typeRes = eval4Type(node.type, st.stabR, st.ctx);
			if (typeRes.isErr){
				st.errs ~= typeRes.err;
				return;
			}
			symC.type = typeRes.val;
		}

		SmErrsVal!AValCT valRes = eval4Val(node.val, st.stabR, st.ctx);
		if (valRes.isErr){
			st.errs ~= valRes.err;
			return;
		}
		symC.data = valRes.val.dataL;
		if (isAuto)
			symC.type = valRes.val.typeL;
		if (!valRes.val.typeL.canCastTo(symC.type)){
			st.errs ~= errIncompatType(node.pos, symC.type, valRes.val.typeL);
			return;
		}
	}

	void enumSmIter(EnumSmDef node, ref St st){
		if (node.isRecDep(st))
			return;
		ASymbol* sym = st.sMap[node];
		assert (sym);
		st.dep[sym] = (void[0]).init;
		scope(exit) st.dep.remove(sym);
		AEnum* symC = &sym.enumS;

		immutable bool isAuto = cast(AutoExpr)node.type !is null;
		if (!isAuto){
			SmErrsVal!ADataType typeRes = eval4Type(node.type, st.stabR, st.ctx);
			if (typeRes.isErr){
				st.errs ~= typeRes.err;
				return;
			}
			symC.type = typeRes.val;
		}

		ADataType[] types;
		foreach (EnumMember member; node.members){
			if (member.value is null){
				st.errs ~= errEnumMemValMis(member);
				return;
			}
			SmErrsVal!AValCT valRes = eval4Val(member.value, st.stabR, st.ctx);
			if (valRes.isErr){
				st.errs ~= valRes.err;
				return;
			}
			types ~= valRes.val.typeL;
			AEnumMember amem;
			amem.val = valRes.val.dataL;
			amem.ident = symC.ident ~ member.name.IdentU;
			symC.members ~= amem;
		}

		if (isAuto){
			symC.type = commonType(types);
			if (symC.type == ADataType.ofNoInit){
				st.errs ~= errIncompatType(node);
				//return;
			}
		} else {
			foreach (size_t i, ref ADataType type; types){
				if (!type.canCastTo(symC.type))
					st.errs ~= errIncompatType(node.members[i].pos, symC.type, type);
			}
		}
	}

	void structIter(StructDef node, ref St st){
		if (node.isRecDep(st))
			return;
		ASymbol* sym = st.sMap[node];
		assert (sym);
		st.dep[sym] = (void[0]).init;
		scope(exit) st.dep.remove(sym);
		AStruct* symC = &sym.structS;
		Struct s = node.def;

		/// maps aliased name to alias name
		/// `aliasMap["alias_name"] = "the_real_thing"`
		string[string] aliasMap;
		void[0][string] nameSet;
		Visibility[string] aliasVis;
		foreach (AggMember memAbs; s.members){
			AggMemberAlias memAls = cast(AggMemberAlias)memAbs;
			if (memAls is null) continue;
			nameSet[memAls.name] = (void[0]).init;
			if (memAls.name in aliasMap){
				if (memAls.name == "this")
					st.errs ~= errMultiInherit(memAls.pos);
				else
					st.errs ~= errIdentReuse(memAls.pos, memAls.name);
				continue;
			}
			aliasMap[memAls.name] = memAls.val.ident;
			aliasVis[memAls.name] = memAls.visibility;
		}

		// remove indirect aliases
		while (true){
			bool done = true;
			foreach (string name; aliasMap){
				if (auto ptr = aliasMap[name] in aliasMap){
					aliasMap[name] = *ptr;
					done = false;
				}
			}
			if (done) break;
		}

		string bName = null;
		if (string* ptr = "this" in aliasMap)
			bName = *ptr;
		AggMemberNamed[] fields = s.members
			.map!(m => cast(AggMemberNamed)m).filter!(m => m !is null).array;
		bool erred = false;
		foreach (AggMemberNamed field; fields){
			if (field.name == "_") continue;
			if (field.name in nameSet){
				st.errs ~= errIdentReuse(field.pos, field.name);
				erred = true;
				continue;
			}
			if (field.name == "this"){
				st.errs ~= errFieldThis(field.pos);
				erred = true;
				continue;
			}
			nameSet[field.name] = (void[0]).init;
		}
		if (erred)
			return;
		if (bName.length){
			immutable size_t ind = fields.countUntil!(f => f.name == bName);
			AggMemberNamed tmp = fields[ind];
			for (size_t i = ind; i > 0; i --)
				fields[i] = fields[i - 1];
			fields[0] = tmp;
		}

		foreach (AggMemberNamed field; fields){
			immutable bool isAuto = cast(AutoExpr)field.type !is null;
			if (isAuto && field.val is null){
				st.errs ~= errAutoNoVal(field.pos);
				continue;
			}
			SmErrsVal!ADataType typeRes = eval4Type(field.type, st.stabR, st.ctx);
			if (typeRes.isErr){
				st.errs ~= typeRes.err;
				continue;
			}
			ADataType type = typeRes.val;
			AValCT val;
			if (field.val){
				SmErrsVal!AValCT valRes = eval4Val(field.val, st.stabR, st.ctx);
				if (valRes.isErr){
					st.errs ~= valRes.err;
					continue;
				}
				val = valRes.val;
				if (isAuto){
					if (!val.typeL.canCastTo(type)){
						st.errs ~= errTypeMis(field, type, val.typeL);
						continue;
					}
					type = val.typeL;
				}
				foreach (string name; aliasMap.byKey
						.filter!(n => aliasMap[n] == field.name)){
					symC.names[name] = symC.types.length;
					symC.nameVis[name] = aliasVis[name];
				}
				symC.names[field.name] = symC.types.length;
				symC.nameVis[field.name] = field.visibility;
				symC.types ~= type;
				symC.initD ~= val.dataL;
			} else {
				// TODO: ask ADataType for initD
				st.errs ~= errUnsup(field.pos, "no default value for struct field");
			}
		}
	}

	void varIter(VarDef node, ref St st){
		if (node.isRecDep(st))
			return;
		ASymbol* sym = st.sMap[node];
		assert (sym);
		st.dep[sym] = (void[0]).init;
		scope(exit) st.dep.remove(sym);
		AVar* symC = &sym.varS;
		symC.isGlobal = st.ctx.length <= 1;
		symC.offset = size_t.max;
		immutable bool isAuto = cast(AutoExpr)node.type !is null;
		if (!isAuto){
			SmErrsVal!ADataType typeVal = eval4Type(node.type, st.stabR, st.ctx);
			if (typeVal.isErr){
				st.errs ~= typeVal.err;
				return;
			}
			symC.type = typeVal.val;
		}
		if (node.value){
			SmErrsVal!AValCT valVal = eval4Val(node.value, st.stabR, st.ctx);
			if (valVal.isErr){
				st.errs ~= valVal.err;
				return;
			}
			if (isAuto){
				symC.type = valVal.val.typeL;
			} else if (!valVal.val.typeL.canCastTo(symC.type)){
				st.errs ~= errTypeMis(node.value, symC.type, valVal.val.typeL);
				return;
			}
		} else if (isAuto){
			st.errs ~= errAutoNoVal(node.pos);
			return;
		}
	}

	void aliasIter(AliasDef node, ref St st){
		if (node.isRecDep(st))
			return;
		ASymbol* sym = st.sMap[node];
		assert (sym);
		st.dep[sym] = (void[0]).init;
		scope(exit) st.dep.remove(sym);
		AAlias* symC = &sym.aliasS;
		SmErrsVal!RExpr exprVal = resolve(node.val, st.stabR, st.ctx);
		if (exprVal.isErr){
			st.errs ~= exprVal.err;
			return;
		}
		symC.expr = exprVal.val;
	}

	void unionIter(UnionDef node, ref St st){
		if (node.isRecDep(st))
			return;
		ASymbol* sym = st.sMap[node];
		assert (sym);
		st.dep[sym] = (void[0]).init;
		scope(exit) st.dep.remove(sym);
	}

	void utestIter(UTest node, ref St st){
		if (node.isRecDep(st))
			return;
		ASymbol* sym = st.sMap[node];
		assert (sym);
		st.dep[sym] = (void[0]).init;
		scope(exit) st.dep.remove(sym);
	}
}

/// Builds Level 1 Symbol Table
/// Returns: Level 1 Symbol Table, or SmErr[]
package SmErrsVal!STab stab1Of(ASTNode node, STab stab, ASymbol*[ASTNode] sMap,
		void[0][ASymbol*] dep, IdentU[] ctx = null){
	St st;
	st.stabR = stab;
	st.stab = stab;
	st.ctx = ctx.dup;
	st.sMap = sMap;
	st.dep = dep;
	It.exec(node, st);
	if (st.errs)
		return SmErrsVal!STab(st.errs);
	return SmErrsVal!STab(st.stab);
}
