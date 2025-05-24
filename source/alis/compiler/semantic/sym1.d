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
	if (node !in st.sMap || st.sMap[node] in st.dep)
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
	}

	void varIter(VarDef node, ref St st){
		if (node.isRecDep(st))
			return;
		ASymbol* sym = st.sMap[node];
		assert (sym);
		st.dep[sym] = (void[0]).init;
		scope(exit) st.dep.remove(sym);
	}

	void aliasIter(AliasDef node, ref St st){
		if (node.isRecDep(st))
			return;
		ASymbol* sym = st.sMap[node];
		assert (sym);
		st.dep[sym] = (void[0]).init;
		scope(exit) st.dep.remove(sym);
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
	It.exec(node, st);
	if (st.errs)
		return SmErrsVal!STab(st.errs);
	return SmErrsVal!STab(st.stab);
}
