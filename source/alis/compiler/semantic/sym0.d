/++
Symbols Resolution, Level 0
+/
module alis.compiler.semantic.sym0;

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
			 alis.compiler.ast,
			 alis.compiler.ast.iter;

import meta;

private alias It = ItL!(mixin(__MODULE__), 0);

private struct St{
	/// errors
	SmErr[] errs;
	/// symbol table
	STab stab;
	/// current context
	IdentU[] ctx;
	/// visibilities
	Visibility[] visStack;
	/// Symbol ASTNode to ASymbol* in STab mapping
	ASymbol*[ASTNode] sMap;
}

/// Level 0 pass function for DefNode -> ASymbol
private void l0Iter(S, D)(D node, ref St st){
	static if (__traits(compiles, node.tParams)){
		if (node.tParams && node.tParams.params.length){
			st.errs ~= errUnsup(node.pos, "templates");
			return;
		}
	}
	S symC;
	symC.ident = st.ctx ~ node.name.IdentU;
	if (__traits(compiles, symC.vis = st.visStack[$ - 1]))
		symC.vis = st.visStack[$ - 1];
	ASymbol* sym = new ASymbol(symC);
	// TODO: untested name collision test
	if (sym.isCallable){
		if (st.stab.hasLocalNonCallable(symC.ident[$ - 1], st.ctx)){
			st.errs ~= errIdentReuse(node.pos, symC.ident[$ - 1].toString);
			return;
		}
	} else if (symC.ident[$ - 1] in st.stab.map){
		st.errs ~= errIdentReuse(node.pos, symC.ident[$ - 1].toString);
		return;
	}
	st.sMap[node] = sym;
	st.stab.add(symC.ident[$ - 1], sym, st.visStack[$ - 1], st.ctx);
}

@ItFn @ITL(0){
	void mixinInitDefIter(MixinInitDef node, ref St st){
		st.errs ~= errUnsup(node);
	}
	void templateIter(TemplateDef node, ref St st){
		st.errs ~= errUnsup(node);
	}
	void cCompIter(CCNode node, ref St st){
		st.errs ~= errUnsup(node);
	}
	void mixinInitIter(MixinInit node, ref St st){
		st.errs ~= errUnsup(node);
	}

	void globDefIter(GlobDef node, ref St st){
		st.visStack ~= node.visibility;
		It.descend(node, st);
		st.visStack.length --;
	}

	void fnIter(FnDef fn, ref St st){
		if (fn.vt !is null){
			st.errs ~= errUnsup(fn.pos, "$vt");
		}
		l0Iter!AFn(fn, st);
	}
	void enumConstIter(EnumConstDef node, ref St st){
		l0Iter!AEnumConst(node, st);
	}
	void enumSmIter(EnumSmDef node, ref St st){
		l0Iter!AEnum(node, st);
	}
	void structIter(StructDef node, ref St st){
		l0Iter!AStruct(node, st);
	}
	void varIter(VarDef node, ref St st){
		l0Iter!AVar(node, st);
	}
	void aliasIter(AliasDef node, ref St st){
		l0Iter!AAlias(node, st);
	}
	void unionIter(UnionDef node, ref St st){
		if (UnkUnion sub = cast(UnkUnion)node){
			st.errs ~= errUnsup(sub);
		}
		l0Iter!AUnion(node, st);
	}
	void utestIter(UTest node, ref St st){
		l0Iter!AUTest(node, st);
	}
}

/// STab Level 0 Result
package struct S0R{
	STab stab; /// symbol table
	ASymbol*[ASTNode] sMap; /// node to sym mapping
}

/// Builds Level 0 Symbol Table
/// Returns: Level 0 Symbol Table with a node-to-symbol map, or SmErr[]
package SmErrsVal!S0R stab0Of(ASTNode node, STab stab = null,
		IdentU[] ctx = null){
	St st;
	if (stab is null)
		stab = new STab;
	st.stab = stab;
	st.ctx = ctx.dup;
	st.visStack ~= Visibility.Default;
	It.exec(node, st);
	if (st.errs)
		return SmErrsVal!S0R(st.errs);
	return SmErrsVal!S0R(S0R(st.stab, st.sMap));
}
