/++
Symbols Resolution
+/
module alis.compiler.semantic.symbols;

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
			 alis.compiler.ast,
			 alis.compiler.ast.iter,
			 alis.compiler.rst;

import meta;

/// Gets imports at any node in the AST (yes I know it's a struct, it has an
/// opCall override)
///
/// Returns: Imports
private struct importsOf{
private:
	alias It = ASTIter!(globDefIter, localDefIter, importIter);
static:
	struct St{
		SmErr[] errs;
		Imports imp;
	}

	@ItFn void globDefIter(GlobDef n, ref St st){
		It.exec(n.def, st);
	}
	@ItFn void localDefIter(DefStatement n, ref St st){
		It.exec(n.def, st);
	}
	@ItFn void importIter(Import n, ref St st){
		if (n.name == "_"){
			if (n.name in st.imp.importsN){
				st.errs ~= errIdentReuse(n.pos, n.name);
				return;
			}
			st.imp.importsN[n.name] = n.moduleIdent.mod;
			return;
		}
		st.imp.imports ~= n.moduleIdent.mod;
	}

public static:
	SmErrsVal!Imports opCall(ASTNode node){
		St st;
		It.exec(node, st);
		if (st.errs)
			return SmErrsVal!Imports(st.errs);
		return SmErrsVal!Imports(st.imp);
	}
}

package struct symOf{
private:
	alias It = ASTIter!(moduleIter,
			globDefIter, mixinInitDefIter, templateIter, cCompIter,
			mixinInitIter, globDefIter, enumConstIter, enumSmIter, structIter,
			varIter, aliasIter, unionIter, utestIter);
static:
	struct St{
		SmErr[] errs;
		STab stabMain;
		STab stab;
		IdentU[] ctx;
		Visibility[] visStack;
		Imports imports;
	}

	@ItFn void moduleIter(Module mod, ref St st){
		st.stab = new STab;
		if (st.stabMain)
			st.stabMain.add(mod.ident.IdentU, st.stab, Visibility.Pub, st.ctx);
		else
			st.stabMain = st.stab;
		if (st.ctx)
			st.ctx ~= mod.ident.IdentU;
		else
			st.ctx = [mod.ident.IdentU];
		It.descend(mod, st);
	}

	@ItFn void mixinInitDefIter(MixinInitDef node, ref St st){
		st.errs ~= errUnsup(node);
	}
	@ItFn void templateIter(TemplateDef node, ref St st){
		st.errs ~= errUnsup(node);
	}
	@ItFn void cCompIter(CCNode node, ref St st){
		st.errs ~= errUnsup(node);
	}
	@ItFn void mixinInitIter(MixinInit node, ref St st){
		st.errs ~= errUnsup(node);
	}

	@ItFn void globDefIter(GlobDef node, ref St st){
		st.visStack ~= node.visibility;
		It.descend(node, st);
	}
	@ItFn void enumConstIter(EnumConstDef node, ref St st){
		SmErrsVal!AEnumConst val = conv(node, st.stabMain,
				st.visStack[$ - 1], st.ctx);
		if (val.isErr){
			st.errs ~= val.err;
			return;
		}
		AEnumConst ret = val.val;
		st.stab.add(ret.ident[$ - 1], new ASymbol(ret), st.visStack[$ - 1], st.ctx);
	}
	@ItFn void enumSmIter(EnumSmDef node, ref St st){}
	@ItFn void structIter(StructDef node, ref St st){}
	@ItFn void varIter(VarDef node, ref St st){}
	@ItFn void aliasIter(AliasDef node, ref St st){}
	@ItFn void unionIter(UnionDef node, ref St st){}
	@ItFn void utestIter(UTest node, ref St st){}

public static:
	SmErrsVal!STab opCall(Module mod,
			STab stab = null, IdentU[] ctx = null){
		St st;
		SmErrsVal!Imports imports = mod.importsOf;
		if (imports.isErr)
			return SmErrsVal!STab(imports.err);
		st.imports = imports.val;
		st.stabMain = stab;
		st.ctx = ctx;
		It.exec(mod, st);
		if (st.errs)
			return SmErrsVal!STab(st.errs);
		return SmErrsVal!STab(st.stab);
	}
}

/// Builds AModule from Module
/// Returns: AModule
/*package SmErrsVal!AModule aModOf(Module node){
	SmErrsVal!(STab!ASymbol) val = symOf(node, null);
	if (val.isErr)
		return SmErrsVal!(Stack!ASymbol)(val.err);
	return SmErrsVal!(Stack!ASymbol)(AModule(ADT(), val.val));
}*/

/// converts an EnumConstDef to AEnumConst
/// Returns: AEnumConst, or errors
private SmErrsVal!AEnumConst conv(EnumConstDef node,
		STab stabMod,
		Visibility vis = Visibility.Default, IdentU[] ctx = null){
	AEnumConst ret;
	ret.ident = ctx ~ node.name.IdentU;
	ret.vis = vis;
	SmErrsVal!AValCT typeVal = eval(node.type, stabMod, ctx);
	if (typeVal.isErr)
		return SmErrsVal!AEnumConst(typeVal.err);
	if (typeVal.val.type != AValCT.Type.Type)
		return SmErrsVal!AEnumConst([errExprTypeExpected(node.type)]);
	ret.type = typeVal.val.typeT;
	SmErrsVal!AValCT dataVal = eval(node.val, stabMod, ctx);
	if (dataVal.isErr)
		return SmErrsVal!AEnumConst(dataVal.err);
	AValCT data = dataVal.val;
	if (data.type != AValCT.Type.Literal)
		return SmErrsVal!AEnumConst([errExprValExpected(node.val)]);
	ret.data = data.dataL;
	return SmErrsVal!AEnumConst(ret);
}
