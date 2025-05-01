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

package struct exprRes{
private:
	alias It = ASTIter!(identExprIter);
static:
	struct St{
		SmErr[] errs;
		ASymbol[IdentU] syms;
		ASymbol[IdentU][] symStack;
		IdentU modId;
		RExpr expr;
	}

	@ItFn void identExprIter(IdentExpr node, ref St st){}
	// TODO make this
public:

	SmErrsVal!RExpr opCall(Expression expr, IdentU modId,
			ASymbol[IdentU][] symStack = null){
		St st;
		st.symStack = symStack.dup;
		if (st.symStack.length)
			st.syms = st.symStack[$ - 1];
		st.modId = modId;
		It.exec(expr, st);
		if (st.errs.length)
			return SmErrsVal!RExpr(st.errs);
		return SmErrsVal!RExpr(st.expr);
	}
}

package struct typeRes{
private:
	alias It = ASTIter!(identExprIter);
static:
	struct St{
		SmErr[] errs;
		ASymbol[IdentU] syms;
		ASymbol[IdentU][] symStack;
		IdentU modId;
		ADataType type;
	}

	@ItFn void identExprIter(IdentExpr node, ref St st){}
	// TODO make this
public:

	SmErrsVal!ADataType opCall(Expression expr, IdentU modId,
			ASymbol[IdentU][] symStack = null){
		St st;
		st.symStack = symStack.dup;
		if (st.symStack.length)
			st.syms = st.symStack[$ - 1];
		st.modId = modId;
		It.exec(expr, st);
		if (st.errs.length)
			return SmErrsVal!ADataType(st.errs);
		return SmErrsVal!ADataType(st.type);
	}
}

package struct symOf{
private:
	alias It = ASTIter!(globDefIter, mixinInitDefIter, templateIter, cCompIter,
			mixinInitIter, globDefIter, enumConstIter, enumSmIter, structIter,
			varIter, aliasIter, unionIter, utestIter);
static:
	struct St{
		SmErr[] errs;
		ASymbol[IdentU] syms;
		ASymbol[IdentU][] symStack;
		IdentU[] ctx;
		Visibility[] visStack;
		Imports imports;
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
		It.exec(node.def, st);
	}
	@ItFn void enumConstIter(EnumConstDef node, ref St st){
		AEnumConst ret;
		ret.ident = st.ctx ~ node.name.IdentU;
		ret.vis = st.visStack[$ - 1];
	}
	@ItFn void enumSmIter(EnumSmDef node, ref St st){}
	@ItFn void structIter(StructDef node, ref St st){}
	@ItFn void varIter(VarDef node, ref St st){}
	@ItFn void aliasIter(AliasDef node, ref St st){}
	@ItFn void unionIter(UnionDef node, ref St st){}
	@ItFn void utestIter(UTest node, ref St st){}

public static:
	SmErrsVal!(ASymbol[IdentU]) opCall(Module mod,
			ASymbol[IdentU][] symStack = null){
		St st;
		SmErrsVal!Imports imports = mod.importsOf;
		if (imports.isErr)
			return SmErrsVal!(ASymbol[IdentU])(imports.err);
		st.imports = imports.val;
		st.symStack = symStack.dup;
		It.exec(mod, st);
		if (st.errs)
			return SmErrsVal!(ASymbol[IdentU])(st.errs);
		return SmErrsVal!(ASymbol[IdentU])(st.syms);
	}
}

/// Builds AModule from Module
/// Returns: AModule
package SmErrsVal!AModule aModOf(Module node){
	SmErrsVal!(ASymbol[IdentU]) val = symOf(node);
	if (val.isErr)
		return SmErrsVal!AModule(val.err);
	return SmErrsVal!AModule(AModule(ADT(), val.val));
}
