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

	@ItFn void globDefIter(GlobDef n, ref St state){
		It.exec(n.def, state);
	}
	@ItFn void localDefIter(DefStatement n, ref St state){
		It.exec(n.def, state);
	}
	@ItFn void importIter(Import n, ref St state){
		if (n.name == "_"){
			if (n.name in state.imp.importsN){
				state.errs ~= errIdentReuse(n.pos, n.name);
				return;
			}
			state.imp.importsN[n.name] = n.moduleIdent.mod;
			return;
		}
		state.imp.imports ~= n.moduleIdent.mod;
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

/// Builds AModule from Module
/// Returns: AModule
package struct aModOf{
private:
	alias It = ASTIter!(globDefIter, mixinInitDefIter, templateIter, cCompIter,
			mixinInitIter, globDefIter, enumConstIter, enumSmIter, structIter,
			varIter, aliasIter, unionIter, utestIter);
static:
	struct St{
		SmErr[] errs;
		AModule mod;
	}
	@ItFn mixinInitDefIter(MixinInitDef node, ref St st){
		st.errs ~= errUnsup(node);
	}
	@ItFn templateIter(TemplateDef node, ref St st){
		st.errs ~= errUnsup(node);
	}
	@ItFn cCompIter(CCNode node, ref St st){
		st.errs ~= errUnsup(node);
	}
	@ItFn mixinInitIter(MixinInit node, ref St st){
		st.errs ~= errUnsup(node);
	}

	@ItFn void globDefIter(GlobDef node, ref St st){
		It.exec(node.def, st);
	}
	@ItFn void enumConstIter(EnumConstDef node, ref St state){}
	@ItFn void enumSmIter(EnumSmDef node, ref St state){}
	@ItFn void structIter(StructDef node, ref St state){}
	@ItFn void varIter(VarDef node, ref St state){}
	@ItFn void aliasIter(AliasDef node, ref St state){}
	@ItFn void unionIter(UnionDef node, ref St state){}
	@ItFn void utestIter(UTest node, ref St state){}

public static:
	SmErrsVal!AModule opCall(Module mod){
		St st;
		It.exec(mod, st);
		if (st.errs)
			return SmErrsVal!AModule(st.errs);
		return SmErrsVal!AModule(st.mod);
	}
}
