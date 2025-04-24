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

/// Builds symbol table
/// Returns: symbol table
package SmErrsVal!(STab!ASymbol) sTabBuild(Module mod){
	SmErrsVal!(STab!DefNode) std = mod.sTab0Build;
	if (std.isErr)
		return SmErrsVal!(STab!ASymbol)(std.err);
	return std.val.sTab1Build;
}

/// Builds initial symbol table
/// Returns: symbol initial symbol table
private SmErrsVal!(STab!DefNode) sTab0Build(Module mod){
	St0 st0;
	//It!(Lv.Mod).exec(mod, st0); // TODO: FIX THIS!
	if (st0.errs.length)
		return SmErrsVal!(STab!DefNode)(st0.errs);
	return SmErrsVal!(STab!DefNode)(st0.st);
}

/// Builds final symbol table from intial symbol table
private SmErrsVal!(STab!ASymbol) sTab1Build(STab!DefNode stab){
	STab!ASymbol ret = new STab!ASymbol;
	foreach (IdentU id, STab!DefNode.EndNode[] nodes; stab.map){
		foreach (DefNode def, IdentU visId; nodes.map!(n => tuple(n.val, n.vis))){
			St1 state;
			state.st = ret;
			state.st0 = stab;
			state.visId = visId;
			//It!(Lv.Defs).exec(def, state);
		}
	}
	return SmErrsVal!(STab!ASymbol)(ret);
}

/// Gets imports at any node in the AST (yes I know it's a struct, it has an
/// opCall override)
///
/// Returns: Imports
struct importsOf{
	@disable this();
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
struct aModOf{
	@disable this();
private:
	alias It = ASTIter!(globDefIter);
static:
	static struct St{
		SmErr[] errs;
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
		It.exec(node, st);
	}
	@ItFn void enumConstIter(EnumConstDef node, ref St1 state){}
	@ItFn void enumSmIter(EnumSmDef node, ref St1 state){}
	@ItFn void structIter(StructDef node, ref St1 state){}
	@ItFn void varIter(VarDef node, ref St1 state){}
	@ItFn void aliasIter(AliasDef node, ref St1 state){}
	@ItFn void unionIter(UnionDef node, ref St1 state){}
	@ItFn void utestIter(UTest node, ref St1 state){}

public static:
	SmErrsVal!AModule opCall(Module mod){
		AModule ret;
		return SmErrsVal!AModule(ret);
	}
}
