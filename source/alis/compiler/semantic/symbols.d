/++
Symbols Resolution
+/
module alis.compiler.semantic.symbols;

import std.algorithm,
			 std.typecons,
			 std.range,
			 std.traits,
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
	It!(Lv.Mod).exec(mod, st0);
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
			It!(Lv.Defs).exec(def, state);
		}
	}
	return SmErrsVal!(STab!ASymbol)(ret);
}

/// iteration levels
private enum Lv : size_t{
	Mod, /// module level ST building
	Defs, /// converting module level definitions to ASymbols
}

private alias It(Lv l) = ItL!(mixin(__MODULE__), l);

/// Iteration state for L.Mod
private struct St0{
	/// module id
	IdentU modId;
	/// visibility for next def node
	Visibility vis;
	/// symbol table
	STab!DefNode st;
	/// errors
	SmErr[] errs;
}

private @ITL(Lv.Mod) @ItFn {
	void modIter(Module node, ref St0 state){
		state.st = new STab!DefNode;
		state.modId = node.ident.IdentU;
		It!(Lv.Mod).descend(node, state);
	}

	void varDefListIter(VarDefList varDefList, ref St0 state){
		It!(Lv.Mod).descend(varDefList, state);
	}

	void defIter(DefNode node, ref St0 state){
		IdentU id = node.name.IdentU;
		state.st.valAdd(id, node,
				state.vis != Visibility.Default ? IdentU.init : state.modId);
	}

	void globDefIter(GlobDef node, ref St0 state){
		state.vis = node.visibility;
		It!(Lv.Mod).descend(node, state);
		state.vis = Visibility.Default;
	}

	void mixinInitDefIter(MixinInitDef def, ref St0){
		def.unsupported;
	}

	void templateDefIter(TemplateDef def, ref St0){
		// kinda sad tbh
		def.unsupported;
	}
}

/// Iteration state for Lv.Defs
private struct St1{
	STab!DefNode st0;
	STab!ASymbol st;
	IdentU visId;
}

private @ITL(Lv.Defs) @ItFn {
	void mixinInitDefConv(MixinInitDef node, ref St1){
		node.unsupported;
	}
	void importConv(Import node, ref St1 state){
		STab!ASymbol modSTab = node.moduleIdent.modSTab;
		// TODO: merge imported stab
	}
	void fnConv(FnDef node, ref St1 state){}
	void enumConstConv(EnumConstDef node, ref St1 state){}
	void enumSmConv(EnumSmDef node, ref St1 state){}
	void structConv(StructDef node, ref St1 state){}
	void templateConv(TemplateDef node, ref St1){
		node.unsupported;
	}
	void varConv(VarDef node, ref St1 state){}
	void aliasConv(AliasDef node, ref St1 state){}
	void unionConv(UnionDef node, ref St1 state){}
	void utestConv(UTest node, ref St1 state){}

	void cCompConv(CCNode node, ref St1){
		node.unsupported;
	}
	void mixinInitConv(MixinInit node, ref St1){
		node.unsupported;
	}
}

private void unsupported(ASTNode node){
	assert(false, typeid(node).stringof ~ " not yet supported");
}
