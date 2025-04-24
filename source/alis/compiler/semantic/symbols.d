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

static:

public static:
	SmErrsVal!AModule opCall(Module mod){
		AModule ret;
		return SmErrsVal!AModule(ret);
	}
}

/// iteration levels
private enum Lv : size_t{
	Mod, /// module level ST building
	Defs, /// converting module level definitions to ASymbols
}

//private alias It(Lv l) = ItL!(mixin(__MODULE__), l);

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

/*private @ITL(Lv.Mod) @ItFn {
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
}*/

/// Iteration state for Lv.Defs
private struct St1{
	STab!DefNode st0;
	STab!ASymbol st;
	RFn[] fns;
	void[0][DefNode] visited;
	IdentU visId;
	IdentU[] ctx;
}

private @ITL(Lv.Defs) @ItFn {
	void mixinInitDefConv(MixinInitDef node, ref St1){
		node.unsupported;
	}
	void templateConv(TemplateDef node, ref St1){
		node.unsupported;
	}
	void cCompConv(CCNode node, ref St1){
		node.unsupported;
	}
	void mixinInitConv(MixinInit node, ref St1){
		node.unsupported;
	}

	void importConv(Import node, ref St1 state){
		// TODO: merge imported stab
	}

	void fnConv(FnDef node, ref St1 state){
		RFn fn = new RFn;
		fn.ident = state.ctx.map!(i => i.toString).join(".")
			.format!"%s.%s"(node.name);
		fn.paramsN = node.params.params.map!(p => p.name).array;
		fn.paramCount = fn.paramsN.length;
		fn.pos = node.pos;
		// TODO: figure out fn.body & fn.paramsT
	}
	void enumConstConv(EnumConstDef node, ref St1 state){}
	void enumSmConv(EnumSmDef node, ref St1 state){}
	void structConv(StructDef node, ref St1 state){}
	void varConv(VarDef node, ref St1 state){}
	void aliasConv(AliasDef node, ref St1 state){}
	void unionConv(UnionDef node, ref St1 state){}
	void utestConv(UTest node, ref St1 state){}
}

private void unsupported(ASTNode node){
	import std.conv : to;
	assert(false, typeid(node).to!string ~ " not yet supported");
}
