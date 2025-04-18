/++
Symbols Resolution
+/
module alis.compiler.semantic.symbols;

import std.algorithm,
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
package STab!DefNode sTabBuild(Module mod){
	STab!DefNode ret = new STab!DefNode;

	return ret;
}

/// Iteration State for SymIter
private struct STState{
	/// module id
	IdentU modId;
	/// visibility for next def node
	Visibility[] visStack;
	/// symbol table
	STab!DefNode st;
	/// symbol table stack
	STab!DefNode[] stStack;
	/// current context identifier
	IdentU[] ctx;
	/// errors
	SmErr[] errs;
}

/// symbol table builder iterator
private alias It0 = ItL!(mixin(__MODULE__), 0);

@ITL(0) @ItFn
void modIter(Module node, ref STState state){
	state.ctx ~= node.ident.IdentU;
	state.st = new STab!DefNode;
	state.stStack = [state.st];
	state.modId = node.ident.IdentU;
	It0.descend(node, state);
}

@ITL(0) @ItFn
void varDefListIter(VarDefList varDefList, ref STState state){
	It0.descend(varDefList, state);
}

@ITL(0) @ItFn
void defIter(DefNode node, ref STState state){
	IdentU id = node.name.IdentU;
	state.st.valAdd(id, node,
			state.visStack[$ - 1] != Visibility.Default
			? IdentU.init
			: state.modId);
	It0.descend(node, state);
}

@ITL(0) @ItFn
void globDefIter(GlobDef node, ref STState state){
	state.visStack ~= node.visibility;
	It0.descend(node, state);
	state.visStack.length --;
}
