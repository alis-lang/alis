/++
Symbols Resolution
+/
module alis.compiler.semantic.symbols;

import std.algorithm,
			 std.range,
			 std.traits;

import utils.ds;

debug import std.stdio;

import alis.common,
			 alis.compiler.common,
			 alis.compiler.semantic.common,
			 alis.compiler.semantic.error,
			 alis.compiler.ast,
			 alis.compiler.ast.iter;

/// symbol table builder iterator
package alias STIter = ASTIter!(ItFnsOf!STFns);

/// Iteration State for SymIter
package struct STState{
	/// module id
	//IdentU modId;
	/// symbol table
	STab!DefNode st;
	/// symbol table stack
	STab!DefNode[] stStack;
	/// current context identifier
	IdentU[] ctx;
	/// errors
	SmErr[] errs;
}

/// symbol table builder functions
private struct STFns{
private static:
	@ItPre
	void modIter(Module node, ref STState state){
		state.ctx ~= node.ident.IdentU;
		state.st = new STab!DefNode;
		state.stStack = [state.st];
		//state.modId = node.ident.IdentU;
	}

	@ItPre @ItPost{
		void varDefListIgnore(VarDefList, ref STState){}
	}

	@ItPre
	bool defIter(DefNode node, ref STState state){
		IdentU id = node.name.IdentU;
		STab!DefNode pSt = state.st;
		state.st = new STab!DefNode;
		pSt.stAdd(id, state.st, state.ctx[$ - 1]);
		pSt.valAdd(id, node, state.ctx[$ - 1]);
		state.ctx ~= id;
		state.stStack ~= state.st;
		return true;
	}

	@ItPost
	void defIterPost(DefNode node, ref STState state){
		state.ctx.length --;
		state.stStack.length --;
		state.st = state.stStack[$ - 1];
	}
}
