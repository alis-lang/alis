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

/// symbol table builder functions
private struct STFns{
static:
	@ItPre
	void modIter(Module node, ref STState state){
		state.ctx ~= node.ident.IdentU;
		state.st = new STab!DefNode;
		state.stStack = [state.st];
		state.modId = node.ident.IdentU;
	}

	@ItPre @ItPost{
		void varDefListIgnore(VarDefList, ref STState){}
	}

	@ItPre
	bool defIter(DefNode node, ref STState state){
		IdentU id = node.name.IdentU;
		STab!DefNode pSt = state.st;
		state.st = new STab!DefNode;
		IdentU visIdent = state.visStack[$ - 1] == Visibility.Pub
				? state.modId : state.ctx[$ - 1];
		pSt.stAdd(id, state.st, visIdent);
		pSt.valAdd(id, node, visIdent);
		state.ctx ~= id;
		state.stStack ~= state.st;
		return true;
	}

	@ItPre globDefPre(GlobDef def, ref STState state){
		state.visStack ~= def.visibility;
	}

	@ItPost globDefPost(GlobDef, ref STState state){
		state.visStack.length --;
	}

	@ItPre defFn(FnDef fn, ref STState state){
		STab!DefNode pSt = state.st;
		IdentU visIdent = state.visStack[$ - 1] == Visibility.Pub
				? state.modId : state.ctx[$ - 1];
		state.st = new STab!DefNode;
		pSt.stAdd(fn.name.IdentU, state.st, visIdent);
	}

	@ItPost
	void defIterPost(DefNode, ref STState state){
		state.ctx.length --;
		state.stStack.length --;
		state.st = state.stStack[$ - 1];
	}
}
