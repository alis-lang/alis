/++
Symbols Resolution
+/
module alis.compiler.semantic.symbols;

import std.algorithm,
			 std.range,
			 std.traits;

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
	/// symbol table intermediate
	DefNode[Ident] sti;
	/// symbol table final
	ASymbol[Ident] st;
	/// module info
	AModule mod;
	/// current context identifier
	Ident* ctx;
}

/// symbol table builder functions
private struct STFns{
private static:
	/// Returns: true if a symbol name exists
	bool identExists(Ident i, ref STState state){
		if (i.ident == "_")
			return false;
		return i in state.sti || i in state.st;
	}
	/// extracts imports into symbol table
	void importExtract(R)(R range, ref STState state) if (
			is (ForeachType!R : Import)){
		foreach (Import imp; range){
			// TODO: how to merge imported ST?
			Ident id = Ident(imp.name, state.ctx);
			AImport aImp = AImport(imp.moduleIdent, id);
			if (identExists(id, state)){
				import std.stdio;
				stderr.writeln("ajeeb");
				// TODO error
				continue;
			}
			state.st[id] = ASymbol(aImp);
		}
	}

	@ItPre
	void modIter(Module node, ref STState state){
		state.ctx = new Ident(node.ident);
		importExtract(node.defs
				.map!(d => cast(Import)(d.def))
				.filter!(d => d !is null),
				state);
	}

	@ItPre
	void defIter(DefNode node, ref STState state){
		Ident* subCtx = new Ident(node.name, state.ctx);
		if (*subCtx in state.sti || *subCtx in state.st){

		}
	}
}
