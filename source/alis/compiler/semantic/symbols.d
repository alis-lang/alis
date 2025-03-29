/++
Symbols Resolution
+/
module alis.compiler.semantic.symbols;

import std.algorithm,
			 std.range,
			 std.traits;

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
	/// symbol table intermediate
	DefNode[Ident] sti;
	/// symbol table final
	ASymbol[Ident] st;
	/// current context identifier
	Ident* ctx;
	/// errors
	SmErr[] errs;
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
	void importsExtract(R)(R range, ref STState state) if (
			is (ForeachType!R : Import)){
		foreach (Import imp; range){
			// TODO: how to merge imported ST?
			Ident id = Ident(imp.name, state.ctx);
			AImport aImp = AImport(imp.moduleIdent, id);
			if (identExists(id, state)){
				state.errs ~= errIdentReuse(imp.pos, imp.name);
				continue;
			}
			state.st[id] = ASymbol(aImp);
			// TODO merge imported module's symbol table
		}
	}

	@ItPre
	void modIter(Module node, ref STState state){
		state.ctx = new Ident(node.ident);
		importsExtract(node.defs
				.map!(d => cast(Import)(d.def))
				.filter!(d => d !is null),
				state);
	}

	@ItPre @ItPost{
		void modImportIgnore(Import, ref STState){}
		void varDefListIgnore(VarDefList, ref STState){}
	}

	@ItPre
	void defIter(DefNode node, ref STState state){
		Ident id = Ident(node.name, state.ctx);
		if (identExists(id, state)){
			state.errs ~= errIdentReuse(node.pos, node.name);
			return;
		}
		state.sti[id] = node;
		state.ctx = new Ident;
		*(state.ctx) = id;
	}

	@ItPost
	void defIterPost(DefNode node, ref STState state){
		if (state.ctx.ident == node.name)
			state.ctx = state.ctx.prev;
	}
}
