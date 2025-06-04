/++
AST Package
+/
module alis.compiler.ast;

import std.meta;

import alis.compiler.ast.iter;

public import alis.compiler.ast.ast;

public alias ASTIter(Fns...) =
	Instantiate!(alis.compiler.ast.iter.ASTIter!ASTNodes, Fns);

public import alis.compiler.ast.iter : ItFn, ItFnsOf;
