/++
Parser Package
+/
module alis.compiler.parser;

public import alis.compiler.ast;

import alis.compiler.parser.parser,
			 alis.compiler.error,
			 alis.compiler.lexer,
			 alis.compiler.parser.grammar;

/// Parses Alis Tokens Range into a Module AST Node
/// Returns: CmpErrVal (CmpErr, or Module)
public CmpErrVal!Module parse()(auto ref TokRange toks, string ident = null){
	CmpErrVal!Module ret = parseModule(toks);
	if (!ret.isErr)
		ret.val.ident = ident;
	return ret;
}
