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
public CmpErrVal!Module parse(TokRange toks){
	return parseModule(toks);
}
/// ditto
public CmpErrVal!Module parse(ref TokRange toks){
	return parseModule(toks);
}
