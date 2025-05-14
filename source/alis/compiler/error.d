/++
Alis Compiler Errors
+/
module alis.compiler.error;

import alis.compiler.common,
			 alis.compiler.lexer;

import std.format,
			 std.array,
			 std.string;
/// A possibly erroneous value (CompileErr or some valid value)
alias CmpErrVal(T) = ErrVal!(T, CmpErr);

/// CmpErrVal but with possibility of multiple errors
alias CmpMErrVal(T) = ErrVal!(T, CmpErr[]);

/// Alis Compiler Error
struct CmpErr{
	/// Possible Error types
	enum Type{
		Syntax, /// Unexpected sequence error, lexer error
		UnxpTok, /// Unexpected token, parser error
		EOF, /// unexpected EOF
		VisibilityInvalid, /// invalid visibility applied
		Literal, /// Invalid value in a literal
		UnxpAttrs, /// Unexpected Attribute List
		UnionMixedName, /// Union being named and unnamed
	}
	/// where error happen
	Location pos;
	/// error message
	string msg;
	/// Type
	Type type;
	/// Returns: string representation of this error
	string toString() const pure {
		return format!"%s: %s"(pos, msg);
	}

	@disable this();
	/// constructor
	this (size_t line, size_t col, string msg, Type type){
		this.pos.line = line;
		this.pos.col = col;
		this.msg = msg;
		this.type = type;
	}
	/// ditto
	this (Location pos, string msg, Type type){
		this.pos = pos;
		this.msg = msg;
		this.type = type;
	}
}

/// EOF Error.
public enum errEOF = CmpErr(size_t.max, size_t.max, "Unexpected EndOfFile",
		CmpErr.Type.EOF);

/// Returns: Invalid Visibility error
public CmpErr errVisibility(Tok tok, string msg){
	return CmpErr(tok.line, tok.col, msg, CmpErr.Type.VisibilityInvalid);
}

/// Returns: unexpected token error
public CmpErr errUnxpTok(Tok tok, inout string[] expected){
	return CmpErr(tok.line, tok.col,
			"Unexpected Token. Expected: %s; but found: `%s`".format(
				expected.join(" or, "), tok.token), CmpErr.Type.UnxpTok);
}

/// Returns: Literal error
public CmpErr errLiteral(Tok tok, string msg){
	return CmpErr(tok.line, tok.col,
			"Literal Value Error, `%s`. %s".format(tok.token, msg),
			CmpErr.Type.UnxpTok);
}

/// Returns: unexpected attribute list error
public CmpErr errUnxpAttrs(Tok tok){
	return CmpErr(tok.line, tok.col,
			"Unexpected Attribute List", CmpErr.Type.UnxpAttrs);
}

public CmpErr errrUnionMixed(Tok tok){
	return CmpErr(tok.line, tok.col,
			"Union cannot have mixed named and unnamed members",
			CmpErr.Type.UnionMixedName);
}
