/++
Alis Semantic Errors
+/
module alis.compiler.semantic.error;

import alis.compiler.common;

import std.format,
			 std.conv;

/// Value or SmErr
alias SmErrVal(T) = ErrVal!(T, SmErr);
/// Value or SmErrs
alias SmErrsVal(T) = ErrVal!(T, SmErr[]);

/// Alis Semantic Error
public struct SmErr{
	/// Possible Error Types
	enum Type{
		IdentReuse, /// Same identifier used across definitions
		UnsupFeat, /// Unsupported Feature used
	}
	/// where error happen
	Location pos;
	/// error message
	string msg;
	/// type
	Type type;
	/// string representation
	string toString() const pure {
		return format!"%s: %s"(pos, msg);
	}
	@disable this();
	this (Location pos, string msg, Type type){
		this.pos = pos;
		this.msg = msg;
		this.type = type;
	}
}

/// Identifier re-use error
package SmErr errIdentReuse(Location pos, string ident){
	return SmErr(pos, format!"Identifier `%s` conflicts"(ident),
			SmErr.Type.IdentReuse);
}

/// Unsupported AST node
package SmErr errUnsup(ASTNode node){
	return SmErr(node.pos,
			typeid(node).to!string.format!"Unsupported node `%s`",
			SmErr.Type.UnsupFeat);
}
