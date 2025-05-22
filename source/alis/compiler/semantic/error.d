/++
Alis Semantic Errors
+/
module alis.compiler.semantic.error;

import alis.compiler.common,
			 alis.compiler.ast,
			 alis.common;

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
		ValExprExpected, /// Expression should have resolved to value
		TypeExprExpected, /// Expression should have resolved to type
		ParamCountMis, /// mismatched parameter count
		TypeMis, /// type mismatch
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

/// Unsupported Feature
package SmErr errUnsup(Location pos, string feat){
	return SmErr(pos, feat.format!"Unsupported Feature: %s",
			SmErr.Type.UnsupFeat);
}

/// Expression should have resolved to Value
package SmErr errExprValExpected(Expression expr){
	return SmErr(expr.pos,
			format!"Expression does not evaluate to value",
			SmErr.Type.ValExprExpected);
}

/// Expression should have resolved to type
package SmErr errExprTypeExpected(Expression expr){
	return SmErr(expr.pos,
			format!"Expression does not evaluate to type",
			SmErr.Type.TypeExprExpected);
}

/// Parameter count mismatch
package SmErr errParamCount(ASTNode node, string name, size_t expected,
		size_t got){
	return SmErr(node.pos,
			format!"Mismatched parameter count for `%s`: expected %d, received %d"(
				name, expected, got),
			SmErr.Type.ParamCountMis);
}

/// Type mismatch
package SmErr errTypeMis(ASTNode node, ADataType expected, ADataType got){
	return SmErr(node.pos,
			format!"Mismatched types: expected `%s`, received `%s`"(
				expected.toString, got.toString),
			SmErr.Type.TypeMis);
}
