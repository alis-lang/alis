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
		SymExprExpected, /// Expression should have resolved to symbol
		ParamCountMis, /// mismatched parameter count
		TypeMis, /// type mismatch
		RecursiveDep, /// Recursive Dependency
		IncompatTypes, /// incompatible types
		EnumMemValMis, /// enum member value missing
		MultiInherit, /// multiple alias `this`
		FieldThis, /// field named `this`
		AutoNoVal, /// Cannot have auto when no value provided
		UnionMultiDef, /// Union has more than one default values
		UnionNoDef, /// Union has no default value
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
package SmErr errExprValExpected(ASTNode expr){
	return SmErr(expr.pos,
			format!"Expression does not evaluate to value",
			SmErr.Type.ValExprExpected);
}

/// Expression should have resolved to type
package SmErr errExprTypeExpected(ASTNode expr){
	return SmErr(expr.pos,
			format!"Expression does not evaluate to type",
			SmErr.Type.TypeExprExpected);
}
/// Expression should have resolved to Value
package SmErr errExprSymExpected(ASTNode expr){
	return SmErr(expr.pos,
			format!"Expression does not evaluate to symbol",
			SmErr.Type.SymExprExpected);
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

/// Recursive Dependency
package SmErr errRecDep(Location pos, string name){
	return SmErr(pos,
			name.format!"Recursive Dependency on %s",
			SmErr.Type.RecursiveDep);
}

/// incompatible types
package SmErr errIncompatType()(Location pos, ADataType expected,
		ADataType got){
	return SmErr(pos,
			format!"Incompatible Types: Cannot implicitly cast `%s` to `%s`"(
				got, expected),
			SmErr.Type.IncompatTypes);
}

/// incompatible types in enum values
package SmErr errIncompatType(EnumDef node){
	return SmErr(node.pos,
			format!"Incompatible Types: Enum %s members of incompatible types"(
				node.name), SmErr.Type.IncompatTypes);
}

/// enum member value missing
package SmErr errEnumMemValMis(EnumMember member){
	return SmErr(member.pos, member.name.format!"Enum Member %s has no value",
			SmErr.Type.EnumMemValMis);
}

/// multiple inheritence
package SmErr errMultiInherit(Location pos){
	return SmErr(pos, "Multiple `alias this`", SmErr.Type.MultiInherit);
}

/// field named `this`
package SmErr errFieldThis(Location pos){
	return SmErr(pos, "Field cannot be named `this`", SmErr.Type.FieldThis);
}

/// Cannot have auto when no value provided
package SmErr errAutoNoVal(Location pos){
	return SmErr(pos, "Cannot infer auto type in absense of value",
			SmErr.Type.AutoNoVal);
}

/// Union has more than one default values
package SmErr errUnionMultiDef(Location pos){
	return SmErr(pos, "Union cannot have more than one default value",
			SmErr.Type.UnionMultiDef);
}
/// Union has no default value
package SmErr errUnionNoDef(Location pos){
	return SmErr(pos, "Union has no default value",
			SmErr.Type.UnionNoDef);
}
