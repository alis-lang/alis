/++
Alis Semantic Errors
+/
module alis.compiler.semantic.error;

import alis.compiler.common,
			 alis.compiler.ast,
			 alis.common;

import std.format,
			 std.conv,
			 std.traits,
			 std.range;

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
		RecursiveDep, /// Recursive Dependency
		IncompatTypes, /// incompatible types
		EnumMemValMis, /// enum member value missing
		MultiInherit, /// multiple alias `this`
		FieldThis, /// field named `this`
		AutoNoVal, /// Cannot have auto when no value provided
		UnionMultiDef, /// Union has more than one default values
		UnionNoDef, /// Union has no default value
		UnUnionTypeUnique, /// Unnamed union must have unique types
		FParamNoDef, /// Function Parameter expected to have default value
		Unxp, /// Unexpected error in compiler
		IdentAmbig, /// Ambiguous identifier
		FnAnonParamDef, /// Anonymous Function cannot have default parameter value
		TypeInferFail, /// Failed to infer type
		Bounds, /// Bounds violation
		CallableIncompat, /// Callable symbol called with incompatible params
		NotCallable, /// Callable expected, not found (sadly)
		CallableConflict, /// Multiple callables matched
		Undef, /// use of undefined identifier
		ConstAssign, /// assigning to const
		RefAssign, /// assigning to ref using =
		AssignNotRefable, /// Assignment LHS is not Ref-able
		AssignRefNotRef, /// `@=` used with non-ref LHS
		DerefNoRef, /// trying to deref something that is not a ref
		ConstConst, /// trying to const a const
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

/// Recursive Dependency
package SmErr errRecDep(Location pos, string name){
	return SmErr(pos,
			name.format!"Recursive Dependency on %s",
			SmErr.Type.RecursiveDep);
}

/// incompatible types
package SmErr errIncompatType()(Location pos, string expected,
		string got){
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

/// Unnamed union must have unique types
package SmErr errUnUnionTypeUnique(Location pos, ADataType a, ADataType b){
	return SmErr(pos,
			format!"Unnamed Union's types must be unique: %s is compatible with %s"(
				a, b), SmErr.Type.UnUnionTypeUnique);
}

/// Function Parameter expected to have default value
package SmErr errFParamNoDef(Location pos, string name){
	return SmErr(pos,
			name.format!"Function parameter %s should have default value",
			SmErr.Type.FParamNoDef);
}

/// Unexpected error in compiler
package SmErr errUnxp(Location pos, string err){
	return SmErr(pos, err, SmErr.Type.Unxp);
}

/// Ambiguous identifier
package SmErr errIdentAmbig(R)(Location pos, string ident, R matches) if (
		isInputRange!(R, string)){
	return SmErr(pos,
			format!"Identifier %s is ambiguous, matches with: %(%r,%)"(
				ident, matches), SmErr.Type.IdentAmbig);
}

/// Anonymous Function cannot have default parameter value
package SmErr errFnAnonParamDef(Location pos, string name){
	return SmErr(pos,
			format!"anonymous function parameter `%s` cannot have default value"(
				name), SmErr.Type.FnAnonParamDef);
}

/// Failed to infer type
package SmErr errTypeInferFail(Location pos, string name){
	return SmErr(pos, name.format!"type inference failed for `%s`",
			SmErr.Type.TypeExprExpected);
}

/// Bounds violation
package SmErr errBounds(Location pos, size_t max, size_t got){
	return SmErr(pos, format!"Bounds violation: %d exceeds bound %d"(
				got, max), SmErr.Type.Bounds);
}
/// Callable symbol called with incompatible params
package SmErr errCallableIncompat(R)(Location pos, string symN, R range) if (
		isInputRange!(R, string)){
	return SmErr(pos,
			format!"incompatible parameters: `%s` cannot be called with (%(%r,%))"(
				symN, range), SmErr.Type.CallableIncompat);
}

/// Callable expected, not found (sadly)
package SmErr errNotCallable(Location pos, string symN){
	return SmErr(pos, symN.format!"callable expected: `%s` is not callable",
			SmErr.Type.NotCallable);
}

/// Multiple callables matched
package SmErr errCallableConflict(R)(Location pos, string symN, R range) if (
		isInputRange!(R, string)){
	return SmErr(pos,
			format!"multiple matches: for callable `%s` with parameters %(%r%)"(
				symN, range), SmErr.Type.CallableConflict);
}

/// use of undefined identifier
package SmErr errUndef(Location pos, string id){
	return SmErr(pos, id.format!"undefined identifier: `%s`", SmErr.Type.Undef);
}

/// assigning to const
package SmErr errConstAssign(Location pos, string type){
	return SmErr(pos,
			type.format!"assignment to const: cannot assign to type `%s`",
			SmErr.Type.ConstAssign);
}

/// assigning to ref using =
package SmErr errRefAssign(Location pos){
	return SmErr(pos, "assignment to ref: cannot use `=` to assign to ref",
			SmErr.Type.RefAssign);
}

/// Assignment LHS is not Ref-able
package SmErr errAssignNotRefable(Location pos){
	return SmErr(pos, "assignment to non referenceable value",
		SmErr.Type.AssignNotRefable);
}

/// Expression is not  ref-able
package SmErr errRefableNot(Location pos){
	return SmErr(pos, "expression is not referenceable",
			SmErr.Type.AssignRefNotRef);
}

/// trying to deref something that is not a ref
package SmErr errDerefNoRef(Location pos, string type){
	return SmErr(pos,
			type.format!"only reference can be dereferenced: cannot deref `%s`",
			SmErr.Type.DerefNoRef);
}

/// trying to const a const
package SmErr errConstConst(Location pos, string type){
	return SmErr(pos, type.format!"constOf const: cannot const `%s`",
			SmErr.Type.ConstConst);
}
