module alis.compiler.ast.exprs.operators.base;

import std.json,
			 std.conv,
			 std.array,
			 std.algorithm;

public import alis.compiler.ast.exprs.base;

/// comma separated expressions
public class CommaExpr : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["exprs"] = exprs.map!(a => a.toJson).array;
		ret["_name"] = "CommaExpr";
		return ret;
	}
public:
	/// expressions
	Expression[] exprs;
}

/// postscript operator
public abstract class OpPostExpr : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["op"] = op;
		ret["operand"] = operand.toJson;
		ret["_name"] = "OpPostExpr";
		return ret;
	}
public:
	/// operator
	string op;
	/// operand
	Expression operand;
}

/// overridable postscript operator
public abstract class OpPostExprOverridable : OpPostExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpPostExprOverridable";
		return ret;
	}
}

/// prefix operator
public abstract class OpPreExpr : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["op"] = op;
		ret["operand"] = operand.toJson;
		ret["_name"] = "OpPreExpr";
		return ret;
	}
public:
	/// operator
	string op;
	/// operand
	Expression operand;
}

/// overridable prefix operator
public abstract class OpPreExprOverridable : OpPreExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpPreExprOverridable";
		return ret;
	}
}

/// binary operator
public abstract class OpBinExpr : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["op"] = op;
		ret["lhs"] = lhs.toJson;
		ret["rhs"] = rhs.toJson;
		ret["_name"] = "OpBinExpr";
		return ret;
	}
public:
	/// operator
	string op;
	/// left operand
	Expression lhs;
	/// right operand
	Expression rhs;
}

/// overridable binary operator
public abstract class OpBinExprOverridable : OpBinExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpBinExprOverridable";
		return ret;
	}
}

/// call expression
public class OpCallExpr : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["callee"] = callee.toJson;
		ret["params"] = params.map!(a => a.toJson).array;
		ret["_name"] = "OpCallExpr";
		return ret;
	}
public:
	/// callee
	Expression callee;
	/// parameters
	Expression[] params;
}

/// index expression
public class OpIndexExpr : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["lhs"] = lhs.toJson;
		ret["indexes"] = indexes.map!(a => a.toJson).array;
		ret["_name"] = "OpIndexExpr";
		return ret;
	}
public:
	/// lhs
	Expression lhs;
	/// indexes
	Expression[] indexes;
}
