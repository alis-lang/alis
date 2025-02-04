module alis.compiler.ast.exprs.anondef;

import std.json,
			 std.conv,
			 std.array,
			 std.algorithm;

import alis.compiler.ast.exprs.base,
			 alis.compiler.ast.defs;

import alis.compiler.parser.parser : ASTNode;

/// struct type expression
public class StructAnon : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.toJson;
		ret["_name"] = "StructAnon";
		return ret;
	}
public:
	/// underlying struct
	Struct val;
}

/// anonymous union
public class UnionAnon : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.toJson;
		ret["_name"] = "UnionAnon";
		return ret;
	}
public:
	/// underlying union
	Union val;
}

/// anonymous function expression
public class FnAnonExpr : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["params"] = params.toJson;
		ret["body"] = body.toJson;
		ret["_name"] = "FnAnonExpr";
		return ret;
	}
public:
	/// parameters
	FParamList params;
	/// body
	Expression body;
}
