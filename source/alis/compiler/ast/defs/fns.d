module alis.compiler.ast.defs.fns;

import std.json,
			 std.conv,
			 std.array,
			 std.algorithm;

import alis.compiler.ast.exprs,
			 alis.compiler.ast.defs;

import alis.compiler.parser.parser : ASTNode;

/// function paramter node
public class FParam : DefNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (type)
			ret["type"] = type.toJson;
		if (val)
			ret["val"] = val.toJson;
		ret["_name"] = "FParam";
		return ret;
	}
public:
	/// type, can be null
	Expression type;
	/// default value, can be null
	Expression val;
}

/// function parameter list
public class FParamList : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["params"] = params.map!(a => a.toJson).array;
		ret["_name"] = "FParamList";
		return ret;
	}
public:
	/// parameterse
	FParam[] params;
}

/// function definition
public class FnDef : DefNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (vt)
			ret["vt"] = vt.toJson;
		ret["params"] = params.toJson;
		if (tParams)
			ret["tParams"] = tParams.toJson;
		ret["body"] = body.toJson;
		ret["_name"] = "FnDef";
		return ret;
	}
public:
	/// vt struct, can be null
	Expression vt;
	/// parameters
	FParamList params;
	/// template paramters, can be null
	TParamList tParams;
	/// body expression
	Expression body;
}
