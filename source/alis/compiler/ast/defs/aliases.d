module alis.compiler.ast.defs.aliases;

import std.json,
			 std.conv,
			 std.array,
			 std.algorithm;

import alis.compiler.ast.exprs,
			 alis.compiler.ast.defs;

import alis.compiler.parser.parser : ASTNode;

/// alias definition
public class AliasDef : DefNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (tParams)
			ret["tParams"] = tParams.toJson;
		ret["val"] = val.toJson;
		ret["_name"] = "AliasDef";
		return ret;
	}
public:
	/// template paramters, can be null
	TParamList tParams;
	/// value
	Expression val;
}
