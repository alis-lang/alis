module alis.compiler.ast.defs.vars;

import std.json,
			 std.conv,
			 std.array,
			 std.algorithm;

import alis.compiler.ast.exprs,
			 alis.compiler.ast.misc,
			 alis.compiler.ast.ccomp,
			 alis.compiler.ast.defs;

import alis.compiler.parser.parser : ASTNode;

/// variable definition
public class VarDef : DefNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["type"] = type.toJson;
		if (value)
			ret["value"] = value.toJson;
		ret["_name"] = "VarDef";
		return ret;
	}
public:
	/// type
	Expression type;
	/// optional default value, can be null
	Expression value;
}

/// static variable definition
public class VarStaticDef : VarDef{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "VarStaticDef";
		return ret;
	}
}

/// Variable Definition List
public class VarDefList : DefNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "VarDefList";
		ret["defs"] = defs.map!(a => a.toJson).array;
		return ret;
	}
public:
	/// defs
	VarDef[] defs;
	this(){
		name = "_";
	}
}
