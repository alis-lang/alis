module alis.compiler.ast.defs.structs;

import std.json,
			 std.conv,
			 std.array,
			 std.algorithm;

import alis.compiler.ast.exprs,
			 alis.compiler.ast.misc,
			 alis.compiler.ast.ccomp,
			 alis.compiler.ast.defs;

import alis.compiler.parser.parser : ASTNode;

/// A struct
public class Struct : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["members"] = members.map!(a => a.toJson).array;
		ret["mixinInits"] = mixinInits.map!(a => a.toJson).array;
		ret["cComp"] = cComp.map!(a => a.toJson).array;
		ret["_name"] = "Struct";
		return ret;
	}
public:
	/// members
	AggMember[] members;
	/// conditional compilation
	CCNode[] cComp;
	/// Mixin Inits
	MixinInit[] mixinInits;
}

/// struct definition
public class StructDef : DefNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["def"] = def.toJson;
		if (tParams)
			ret["tParams"] = tParams.toJson;
		ret["_name"] = "StructDef";
		return ret;
	}
public:
	/// underlying struct
	Struct def;
	/// template paramters, can be null
	TParamList tParams;
}
