module alis.compiler.ast.defs.enums;

import std.json,
			 std.conv,
			 std.array,
			 std.algorithm;

import alis.compiler.ast.exprs,
			 alis.compiler.ast.defs,
			 alis.compiler.ast.misc,
			 alis.compiler.ast.ccomp;

import alis.compiler.parser.parser : ASTNode;

/// abstract enum
public abstract class EnumDef : DefNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["type"] = type.toJson;
		if (tParams)
			ret["tParams"] = tParams.toJson;
		ret["_name"] = "EnumDef";
		return ret;
	}
public:
	/// type
	Expression type;
	/// template paramters, can be null
	TParamList tParams;
}

/// enum constant definition
public class EnumConstDef : EnumDef{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.toJson;
		ret["_name"] = "EnumConstDef";
		return ret;
	}
public:
	/// value
	Expression val;
}

/// enum definition
public class EnumSmDef : EnumDef{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["members"] = members.map!(a => a.toJson).array;
		ret["mixinInits"] = mixinInits.map!(a => a.toJson).array;
		ret["cComp"] = cComp.map!(a => a.toJson).array;
		ret["_name"] = "EnumSmDef";
		return ret;
	}
public:
	/// members
	EnumMember[] members;
	/// mixin inits
	MixinInit[] mixinInits;
	/// conditional compilation
	CCNode[] cComp;
}

/// enum member
public class EnumMember : DefNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (value)
			ret["value"] = value.toJson;
		ret["_name"] = "EnumMember";
		return ret;
	}
public:
	/// value. can be null
	Expression value;
}
