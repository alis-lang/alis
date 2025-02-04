module alis.compiler.ast.defs.unions;

import std.json,
			 std.conv,
			 std.array,
			 std.algorithm;

import alis.compiler.ast.exprs,
			 alis.compiler.ast.misc,
			 alis.compiler.ast.ccomp,
			 alis.compiler.ast.defs;

import alis.compiler.parser.parser : ASTNode;

/// An union
public abstract class Union : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["cComp"] = cComp.map!(a => a.toJson).array;
		ret["mixinInits"] = mixinInits.map!(a => a.toJson).array;
		ret["_name"] = "Union";
		return ret;
	}
public:
	/// conditional compilation
	CCNode[] cComp;
	/// mixin inits
	MixinInit[] mixinInits;
}

/// Unknown union (dont know if named or unnamed). Has zero aggMembers, outside
// of CCNode and MixinInit
public class UnkUnion : Union{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "UnkUnion";
		return ret;
	}
}

/// Named Union
public class NamedUnion : Union{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["members"] = members.map!(a => a.toJson).array;
		ret["_name"] = "NamedUnion";
		return ret;
	}
public:
	/// members
	AggMember[] members;
}

/// Unnamed Union
public class UnnamedUnion : Union{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (members)
			ret["members"] = members.map!(a => a.toJson).array;
		ret["_name"] = "UnnamedUnion";
		return ret;
	}
public:
	/// member data types
	UnnamedUnionMember[] members;
}

/// union definition
public class UnionDef : DefNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["def"] = def.toJson;
		if (tParams)
			ret["tParams"] = tParams.toJson;
		ret["_name"] = "UnionDef";
		return ret;
	}
public:
	/// template paramters, can be null
	TParamList tParams;
	/// underlying union
	Union def;
}
