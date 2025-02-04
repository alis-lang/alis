module alis.compiler.ast.misc;

import std.json,
			 std.conv,
			 std.array,
			 std.algorithm;

import alis.compiler.ast.exprs,
			 alis.compiler.ast.defs;

import alis.compiler.parser.parser : ASTNode;

/// Visibility specifier
/// first rightmost bit -> 1 if can read
/// second rightmost bit -> 1 if can write
enum Visibility : ubyte{
	Default = 0,
	IPub = 1,
	Pub = 2,
}

/// Key Value pair node (keyIdent = valExpr)
public class KeyVal : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["key"] = key;
		if (val)
			ret["val"] = val.toJson;
		ret["_name"] = "KeyVal";
		return ret;
	}
public:
	/// key ident
	string key;
	/// value
	Expression val;
}

/// Mixin Init
public class MixinInit : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["mixn"] = mixn.toJson;
		ret["params"] = params.map!(a => a.toJson).array;
		ret["_name"] = "MixinInit";
		return ret;
	}
public:
	/// Mixin
	Expression mixn;
	/// Parameters
	Expression[] params;
}

/// Mixin init to definition
public class MixinInitDef : DefNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["mixinInit"] = mixinInit.toJson;
		ret["_name"] = "MixinInitDef";
		return ret;
	}
public:
	/// underlying mixinInit
	MixinInit mixinInit;
	this(){
		name = "_";
	}
}

/// Attribute List
public class AttrList : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (attrs)
			ret["attrs"] = attrs.map!(a => a.toJson).array;
		ret["_name"] = "AttrList";
		return ret;
	}
public:
	/// attributes
	Expression[] attrs;
}

/// Any Block (body is just tokens)
public class AnyBlock : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["body"] = body.map!(a => a.toString).array;
		ret["_name"] = "AnyBlock";
		return ret;
	}
public:
	import alis.compiler.lexer : Tok;
	Tok[] body;
}

/// Aggregate Member List
public class AggMemberList : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (members)
			ret["members"] = members.map!(a => a.toJson).array;
		if (uMembers)
			ret["uMembers"] = uMembers.map!(a => a.toJson).array;
		ret["_name"] = "AggMemberList";
		return ret;
	}
public:
	/// aggregate members
	AggMember[] members;
	/// unnamed members
	UnnamedUnionMember[] uMembers;
}

/// aggregate member (union/struct member)
public abstract class AggMember : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["visibility"] = visibility.to!string;
		if (attrs)
			ret["attrs"] = attrs.toJson;
		ret["_name"] = "AggMember";

		return ret;
	}
public:
	/// visibility
	Visibility visibility;
	/// attributes
	AttrList attrs;
}

/// AggMember in case of alias
public class AggMemberAlias : AggMember{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["name"] = name;
		ret["val"] = val.toJson;
		ret["_name"] = "AggMemberAlias";
		return ret;
	}
public:
	/// name
	string name;
	/// val
	Expression val;
}

/// AggMember named
public class AggMemberNamed : AggMember{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["type"] = type.toJson;
		ret["name"] = name;
		if (val)
			ret["val"] = val.toJson;
		ret["_name"] = "AggMemberNamed";
		return ret;
	}
public:
	/// type
	Expression type;
	/// name
	string name;
	/// default value, can be null
	Expression val;
}

/// AggMember unnamed
public class UnnamedUnionMember : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["type"] = type.toJson;
		if (val)
			ret["val"] = val.toJson;
		if (attrs)
			ret["attrs"] = attrs.toJson;
		ret["_name"] = "UnnamedUnionMember";
		return ret;
	}
public:
	/// attributes
	AttrList attrs;
	/// type
	Expression type;
	/// default value, can be null
	Expression val;
}
