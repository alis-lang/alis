module alis.compiler.ast.defs.templates;

import std.json,
			 std.conv,
			 std.array,
			 std.algorithm;

import alis.compiler.ast.exprs,
			 alis.compiler.ast.misc,
			 alis.compiler.ast.ccomp,
			 alis.compiler.ast.defs;

import alis.compiler.parser.parser : ASTNode;

/// abstract template parameter node
public abstract class TParam : DefNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["specialization"] = specialization.map!(a => a.toJson).array;
		ret["_name"] = "TParam";
		return ret;
	}
public:
	/// specialization
	Expression[] specialization;
}

/// template alias paramter node
public class TParamAlias : TParam{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "TParamAlias";
		return ret;
	}
}

/// template alias sequence parameter node
public class TParamAliasSeq : TParamAlias{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "TParamAliasSeq";
		return ret;
	}
}

/// template $type paramter node
public class TParamType : TParam{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "TParamType";
		return ret;
	}
}

/// template $type sequence parameter node
public class TParamTypeSeq : TParamType{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "TParamTypeSeq";
		return ret;
	}
}

/// template parameter, typical
public class TParamSm : TParam{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["type"] = type.toJson;
		ret["_name"] = "TParamSm";
		return ret;
	}
public:
	/// type
	Expression type;
}

/// template param list
public class TParamList : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["params"] = params.map!(a => a.toJson).array;
		if (condition)
			ret["condition"] = condition.toJson;
		ret["_name"] = "TParamList";
		return ret;
	}
public:
	/// parameters
	TParam[] params;
	/// condition, can be null
	Expression condition;
}

/// template definition
public class TemplateDef : DefNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (tParams)
			ret["tParams"] = tParams.toJson;
		ret["body"] = body.toJson;
		ret["_name"] = "TemplateDef";
		return ret;
	}
public:
	/// template paramters, can be null
	TParamList tParams;
	/// body
	AnyBlock body;
}

/// Mixin Definition
public class TemplateMixinDef : TemplateDef{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "TemplateMixinDef";
		return ret;
	}
}
