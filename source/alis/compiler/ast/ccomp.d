module alis.compiler.ast.ccomp;

import std.json,
			 std.conv,
			 std.array,
			 std.algorithm;

import alis.compiler.ast.exprs,
			 alis.compiler.ast.misc,
			 alis.compiler.ast.statements;

import alis.compiler.parser.parser : ASTNode;

/// conditional compilation node
public abstract class CCNode : Statement{
public:
}

/// static if statement node
public class StaticIf : CCNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["condition"] = conditions.map!(a => a.toJson).array;
		ret["onTrue"] = onTrue.map!(a => a.toJson).array;
		if (onFalse)
			ret["onFalse"] = onFalse.toJson;
		ret["_name"] = "StaticIf";
		return ret;
	}
public:
	/// conditions
	Expression[] conditions;
	/// on true body, for each condition in conditions
	AnyBlock[] onTrue;
	/// else body, can be null
	AnyBlock onFalse;
}

/// static for statement node
public class StaticFor : CCNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["countIdent"] = countIdent;
		ret["valType"] = valType.toJson;
		ret["valIdent"] = valIdent;
		ret["range"] = range.toJson;
		ret["body"] = body.toJson;
		ret["_name"] = "StaticFor";
		return ret;
	}
public:
	/// counter name, can be null
	string countIdent;
	/// value type
	Expression valType;
	/// value
	string valIdent;
	/// range
	Expression range;
	/// loop body
	AnyBlock body;
}

/// Static Case statement
public class StaticCase : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.toJson;
		ret["body"] = body.toJson;
		ret["_name"] = "StaticCase";
		return ret;
	}
public:
	/// case value
	Expression val;
	/// case block
	AnyBlock body;
}

/// switch case statement
public class StaticSwitch : CCNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.toJson;
		ret["cases"] = cases.map!(a => a.toJson).array;
		ret["cComp"] = cComp.map!(a => a.toJson).array;
		ret["mixinInits"] = mixinInits.map!(a => a.toJson).array;
		ret["_name"] = "StaticSwitch";
		return ret;
	}
public:
	/// value to switch on
	Expression val;
	/// cases
	StaticCase[] cases;
	/// conditional compilation
	CCNode[] cComp;
	/// mixin inits
	MixinInit[] mixinInits;
}

/// Static Default Case statement
public class StaticCaseDef : StaticCase{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "StaticCaseDef";
		return ret;
	}
}
