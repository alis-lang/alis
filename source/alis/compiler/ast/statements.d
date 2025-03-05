module alis.compiler.ast.statements;

import std.json,
			 std.conv,
			 std.array,
			 std.algorithm;

import alis.compiler.ast.defs,
			 alis.compiler.ast.exprs,
			 alis.compiler.ast.ccomp,
			 alis.compiler.ast.misc;

import alis.compiler.parser.parser : ASTNode;

/// statement node
public abstract class Statement : ASTNode{
public:
}

/// Mixin init to definition
public class MixinInitStmnt : Statement{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["mixinInit"] = mixinInit.toJson;
		ret["_name"] = "MixinInitStmnt";
		return ret;
	}
public:
	/// underlying mixinInit
	MixinInit mixinInit;
}

/// Definition as Statement
public class DefStatement : Statement{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "DefStatement";
		ret["def"] = def.toJson;
		return ret;
	}
public:
	/// definition
	DefNode def;
}

/// statement block node
public class Block : Statement{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["statements"] = statements.map!(a => a.toJson).array;
		ret["_name"] = "Block";
		return ret;
	}
public:
	/// statements
	Statement[] statements;
}

/// return statemment
public class Return : Statement{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (val)
			ret["val"] = val.toJson;
		ret["_name"] = "Return";
		return ret;
	}
public:
	/// return value, can be null
	Expression val;
}

/// if statement node
public class If : Statement{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["condition"] = condition.toJson;
		ret["onTrue"] = onTrue.toJson;
		if (onFalse)
			ret["onFalse"] = onFalse.toJson;
		ret["_name"] = "If";
		return ret;
	}
public:
	/// condition
	Expression condition;
	/// on true statement
	Statement onTrue;
	/// on false statement (else), can be null
	Statement onFalse;
}

/// for statement node
public class For : Statement{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["countIdent"] = countIdent;
		ret["valType"] = valType.toJson;
		ret["valIdent"] = valIdent;
		ret["range"] = range.toJson;
		ret["body"] = body.toJson;
		ret["_name"] = "For";
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
	Statement body;
}

/// while statement node
public class While : Statement{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["condition"] = condition.toJson;
		ret["body"] = body.toJson;
		ret["_name"] = "While";
		return ret;
	}
public:
	/// condition
	Expression condition;
	/// loop body
	Statement body;
}

/// do while statement node
public class DoWhile : Statement{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["condition"] = condition.toJson;
		ret["body"] = body.toJson;
		ret["_name"] = "DoWhile";
		return ret;
	}
public:
	/// condition
	Expression condition;
	/// loop body
	Statement body;
}

/// Case statement
public class Case : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.toJson;
		ret["body"] = body.toJson;
		ret["_name"] = "Case";
		return ret;
	}
public:
	/// case value
	Expression val;
	/// case block
	Statement body;
}

/// Default Case statement
public class CaseDef : Case{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "CaseDef";
		return ret;
	}
}

/// switch case statement
public class Switch : Statement{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.toJson;
		ret["cases"] = cases.map!(a => a.toJson).array;
		ret["cComp"] = cComp.map!(a => a.toJson).array;
		ret["mixinInits"] = mixinInits.map!(a => a.toJson).array;
		ret["_name"] = "Switch";
		return ret;
	}
public:
	/// value to switch on
	Expression val;
	/// cases
	Case[] cases;
	/// conditional compilation
	CCNode[] cComp;
	/// mixin inits
	MixinInit[] mixinInits;
}
