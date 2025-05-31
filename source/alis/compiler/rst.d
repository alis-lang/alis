/++
Resolved AST nodes
+/
module alis.compiler.rst;

import alis.common,
			 alis.compiler.common,
			 alis.compiler.ast;

import std.json,
			 std.conv,
			 std.range,
			 std.array,
			 std.algorithm;

/// Resolved Module
public class RModule : ASTNode{
public:
	/// functions
	RFn[] fns;
	/// whether any function is public
	bool[] fnIsPublic;
	/// init blocks
	RBlock[] initers;
	/// globals (parameters and variables) types
	ADataType[] globalsT;
	/// globals names
	string[] globalsN;
	/// globals visibility
	Visibility[] globalsV;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["fns"] = fns.length.iota
			.map!((size_t i){
					JSONValue f = fns[i].jsonOf;
					f["isPublic"] = fnIsPublic[i];
					return f;
			})
			.array;
		ret["globals"] = globalsT.length.iota
			.map!(i => JSONValue([
						"name": globalsN[i],
						"type": globalsT[i].toString,
						"vis": globalsV.to!string
			]))
			.array;
		return ret;
	}
}

/// Resolved Function
public class RFn : ASTNode{
public:
	/// identifier
	string ident;
	/// body
	RExpr body;
	/// paramter types
	ADataType[] paramsT;
	/// parameter names
	string[] paramsN;
	/// how many of the locals are parameters
	size_t paramCount;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (body)
			ret["body"] = body.jsonOf;
		ret["ident"] = ident;
		ret["locals"] = paramsT.length.iota
			.map!(i => JSONValue(
						["name": paramsN[i], "type": paramsT[i].toString]
						))
			.array;
		ret["paramCount"] = JSONValue(paramCount);
		ret["_name"] = "RFn";
		return ret;
	}
}

/// Resovled Statement
public abstract class RStatement : Statement{
}

/// Resovled Block
public class RBlock : RStatement{
public:
	/// statements
	RStatement[] statements;
	/// locals (parameters and variables) types
	ADataType[] localsT;
	/// locals names
	string[] localsN;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["statements"] = statements.map!(a => a.jsonOf).array;
		ret["_name"] = "RBlock";
		ret["locals"] = localsT.length.iota
			.map!(i => JSONValue(
						["name": localsN[i], "type": localsT[i].toString]
						))
			.array;
		return ret;
	}
}

/// Resovled Return Statement
public class RReturn : RStatement{
public:
	/// return value, can be null
	RExpr val;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (val)
			ret["val"] = val.jsonOf;
		ret["_name"] = "RReturn";
		return ret;
	}
}

/// resovled if statement node
public class RIf : RStatement{
public:
	/// condition
	RExpr condition;
	/// on true statement
	RStatement onTrue;
	/// on false statement (else), can be null
	RStatement onFalse;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["condition"] = condition.jsonOf;
		ret["onTrue"] = onTrue.jsonOf;
		if (onFalse)
			ret["onFalse"] = onFalse.jsonOf;
		ret["_name"] = "RIf";
		return ret;
	}
}

/// resolved for statement node
public class RFor : RStatement{
public:
	/// counter name, can be null
	string countIdent;
	/// value type
	ADataType valType;
	/// value
	string valIdent;
	/// range
	RExpr range;
	/// loop body
	RStatement body;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["countIdent"] = countIdent;
		ret["valType"] = valType.toString;
		ret["valIdent"] = valIdent;
		ret["range"] = range.jsonOf;
		ret["body"] = body.jsonOf;
		ret["_name"] = "RFor";
		return ret;
	}
}

/// resolved while statement node
public class RWhile : RStatement{
public:
	/// condition
	RExpr condition;
	/// loop body
	RStatement body;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["condition"] = condition.jsonOf;
		ret["body"] = body.jsonOf;
		ret["_name"] = "RWhile";
		return ret;
	}
}

/// resolved do while statement node
public class RDoWhile : RStatement{
public:
	/// condition
	RExpr condition;
	/// loop body
	RStatement body;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["condition"] = condition.jsonOf;
		ret["body"] = body.jsonOf;
		ret["_name"] = "RDoWhile";
		return ret;
	}
}

/// resolved Case statement
public class RCase : ASTNode{
public:
	/// case value
	RLiteralExpr val;
	/// case block
	RStatement body;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.jsonOf;
		ret["body"] = body.jsonOf;
		ret["_name"] = "RCase";
		return ret;
	}
}

/// resolved Default Case statement
public class RCaseDef : RCase{
protected:

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RCaseDef";
		return ret;
	}
}

/// resolved switch case statement
public class RSwitch : RStatement{
public:
	/// value to switch on
	RExpr val;
	/// cases
	RCase[] cases;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.jsonOf;
		ret["cases"] = cases.map!(a => a.jsonOf).array;
		ret["_name"] = "RSwitch";
		return ret;
	}
}

/// Resolved Expression
public abstract class RExpr : RStatement{
protected:

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RExpr";
		return ret;
	}
}

/// Resolved Identifier
public class RIdentExpr : RExpr{
public:
	/// identifier
	string ident;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RIdentExpr";
		ret["ident"] = ident;
		return ret;
	}
}

/// Resolved Block Expression
public class RBlockExpr : RExpr{
public:
	/// return type
	ADataType type;
	/// block
	RBlock block;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RBlockExpr";
		ret["type"] = type.toString;
		ret["block"] = block.jsonOf;
		return ret;
	}
}

/// Resolved Intrinsic Expression
public class RIntrinsicExpr : RExpr{
public:
	/// intrinsic name
	string name;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["name"] = name;
		ret["_name"] = "RIntrinsicExpr";
		return ret;
	}
}

/// Resolved Intrinsic Call Expression
public class RIntrinsicCallExpr : RExpr{
public:
	/// intrinsic name
	string name;
	/// parameters
	RExpr[] params;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["name"] = name;
		ret["params"] = params.map!(p => p.jsonOf).array;
		ret["_name"] = "RIntrinsicCallExpr";
		return ret;
	}
}

/// Data Type as a Resolved Expression
public class RDTypeExpr : RExpr{
public:
	/// type
	ADataType type;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["type"] = type.toString;
		ret["_name"] = "RDTypeExpr";
		return ret;
	}
}

/// Resolved Assignment Expression
public class RAssignExpr : RExpr{
public:
	/// left side
	RExpr lhs;
	/// right side
	RExpr rhs;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RAssignExpr";
		ret["lhs"] = lhs.jsonOf;
		ret["rhs"] = rhs.jsonOf;
		return ret;
	}
}

/// Resolved Reference Assign Expression
public class RRefAssignExpr : RExpr{
public:
	/// left side
	RExpr lhs;
	/// right side
	RExpr rhs;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RRefAssignExpr";
		ret["lhs"] = lhs.jsonOf;
		ret["rhs"] = rhs.jsonOf;
		return ret;
	}
}

/// Resovled Dereference Expression
public class RDerefExpr : RExpr{
public:
	/// value
	RExpr val;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RDerefExpr";
		ret["val"] = val.jsonOf;
		return ret;
	}
}

/// Resolved Comma expression
public class RCommaExpr : RExpr{
public:
	/// expressions
	RExpr[] exprs;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RCommaExpr";
		ret["exprs"] = exprs.map!(a => a.jsonOf).array;
		return ret;
	}
}

/// Resolved Function Call Expression
public class RFnCallExpr : RExpr{
public:
	/// callee
	RExpr callee;
	/// parameters
	RExpr[] params;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RFnCallExpr";
		ret["callee"] = callee.jsonOf;
		ret["params"] = params.map!(a => a.jsonOf).array;
		return ret;
	}
}

/// Resolved Prefix `is` Expression
public class RPreIsExpr : RExpr{
public:
	/// operand
	RExpr val;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RIsPre";
		ret["val"] = val.jsonOf;
		return ret;
	}
}

/// Resolved Prefix `!is` Expression
public class RPreNotIsExpr : RExpr{
public:
	/// operand
	RExpr val;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RNotIsPre";
		ret["val"] = val.jsonOf;
		return ret;
	}
}

/// Resovled Prefix `@` Expression
public class RRefExpr : RExpr{
public:
	/// operand
	RExpr val;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RRefExpr";
		ret["val"] = val.jsonOf;
		return ret;
	}
}

/// Resolved VTable Member Get Expression
public class RVTGetExpr : RExpr{
public:
	/// value
	RExpr val;
	/// member name
	string member;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RVTGetExpr";
		ret["val"] = val.jsonOf;
		ret["member"] = member;
		return ret;
	}
}

/// Resolved Member Get Expression
public class RMemberGetExpr : RExpr{
public:
	/// value
	RExpr val;
	/// member name
	string member;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RMemberGetExpr";
		ret["val"] = val.jsonOf;
		ret["member"] = member;
		return ret;
	}
}

/// Resolved Enum Member Get Expression
public class REnumMemberGetExpr : RExpr{
public:
	/// the enum
	RIdentExpr val;
	/// member name
	string member;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "REnumMemberGetExpr";
		ret["val"] = val.jsonOf;
		ret["member"] = member;
		return ret;
	}
}

/// Resolved Function Expression
public class RFnExpr : RExpr{
public:
	/// function
	AFn fn; // TODO: NO. Very Bad!

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RFnExpr";
		ret["fn"] = "FUNCTION_NAME"; // TODO: where AFn.toString?????
		return ret;
	}
}

/// Resolved Struct Literal
public class RStructLiteralExpr : RExpr{
public:
	/// key value pairs of members
	RExpr[string] members;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		JSONValue obj;
		foreach (string name, val; members)
			obj[name] = val.jsonOf;
		ret["members"] = obj;
		ret["_name"] = "RStructLiteralExpr";
		return ret;
	}
}

/// Resolved Array Literal Expression
public class RArrayLiteralExpr : RExpr{
public:
	/// elements
	RExpr[] elements;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["elements"] = elements.map!(a => a.jsonOf).array;
		ret["_name"] = "RArrayLiteralExpr";
		return ret;
	}
}

/// Resolved Literal Value Expression
public class RLiteralExpr : RExpr{
public:
	/// value
	ubyte[] value;
	/// type
	ADataType type;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["value"] = value;
		ret["type"] = type.toString;
		ret["_name"] = "RLiteralExpr";
		return ret;
	}
}
