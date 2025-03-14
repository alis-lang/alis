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
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["fns"] = fns.length.iota
			.map!((size_t i){
					JSONValue f = fns[i].toJson;
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
}

/// Resolved Function
public class RFn : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["body"] = body.toJson;
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
}

/// Resovled Statement
public abstract class RStatement : Statement{
}

/// Resovled Block
public class RBlock : RStatement{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["statements"] = statements.map!(a => a.toJson).array;
		ret["_name"] = "RBlock";
		ret["locals"] = localsT.length.iota
			.map!(i => JSONValue(
						["name": localsN[i], "type": localsT[i].toString]
						))
			.array;
		return ret;
	}
public:
	/// statements
	RStatement[] statements;
	/// locals (parameters and variables) types
	ADataType[] localsT;
	/// locals names
	string[] localsN;
}

/// Resovled Return Statement
public class RReturn : RStatement{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (val)
			ret["val"] = val.toJson;
		ret["_name"] = "RReturn";
		return ret;
	}
public:
	/// return value, can be null
	RExpr val;
}

/// resovled if statement node
public class RIf : RStatement{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["condition"] = condition.toJson;
		ret["onTrue"] = onTrue.toJson;
		if (onFalse)
			ret["onFalse"] = onFalse.toJson;
		ret["_name"] = "RIf";
		return ret;
	}
public:
	/// condition
	RExpr condition;
	/// on true statement
	RStatement onTrue;
	/// on false statement (else), can be null
	RStatement onFalse;
}

/// resolved for statement node
public class RFor : RStatement{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["countIdent"] = countIdent;
		ret["valType"] = valType.toString;
		ret["valIdent"] = valIdent;
		ret["range"] = range.toJson;
		ret["body"] = body.toJson;
		ret["_name"] = "RFor";
		return ret;
	}
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
}

/// resolved while statement node
public class RWhile : RStatement{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["condition"] = condition.toJson;
		ret["body"] = body.toJson;
		ret["_name"] = "RWhile";
		return ret;
	}
public:
	/// condition
	RExpr condition;
	/// loop body
	RStatement body;
}

/// resolved do while statement node
public class RDoWhile : RStatement{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["condition"] = condition.toJson;
		ret["body"] = body.toJson;
		ret["_name"] = "RDoWhile";
		return ret;
	}
public:
	/// condition
	RExpr condition;
	/// loop body
	RStatement body;
}

/// resolved Case statement
public class RCase : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.toJson;
		ret["body"] = body.toJson;
		ret["_name"] = "RCase";
		return ret;
	}
public:
	/// case value
	RLiteralExpr val;
	/// case block
	RStatement body;
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
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.toJson;
		ret["cases"] = cases.map!(a => a.toJson).array;
		ret["_name"] = "RSwitch";
		return ret;
	}
public:
	/// value to switch on
	RExpr val;
	/// cases
	RCase[] cases;
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
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RIdentExpr";
		ret["ident"] = ident;
		return ret;
	}
public:
	/// identifier
	string ident;
}

/// Resolved Block Expression
public class RBlockExpr : RExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RBlockExpr";
		ret["type"] = type.toString;
		ret["block"] = block.toJson;
		return ret;
	}
public:
	/// return type
	ADataType type;
	/// block
	RBlock block;
}

/// Resolved Intrinsic Expression
public class RIntrinsicExpr : RExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["name"] = name;
		ret["_name"] = "RIntrinsicExpr";
		return ret;
	}
public:
	/// intrinsic name
	string name;
}

/// Resolved Intrinsic Call Expression
public class RIntrinsicCallExpr : RExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["name"] = name;
		ret["params"] = params.map!(p => p.toJson).array;
		ret["_name"] = "RIntrinsicCallExpr";
		return ret;
	}
public:
	/// intrinsic name
	string name;
	/// parameters
	RExpr[] params;
}

/// Data Type as a Resolved Expression
public class RDTypeExpr : RExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["type"] = type.toString;
		ret["_name"] = "RDTypeExpr";
		return ret;
	}
public:
	/// type
	ADataType type;
}

/// Resolved Assignment Expression
public class RAssignExpr : RExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RAssignExpr";
		ret["lhs"] = lhs.toJson;
		ret["rhs"] = rhs.toJson;
		return ret;
	}
public:
	/// left side
	RExpr lhs;
	/// right side
	RExpr rhs;
}

/// Resolved Reference Assign Expression
public class RRefAssignExpr : RExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RRefAssignExpr";
		ret["lhs"] = lhs.toJson;
		ret["rhs"] = rhs.toJson;
		return ret;
	}
public:
	/// left side
	RExpr lhs;
	/// right side
	RExpr rhs;
}

/// Resovled Dereference Expression
public class RDerefExpr : RExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RDerefExpr";
		ret["val"] = val.toJson;
		return ret;
	}
public:
	/// value
	RExpr val;
}

/// Resolved Comma expression
public class RCommaExpr : RExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RCommaExpr";
		ret["exprs"] = exprs.map!(a => a.toJson).array;
		return ret;
	}
public:
	/// expressions
	RExpr[] exprs;
}

/// Resolved Function Call Expression
public class RFnCallExpr : RExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RFnCallExpr";
		ret["callee"] = callee.toJson;
		ret["params"] = params.map!(a => a.toJson).array;
		return ret;
	}
public:
	/// callee
	RExpr callee;
	/// parameters
	RExpr[] params;
}

/// Resolved Prefix `is` Expression
public class RPreIsExpr : RExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RIsPre";
		ret["val"] = val.toJson;
		return ret;
	}
public:
	/// operand
	RExpr val;
}

/// Resolved Prefix `!is` Expression
public class RPreNotIsExpr : RExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RNotIsPre";
		ret["val"] = val.toJson;
		return ret;
	}
public:
	/// operand
	RExpr val;
}

/// Resovled Prefix `@` Expression
public class RRefExpr : RExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RRefExpr";
		ret["val"] = val.toJson;
		return ret;
	}
public:
	/// operand
	RExpr val;
}

/// Resolved VTable Member Get Expression
public class RVTGetExpr : RExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RVTGetExpr";
		ret["val"] = val.toJson;
		ret["member"] = member;
		return ret;
	}
public:
	/// value
	RExpr val;
	/// member name
	string member;
}

/// Resolved Member Get Expression
public class RMemberGetExpr : RExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RMemberGetExpr";
		ret["val"] = val.toJson;
		ret["member"] = member;
		return ret;
	}
public:
	/// value
	RExpr val;
	/// member name
	string member;
}

/// Resolved Function Expression
public class RFnExpr : RExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RFnExpr";
		ret["fn"] = "FUNCTION_NAME"; // TODO: where AFn.toString?????
		return ret;
	}
public:
	/// function
	AFn fn; // TODO: NO. Very Bad!
}

/// Resolved Struct Literal
public class RStructLiteralExpr : RExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		JSONValue obj;
		foreach (string name, val; members)
			obj[name] = val.toJson;
		ret["members"] = obj;
		ret["_name"] = "RStructLiteralExpr";
		return ret;
	}
public:
	/// key value pairs of members
	RExpr[string] members;
}

/// Resolved Array Literal Expression
public class RArrayLiteralExpr : RExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["elements"] = elements.map!(a => a.toJson).array;
		ret["_name"] = "RArrayLiteralExpr";
		return ret;
	}
public:
	/// elements
	RExpr[] elements;
}

/// Resolved Literal Value Expression
public class RLiteralExpr : RExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["value"] = value;
		ret["type"] = type.toString;
		ret["_name"] = "RLiteralExpr";
		return ret;
	}
public:
	/// value
	ubyte[] value;
	/// type
	ADataType type;
}
