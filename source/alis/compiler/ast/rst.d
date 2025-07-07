/++
Resolved AST nodes
+/
module alis.compiler.ast.rst;

import alis.common,
			 alis.compiler.common,
			 alis.compiler.ast;

import std.json,
			 std.conv,
			 std.range,
			 std.array,
			 std.algorithm;

import std.meta;

public alias RSTIter(Fns...) =
	Instantiate!(alis.compiler.ast.iter.ASTIter!RSTNodes, Fns);

private template GetAll(){
	alias GetAll = AliasSeq!(RFn, RModule);
	static foreach (string name; __traits(allMembers, mixin(__MODULE__))){
		static if (is (__traits(getMember, mixin(__MODULE__), name) : RStatement)){
			GetAll = AliasSeq!(GetAll, __traits(getMember, mixin(__MODULE__), name));
		}
	}
}

/// Sequence of all Nodes in this module that are children of RStatement
public alias RSTNodes = GetAll!();

/// Resolved Module
public class RModule : ASTNode{
public:
	/// functions
	RFn[] fns;
	/// init blocks
	RBlock[] initers;
	/// globals data types
	ADataType[] globalsT;
	/// globals names
	string[] globalsN;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["fns"] = fns.map!(f => f.jsonOf).array;
		ret["globals"] = globalsN.length.iota
			.map!(i => ["name": globalsN[i], "type": globalsT[i].toString].JSONValue)
			.array;
		ret["init"] = initers.map!(i => i.jsonOf).array;
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
		ret["_name"] = "RFn";
		return ret;
	}
}

/// Resovled Statement
public abstract class RStatement : Statement{}

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

/// Resolved ReturnFromFunction Statement
public class RReturnFn : RStatement{
public:
	/// return value, can be null
	RExpr val;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (val)
			ret["val"] = val.jsonOf;
		ret["_name"] = "RReturnFn";
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

/// resolved switch case statement
public class RSwitch : RStatement{
public:
	/// value to switch on
	RExpr val;
	/// values to match against
	RLiteralExpr[] vals;
	/// statements to jump to
	RStatement[] stmnts;
	/// default statement, if any
	RStatement def;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.jsonOf;
		if (def)
			ret["default"] = def.jsonOf;
		JSONValue cases;
		foreach (size_t i; 0 .. vals.length)
			cases[vals[i].toString] = stmnts[i].jsonOf;
		ret["cases"] = cases;
		ret["_name"] = "RSwitch";
		return ret;
	}
}

/// Resolved Expression
public abstract class RExpr : RStatement{
public:
	/// whether `type` has been set
	bool hasType = false;
	/// `typeOf` for this expression. Only valid if `this.hasType`
	ADataType type;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RExpr";
		if (hasType)
			ret["type"] = type.toString;
		return ret;
	}

	override string toString() const pure {
		return "TODO"; // TODO: implement RExpr.toString
	}
}

/// Resolved Var Get expression
public class RVarExpr : RExpr{
public:
	/// var
	AVar var;

	this(){}
	this()(auto ref AVar var){
		this.var = var;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RVarExpr";
		ret["var"] = var.toString;
		return ret;
	}
}

/// Resolved Block Expression
public class RBlockExpr : RExpr{
public:
	/// block
	RBlock block;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RBlockExpr";
		ret["block"] = block.jsonOf;
		return ret;
	}
}

/// Resolved Assignment Expression
public class RVarAssignExpr : RExpr{
public:
	/// variable
	AVar var;
	/// value
	RExpr val;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RVarAssignExpr";
		ret["var"] = var.toString;
		ret["val"] = val.jsonOf;
		return ret;
	}
}

/// Resolved Reference Assign Expression
public class RRefAssignExpr : RExpr{
public:
	/// left side. will evaluate to a reference
	RExpr refExpr;
	/// right side. will evaluate to type being referenced
	RExpr valExpr;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RRefAssignExpr";
		ret["refExpr"] = refExpr.jsonOf;
		ret["valExpr"] = refExpr.jsonOf;
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

/// Resolved Function Expression
public class RFnExpr : RExpr{
public:
	/// function
	AFn fn;

	this(){}
	this()(auto ref AFn fn){
		this.fn = fn;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RFnExpr";
		ret["fn"] = fn.toString;
		return ret;
	}
}

/// Resolved Struct Literal
public class RStructLiteralExpr : RExpr{
public:
	/// key value pairs of members
	/// member names
	string[] names;
	/// member values
	RExpr[] vals;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		JSONValue obj;
		foreach (size_t i; 0 .. names.length){
			JSONValue sub;
			sub["name"] = names[i];
			sub["val"] = vals[i].jsonOf;
			obj[i] = sub;
		}
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
