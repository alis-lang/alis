module alis.compiler.ast.exprs.operators.pre;

import std.json,
			 std.conv,
			 std.array,
			 std.algorithm;

import alis.compiler.parser.parser : ASTNode;

import alis.compiler.ast.exprs.operators.base;

/// OpIsPre prefix operator
public class OpIsPre : OpPreExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpIsPre";
		return ret;
	}
public:
	this(){
		this.op = "is";
	}
}

/// OpNotIsPre prefix operator
public class OpNotIsPre : OpPreExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpNotIsPre";
		return ret;
	}
public:
	this(){
		this.op = "!is";
	}
}

/// OpConst prefix operator
public class OpConstPre : OpPreExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpConstPre";
		return ret;
	}
public:
	this(){
		this.op = "const";
	}
}

/// OpNot prefix operator
public class OpNotPre : OpPreExprOverridable{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpNotPre";
		return ret;
	}
public:
	this(){
		this.op = "!";
	}
}

/// OpRef prefix operator
public class OpRefPre : OpPreExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpRefPre";
		return ret;
	}
public:
	this(){
		this.op = "@";
	}
}

/// OpTag prefix operator
public class OpTagPre : OpPreExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpTagPre";
		return ret;
	}
public:
	this(){
		this.op = "#";
	}
}

/// OpBitNot prefix operator
public class OpBitNotPre : OpPreExprOverridable{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpBitNotPre";
		return ret;
	}
public:
	this(){
		this.op = "~";
	}
}
