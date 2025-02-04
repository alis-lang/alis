module alis.compiler.ast.exprs.operators.post;

import std.json,
			 std.conv,
			 std.array,
			 std.algorithm;

import alis.compiler.parser.parser : ASTNode;

import alis.compiler.ast.exprs.operators.base;

/// OpRefPost postfix operator
public class OpRefPost : OpPostExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpRefPost";
		return ret;
	}
public:
	this(){
		this.op = "@";
	}
}

/// OpInc postfix operator
public class OpIncPost : OpPostExprOverridable{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpIncPost";
		return ret;
	}
public:
	this(){
		this.op = "++";
	}
}

/// OpDec postfix operator
public class OpDecPost : OpPostExprOverridable{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpDecPost";
		return ret;
	}
public:
	this(){
		this.op = "--";
	}
}

/// OpDots postfix operator
public class OpDotsPost : OpPostExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpDotsPost";
		return ret;
	}
public:
	this(){
		this.op = "...";
	}
}
