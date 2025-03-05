module alis.compiler.ast.exprs.operators.error;

import std.json,
			 std.conv,
			 std.array,
			 std.algorithm;

import alis.compiler.parser.parser : ASTNode;

import alis.compiler.ast.exprs.base,
			 alis.compiler.ast.exprs.operators;

/// OpNot postfix operator
public class OpNotPost : OpPostExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpNotPost";
		return ret;
	}
public:
	this(){
		this.op = "!";
	}
}

/// OpQ postfix operator
public class OpQPost : OpPostExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpQPost";
		return ret;
	}
public:
	this(){
		this.op = "?";
	}
}

/// OpNotNot binary operator
public class OpNotNotBin : OpBinExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpNotNotBin";
		return ret;
	}
public:
	this(){
		this.op = "!!";
	}
}

/// OpQQ binary operator
public class OpQQBin : OpBinExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpQQBin";
		return ret;
	}
public:
	this(){
		this.op = "??";
	}
}
