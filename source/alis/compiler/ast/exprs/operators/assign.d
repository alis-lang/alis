module alis.compiler.ast.exprs.operators.assign;

import std.json,
			 std.conv,
			 std.array,
			 std.algorithm;

import alis.compiler.parser.parser : ASTNode;

import alis.compiler.ast.exprs.operators.base;

/// OpAssign binary operator
public class OpAssignBin : OpBinExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpAssignBin";
		return ret;
	}
public:
	this(){
		this.op = "=";
	}
}

/// compound assignment operators
public abstract class OpAssignCompound : OpAssignBin{
}

/// OpAssignAdd binary operator
public class OpAssignAddBin : OpAssignCompound{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpAssignAddBin";
		return ret;
	}
public:
	this(){
		this.op = "+=";
	}
}

/// OpAssignSub binary operator
public class OpAssignSubBin : OpAssignCompound{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpAssignSubBin";
		return ret;
	}
public:
	this(){
		this.op = "-=";
	}
}

/// OpAssignMul binary operator
public class OpAssignMulBin : OpAssignCompound{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpAssignMulBin";
		return ret;
	}
public:
	this(){
		this.op = "*=";
	}
}

/// OpAssignDiv binary operator
public class OpAssignDivBin : OpAssignCompound{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpAssignDivBin";
		return ret;
	}
public:
	this(){
		this.op = "/=";
	}
}

/// OpAssignMod binary operator
public class OpAssignModBin : OpAssignCompound{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpAssignModBin";
		return ret;
	}
public:
	this(){
		this.op = "%=";
	}
}

/// OpAssignAnd binary operator
public class OpAssignAndBin : OpAssignCompound{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpAssignAndBin";
		return ret;
	}
public:
	this(){
		this.op = "&=";
	}
}

/// OpAssignOr binary operator
public class OpAssignOrBin : OpAssignCompound{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpAssignOrBin";
		return ret;
	}
public:
	this(){
		this.op = "|=";
	}
}

/// OpAssignXor binary operator
public class OpAssignXorBin : OpAssignCompound{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpAssignXorBin";
		return ret;
	}
public:
	this(){
		this.op = "^=";
	}
}

/// OpAssignRef binary operator
public class OpAssignRefBin : OpBinExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpAssignRefBin";
		return ret;
	}
public:
	this(){
		this.op = "@=";
	}
}
