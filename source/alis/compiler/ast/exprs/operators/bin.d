module alis.compiler.ast.exprs.operators.bin;

import std.json,
			 std.conv,
			 std.array,
			 std.algorithm;

import alis.compiler.parser.parser : ASTNode;

import alis.compiler.ast.exprs.base,
			 alis.compiler.ast.exprs.operators.base;

/// OpArrow binary operator
public class OpArrowBin : OpBinExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpArrowBin";
		return ret;
	}
public:
	this(){
		this.op = "->";
	}
}

/// OpComma binary operator
public class OpCommaBin : OpBinExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpCommaBin";
		return ret;
	}
public:
	this(){
		this.op = ",";
	}
}

/// OpDot binary operator
public class OpDotBin : OpBinExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpDotBin";
		return ret;
	}
public:
	this(){
		this.op = ".";
	}
}

/// OpMul binary operator
public class OpMulBin : OpBinExprOverridable{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpMulBin";
		return ret;
	}
public:
	this(){
		this.op = "*";
	}
}

/// OpDiv binary operator
public class OpDivBin : OpBinExprOverridable{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpDivBin";
		return ret;
	}
public:
	this(){
		this.op = "/";
	}
}

/// OpMod binary operator
public class OpModBin : OpBinExprOverridable{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpModBin";
		return ret;
	}
public:
	this(){
		this.op = "%";
	}
}

/// OpAdd binary operator
public class OpAddBin : OpBinExprOverridable{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpAddBin";
		return ret;
	}
public:
	this(){
		this.op = "+";
	}
}

/// OpSub binary operator
public class OpSubBin : OpBinExprOverridable{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpSubBin";
		return ret;
	}
public:
	this(){
		this.op = "-";
	}
}

/// OpLS binary operator
public class OpLSBin : OpBinExprOverridable{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpLSBin";
		return ret;
	}
public:
	this(){
		this.op = "<<";
	}
}

/// OpRS binary operator
public class OpRSBin : OpBinExprOverridable{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpRSBin";
		return ret;
	}
public:
	this(){
		this.op = ">>";
	}
}

/// OpEq binary operator
public class OpEqBin : OpBinExprOverridable{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpEqBin";
		return ret;
	}
public:
	this(){
		this.op = "==";
	}
}

/// OpNotEq binary operator
public class OpNotEqBin : OpBinExprOverridable{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpNotEqBin";
		return ret;
	}
public:
	this(){
		this.op = "!=";
	}
}

/// OpGrEq binary operator
public class OpGrEqBin : OpBinExprOverridable{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpGrEqBin";
		return ret;
	}
public:
	this(){
		this.op = ">=";
	}
}

/// OpLsEq binary operator
public class OpLsEqBin : OpBinExprOverridable{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpLsEqBin";
		return ret;
	}
public:
	this(){
		this.op = "<=";
	}
}

/// OpGr binary operator
public class OpGrBin : OpBinExprOverridable{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpGrBin";
		return ret;
	}
public:
	this(){
		this.op = ">";
	}
}

/// OpLs binary operator
public class OpLsBin : OpBinExprOverridable{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpLsBin";
		return ret;
	}
public:
	this(){
		this.op = "<";
	}
}

/// OpColon binary operator
public class OpColonBin : OpBinExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpColonBin";
		return ret;
	}
public:
	this(){
		this.op = ":";
	}
}

/// OpIs binary operator
public class OpIsBin : OpBinExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpIsBin";
		return ret;
	}
public:
	this(){
		this.op = "is";
	}
}

/// OpNotIs binary operator
public class OpNotIsBin : OpBinExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpNotIsBin";
		return ret;
	}
public:
	this(){
		this.op = "!is";
	}
}

/// OpBitAnd binary operator
public class OpBitAndBin : OpBinExprOverridable{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpBitAndBin";
		return ret;
	}
public:
	this(){
		this.op = "&";
	}
}

/// OpBitOr binary operator
public class OpBitOrBin : OpBinExprOverridable{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpBitOrBin";
		return ret;
	}
public:
	this(){
		this.op = "|";
	}
}

/// OpBitXor binary operator
public class OpBitXorBin : OpBinExprOverridable{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpBitXorBin";
		return ret;
	}
public:
	this(){
		this.op = "^";
	}
}

/// OpAnd binary operator
public class OpAndBin : OpBinExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpAndBin";
		return ret;
	}
public:
	this(){
		this.op = "&&";
	}
}

/// OpOr binary operator
public class OpOrBin : OpBinExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpOrBin";
		return ret;
	}
public:
	this(){
		this.op = "||";
	}
}
