module alis.compiler.ast.exprs.intrinsics;

import std.json,
			 std.conv,
			 std.array,
			 std.algorithm;

import alis.compiler.ast.exprs.base;

import alis.compiler.parser.parser : ASTNode;

/// `$type` intrinsic
public class IntrType : IntrinsicExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "IntrType";
		return ret;
	}
public:
	this(){
		this.name = "type";
	}
}

/// `$noinit` intrinsic
public class IntrNoInit : IntrinsicExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "IntrNoInit";
		return ret;
	}
public:
	this(){
		this.name = "noinit";
	}
}

/// `$noinitval` intrinsic
public class IntrNoInitVal : IntrinsicExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "IntrNoInitVal";
		return ret;
	}
public:
	this(){
		this.name = "noinitval";
	}
}

/// `$int` intrinsic
public class IntrInt : IntrinsicExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "IntrInt";
		return ret;
	}
public:
	this(){
		this.name = "int";
	}
}

/// `$uint` intrinsic
public class IntrUInt : IntrinsicExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "IntrUInt";
		return ret;
	}
public:
	this(){
		this.name = "uint";
	}
}

/// `$float` intrinsic
public class IntrFloat : IntrinsicExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "IntrFloat";
		return ret;
	}
public:
	this(){
		this.name = "float";
	}
}

/// `$char` intrinsic
public class IntrChar : IntrinsicExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "IntrChar";
		return ret;
	}
public:
	this(){
		this.name = "char";
	}
}

/// `$slice` intrinsic
public class IntrSlice : IntrinsicExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "IntrSlice";
		return ret;
	}
public:
	this(){
		this.name = "slice";
	}
}

/// `$array` intrinsic
public class IntrArray : IntrinsicExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "IntrArray";
		return ret;
	}
public:
	this(){
		this.name = "array";
	}
}

/// `$arrayLen` intrinsic
public class IntrArrayLen : IntrinsicExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "IntrArrayLen";
		return ret;
	}
public:
	this(){
		this.name = "arrayLen";
	}
}

/// `$arrayInd` intrinsic
public class IntrArrayInd : IntrinsicExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "IntrArrayInd";
		return ret;
	}
public:
	this(){
		this.name = "arrayInd";
	}
}

/// `$unionIs` intrinsic
public class IntrUnionIs : IntrinsicExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "IntrUnionIs";
		return ret;
	}
public:
	this(){
		this.name = "unionIs";
	}
}

/// `$vt` intrinsic
public class IntrVt : IntrinsicExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "IntrVt";
		return ret;
	}
public:
	this(){
		this.name = "vt";
	}
}

/// `$attrsOf` intrinsic
public class IntrAttrsOf : IntrinsicExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "IntrAttrsOf";
		return ret;
	}
public:
	this(){
		this.name = "attrsOf";
	}
}

/// `$byAttrs` intrinsic
public class IntrByAttrs : IntrinsicExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "IntrByAttrs";
		return ret;
	}
public:
	this(){
		this.name = "byAttrs";
	}
}

/// `$debug` intrinsic
public class IntrDebug : IntrinsicExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "IntrDebug";
		return ret;
	}
public:
	this(){
		this.name = "debug";
	}
}

/// `$stackTrace` intrinsic
public class IntrStackTrace : IntrinsicExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "IntrStackTrace";
		return ret;
	}
public:
	this(){
		this.name = "stackTrace";
	}
}

/// `$isType` intrinsic
public class IntrIsType : IntrinsicExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "IntrIsType";
		return ret;
	}
public:
	this(){
		this.name = "isType";
	}
}

/// `$seqLen` intrinsic
public class IntrSeqLen : IntrinsicExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "IntrSeqLen";
		return ret;
	}
public:
	this(){
		this.name = "seqLen";
	}
}

/// `$seqInd` intrinsic
public class IntrSeqInd : IntrinsicExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "IntrSeqInd";
		return ret;
	}
public:
	this(){
		this.name = "seqInd";
	}
}

/// `$err` intrinsic
public class IntrErr : IntrinsicExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "IntrErr";
		return ret;
	}
public:
	this(){
		this.name = "err";
	}
}

/// `$typeOf` intrinsic
public class IntrTypeOf : IntrinsicExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "IntrTypeOf";
		return ret;
	}
public:
	this(){
		this.name = "typeOf";
	}
}
