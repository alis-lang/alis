module alis.compiler.ast.exprs.literals;

import std.json,
			 std.conv,
			 std.array,
			 std.algorithm;

import alis.compiler.ast.exprs.base,
			 alis.compiler.ast.ccomp,
			 alis.compiler.ast.misc;


import alis.compiler.parser.parser : ASTNode;

/// struct literal expression
public class StructLiteralExpr : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["keyVals"] = keyVals.map!(a => a.toJson).array;
		ret["cComp"] = cComp.map!(a => a.toJson).array;
		ret["_name"] = "StructLiteralExpr";
		return ret;
	}
public:
	/// key value pairs of members
	KeyVal[] keyVals;
	/// conditional compilation
	CCNode[] cComp;
}

/// boolean literal expression
public class BoolLiteralExpr : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val;
		ret["_name"] = "BoolLiteralExpr";
		return ret;
	}
public:
	/// value
	bool val;
}

/// literal integer expression
public class LiteralIntExpr : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val;
		ret["_name"] = "LiteralIntExpr";
		return ret;
	}
public:
	/// value
	ptrdiff_t val;
}

/// literal float expression
public class LiteralFloatExpr : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val;
		ret["_name"] = "LiteralFloatExpr";
		return ret;
	}
public:
	/// value
	double val;
}

/// literal string expression
public class LiteralStringExpr : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val;
		ret["_name"] = "LiteralStringExpr";
		return ret;
	}
public:
	/// value
	string val;
}

/// literal character expression
public class LiteralCharExpr : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val;
		ret["_name"] = "LiteralCharExpr";
		return ret;
	}
public:
	/// value
	char val;
}

/// literal array expression
public class LiteralArrayExpr : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["elements"] = elements.map!(a => a.toJson).array;
		ret["_name"] = "LiteralArrayExpr";
		return ret;
	}
public:
	/// elements
	Expression[] elements;
}
