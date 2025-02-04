module alis.compiler.ast.defs.utests;

import std.json,
			 std.conv,
			 std.array,
			 std.algorithm;

import alis.compiler.ast.exprs,
			 alis.compiler.ast.statements,
			 alis.compiler.ast.misc,
			 alis.compiler.ast.ccomp,
			 alis.compiler.ast.defs;

import alis.compiler.parser.parser : ASTNode;

/// utest node
public class UTest : DefNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (desc)
			ret["desc"] = desc;
		ret["body"] = body.toJson;
		ret["_name"] = "UTest";
		return ret;
	}
public:
	/// test description
	string desc;
	/// test body
	Block body;
}
