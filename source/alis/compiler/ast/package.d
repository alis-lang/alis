/++
AST Node definitions package
+/
module alis.compiler.ast;

import std.array,
			 std.algorithm;

public import alis.compiler.parser.parser : ASTNode;

public import
			 alis.compiler.ast.defs,
			 alis.compiler.ast.exprs,
			 alis.compiler.ast.ccomp,
			 alis.compiler.ast.misc,
			 alis.compiler.ast.statements;

/// alis module
public class Module : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["defs"] = defs.map!(a => a.toJson).array;
		ret["cComp"] = cComp.map!(a => a.toJson).array;
		ret["_name"] = "Module";
		return ret;
	}
public:
	/// module identifier (can have dots)
	string ident;
	/// definitions
	GlobDef[] defs;
	/// conditional compilation
	CCNode[] cComp;
}
