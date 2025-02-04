module alis.compiler.ast.defs;

import std.json,
			 std.conv,
			 std.array,
			 std.algorithm;

public import
			 alis.compiler.ast.defs.aliases,
			 alis.compiler.ast.defs.enums,
			 alis.compiler.ast.defs.fns,
			 alis.compiler.ast.defs.structs,
			 alis.compiler.ast.defs.templates,
			 alis.compiler.ast.defs.unions,
			 alis.compiler.ast.defs.utests,
			 alis.compiler.ast.defs.vars;

import alis.compiler.ast.misc;

import alis.compiler.parser.parser : ASTNode;

/// definition node
public abstract class DefNode : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (attrs)
			ret["attrs"] = attrs.toJson;
		ret["name"] = name;
		ret["_name"] = "DefNode";
		return ret;
	}
public:
	/// attributes
	AttrList attrs;
	/// name
	string name;
}

/// global (module level) definition node
public class GlobDef : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["def"] = def.toJson;
		ret["_name"] = "GlobDef";
		ret["visibility"] = visibility.to!string;
		return ret;
	}
public:
	/// visibility
	Visibility visibility = Visibility.Default;
	/// definition node
	DefNode def;
}

/// import
public class Import : DefNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["moduleIdent"] = moduleIdent;
		ret["_name"] = "Import";
		return ret;
	}
public:
	/// module identifier (dot separated strings)
	string[] moduleIdent;
}
