module alis.compiler.ast.exprs.base;

import std.json,
			 std.conv,
			 std.array,
			 std.algorithm;

import alis.compiler.ast.statements;

import alis.compiler.parser.parser : ASTNode;

/// expression
public abstract class Expression : Statement{
private:
	Expression _leaf; /// leaf node in _next linked list
	Expression _next; /// next node in resolution chain
	Expression _parent; /// parent node in resolution chain
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (_next)
			ret["_next"] = _next.toJson;
		return ret;
	}
public:
	this() {
		_leaf = this;
		_next = null;
		_parent = null;
	}
	/// parent Expression, if this is a resolved form
	@property Expression parent(Expression val) pure {
		return _parent = val;
	}
	/// next resolved form. or null if this is the leaf
	@property Expression next() pure {
		return _next;
	}
	/// ditto
	@property Expression next(Expression val) pure {
		assert(val !is null);
		_next = val;
		_leaf = _next;
		Expression p = _parent;
		while (p !is null){
			p._leaf = _leaf;
			p = p._parent;
		}
		return _next;
	}
	/// returns: leaf node in resolution chain. will be `this` if this is the leaf
	@property Expression leaf() pure {
		return _leaf;
	}
	override JSONValue toJson() const pure {
		if (_leaf && _leaf !is this){
			JSONValue ret = _leaf.toJson;
			ret["_isResolved"] = JSONValue(true);
			return ret;
		}
		return jsonOf();
	}
}

/// Idenfifier Expression
public class IdentExpr : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["ident"] = ident;
		ret["_name"] = "IdentExpr";
		return ret;
	}
public:
	/// identifier
	string ident;
}

/// block expression
public class BlockExpr : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["type"] = type.toJson;
		ret["block"] = block.toJson;
		ret["_name"] = "BlockExpr";
		return ret;
	}
public:
	/// return type
	Expression type;
	/// block
	Block block;
}

/// intrinsic expression
public class IntrinsicExpr : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["name"] = name;
		ret["_name"] = "IntrinsicExpr";
		return ret;
	}
public:
	/// intrinsic name
	string name;
}
