module alis.compiler.ast.exprs.idents;

import std.json,
			 std.conv,
			 std.array,
			 std.algorithm;

import alis.compiler.ast.exprs.base;

import alis.compiler.parser.parser : ASTNode;

/// `auto`
public class AutoExpr : IdentExpr{
public:
	this(){
		this.ident = "auto";
	}
}

/// `this`
public class ThisExpr : IdentExpr{
public:
	this(){
		this.ident = "this";
	}
}

/// `int`
public class IntExpr : IdentExpr{
public:
	this(){
		this.ident = "int";
	}
}

/// `uint`
public class UIntExpr : IdentExpr{
public:
	this(){
		this.ident = "uint";
	}
}

/// `float`
public class FloatExpr : IdentExpr{
public:
	this(){
		this.ident = "float";
	}
}

/// `char`
public class CharExpr : IdentExpr{
public:
	this(){
		this.ident = "char";
	}
}

/// `string`
public class StringExpr : IdentExpr{
public:
	this(){
		this.ident = "string";
	}
}

/// `bool`
public class BoolExpr : IdentExpr{
public:
	this(){
		this.ident = "bool";
	}
}
