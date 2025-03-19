/++
AST Node definitions package
+/
module alis.compiler.ast.ast;

import std.array,
			 std.conv,
			 std.algorithm;

import alis.compiler.common : ASTNode, Visibility, IsASTNode;

import std.meta,
			 std.traits;

private template GetAll(){
	alias GetAll = AliasSeq!();
	static foreach (string name; __traits(allMembers, mixin(__MODULE__))){
		static if (is (__traits(getMember, mixin(__MODULE__), name) : ASTNode)){
			GetAll = AliasSeq!(GetAll, __traits(getMember, mixin(__MODULE__), name));
		}
	}
}

/// Sequence of all ASTNodes in this module
alias ASTNodes = GetAll!();

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

// Misc -----------------------------------------------------------------------

/// Key Value pair node (keyIdent = valExpr)
public class KeyVal : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["key"] = key;
		if (val)
			ret["val"] = val.toJson;
		ret["_name"] = "KeyVal";
		return ret;
	}
public:
	/// key ident
	string key;
	/// value
	Expression val;
}

/// Mixin Init
public class MixinInit : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["mixn"] = mixn.toJson;
		ret["params"] = params.map!(a => a.toJson).array;
		ret["_name"] = "MixinInit";
		return ret;
	}
public:
	/// Mixin
	Expression mixn;
	/// Parameters
	Expression[] params;
}

/// Mixin init to definition
public class MixinInitDef : DefNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["mixinInit"] = mixinInit.toJson;
		ret["_name"] = "MixinInitDef";
		return ret;
	}
public:
	/// underlying mixinInit
	MixinInit mixinInit;
	this(){
		name = "_";
	}
}

/// Attribute List
public class AttrList : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (attrs)
			ret["attrs"] = attrs.map!(a => a.toJson).array;
		ret["_name"] = "AttrList";
		return ret;
	}
public:
	/// attributes
	Expression[] attrs;
}

/// Any Block (body is just tokens)
public class AnyBlock : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["body"] = body.map!(a => a.toString).array;
		ret["_name"] = "AnyBlock";
		return ret;
	}
public:
	import alis.compiler.lexer : Tok;
	Tok[] body;
}

/// Aggregate Member List
public class AggMemberList : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (members)
			ret["members"] = members.map!(a => a.toJson).array;
		if (uMembers)
			ret["uMembers"] = uMembers.map!(a => a.toJson).array;
		ret["_name"] = "AggMemberList";
		return ret;
	}
public:
	/// aggregate members
	AggMember[] members;
	/// unnamed members
	UnnamedUnionMember[] uMembers;
}

/// aggregate member (union/struct member)
public abstract class AggMember : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["visibility"] = visibility.to!string;
		if (attrs)
			ret["attrs"] = attrs.toJson;
		ret["_name"] = "AggMember";

		return ret;
	}
public:
	/// visibility
	Visibility visibility;
	/// attributes
	AttrList attrs;
}

/// AggMember in case of alias
public class AggMemberAlias : AggMember{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["name"] = name;
		ret["val"] = val.toJson;
		ret["_name"] = "AggMemberAlias";
		return ret;
	}
public:
	/// name
	string name;
	/// val
	Expression val;
}

/// AggMember named
public class AggMemberNamed : AggMember{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["type"] = type.toJson;
		ret["name"] = name;
		if (val)
			ret["val"] = val.toJson;
		ret["_name"] = "AggMemberNamed";
		return ret;
	}
public:
	/// type
	Expression type;
	/// name
	string name;
	/// default value, can be null
	Expression val;
}

/// AggMember unnamed
public class UnnamedUnionMember : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["type"] = type.toJson;
		if (val)
			ret["val"] = val.toJson;
		if (attrs)
			ret["attrs"] = attrs.toJson;
		ret["_name"] = "UnnamedUnionMember";
		return ret;
	}
public:
	/// attributes
	AttrList attrs;
	/// type
	Expression type;
	/// default value, can be null
	Expression val;
}

// Definitions ----------------------------------------------------------------

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

/// function paramter node
public class FParam : DefNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (type)
			ret["type"] = type.toJson;
		if (val)
			ret["val"] = val.toJson;
		ret["_name"] = "FParam";
		return ret;
	}
public:
	/// type, can be null
	Expression type;
	/// default value, can be null
	Expression val;
}

/// function parameter list
public class FParamList : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["params"] = params.map!(a => a.toJson).array;
		ret["_name"] = "FParamList";
		return ret;
	}
public:
	/// parameterse
	FParam[] params;
}

/// function definition
public class FnDef : DefNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (vt)
			ret["vt"] = vt.toJson;
		ret["params"] = params.toJson;
		if (tParams)
			ret["tParams"] = tParams.toJson;
		ret["body"] = body.toJson;
		ret["_name"] = "FnDef";
		return ret;
	}
public:
	/// vt struct, can be null
	Expression vt;
	/// parameters
	FParamList params;
	/// template paramters, can be null
	TParamList tParams;
	/// body expression
	Expression body;
}

/// abstract enum
public abstract class EnumDef : DefNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["type"] = type.toJson;
		if (tParams)
			ret["tParams"] = tParams.toJson;
		ret["_name"] = "EnumDef";
		return ret;
	}
public:
	/// type
	Expression type;
	/// template paramters, can be null
	TParamList tParams;
}

/// enum constant definition
public class EnumConstDef : EnumDef{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.toJson;
		ret["_name"] = "EnumConstDef";
		return ret;
	}
public:
	/// value
	Expression val;
}

/// enum definition
public class EnumSmDef : EnumDef{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["members"] = members.map!(a => a.toJson).array;
		ret["mixinInits"] = mixinInits.map!(a => a.toJson).array;
		ret["cComp"] = cComp.map!(a => a.toJson).array;
		ret["_name"] = "EnumSmDef";
		return ret;
	}
public:
	/// members
	EnumMember[] members;
	/// mixin inits
	MixinInit[] mixinInits;
	/// conditional compilation
	CCNode[] cComp;
}

/// enum member
public class EnumMember : DefNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (value)
			ret["value"] = value.toJson;
		ret["_name"] = "EnumMember";
		return ret;
	}
public:
	/// value. can be null
	Expression value;
}

/// A struct
public class Struct : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["members"] = members.map!(a => a.toJson).array;
		ret["mixinInits"] = mixinInits.map!(a => a.toJson).array;
		ret["cComp"] = cComp.map!(a => a.toJson).array;
		ret["_name"] = "Struct";
		return ret;
	}
public:
	/// members
	AggMember[] members;
	/// conditional compilation
	CCNode[] cComp;
	/// Mixin Inits
	MixinInit[] mixinInits;
}

/// struct definition
public class StructDef : DefNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["def"] = def.toJson;
		if (tParams)
			ret["tParams"] = tParams.toJson;
		ret["_name"] = "StructDef";
		return ret;
	}
public:
	/// underlying struct
	Struct def;
	/// template paramters, can be null
	TParamList tParams;
}

/// abstract template parameter node
public abstract class TParam : DefNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["specialization"] = specialization.map!(a => a.toJson).array;
		ret["_name"] = "TParam";
		return ret;
	}
public:
	/// specialization
	Expression[] specialization;
}

/// template alias paramter node
public class TParamAlias : TParam{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "TParamAlias";
		return ret;
	}
}

/// template alias sequence parameter node
public class TParamAliasSeq : TParamAlias{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "TParamAliasSeq";
		return ret;
	}
}

/// template $type paramter node
public class TParamType : TParam{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "TParamType";
		return ret;
	}
}

/// template $type sequence parameter node
public class TParamTypeSeq : TParamType{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "TParamTypeSeq";
		return ret;
	}
}

/// template parameter, typical
public class TParamSm : TParam{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["type"] = type.toJson;
		ret["_name"] = "TParamSm";
		return ret;
	}
public:
	/// type
	Expression type;
}

/// template param list
public class TParamList : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["params"] = params.map!(a => a.toJson).array;
		if (condition)
			ret["condition"] = condition.toJson;
		ret["_name"] = "TParamList";
		return ret;
	}
public:
	/// parameters
	TParam[] params;
	/// condition, can be null
	Expression condition;
}

/// template definition
public class TemplateDef : DefNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (tParams)
			ret["tParams"] = tParams.toJson;
		ret["body"] = body.toJson;
		ret["_name"] = "TemplateDef";
		return ret;
	}
public:
	/// template paramters, can be null
	TParamList tParams;
	/// body
	AnyBlock body;
}

/// Mixin Definition
public class TemplateMixinDef : TemplateDef{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "TemplateMixinDef";
		return ret;
	}
}

/// variable definition
public class VarDef : DefNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["type"] = type.toJson;
		if (value)
			ret["value"] = value.toJson;
		ret["_name"] = "VarDef";
		return ret;
	}
public:
	/// type
	Expression type;
	/// optional default value, can be null
	Expression value;
}

/// static variable definition
public class VarStaticDef : VarDef{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "VarStaticDef";
		return ret;
	}
}

/// Variable Definition List
public class VarDefList : DefNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "VarDefList";
		ret["defs"] = defs.map!(a => a.toJson).array;
		return ret;
	}
public:
	/// defs
	VarDef[] defs;
	this(){
		name = "_";
	}
}

/// alias definition
public class AliasDef : DefNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (tParams)
			ret["tParams"] = tParams.toJson;
		ret["val"] = val.toJson;
		ret["_name"] = "AliasDef";
		return ret;
	}
public:
	/// template paramters, can be null
	TParamList tParams;
	/// value
	Expression val;
}

/// An union
public abstract class Union : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["cComp"] = cComp.map!(a => a.toJson).array;
		ret["mixinInits"] = mixinInits.map!(a => a.toJson).array;
		ret["_name"] = "Union";
		return ret;
	}
public:
	/// conditional compilation
	CCNode[] cComp;
	/// mixin inits
	MixinInit[] mixinInits;
}

/// Unknown union (dont know if named or unnamed). Has zero aggMembers, outside
// of CCNode and MixinInit
public class UnkUnion : Union{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "UnkUnion";
		return ret;
	}
}

/// Named Union
public class NamedUnion : Union{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["members"] = members.map!(a => a.toJson).array;
		ret["_name"] = "NamedUnion";
		return ret;
	}
public:
	/// members
	AggMember[] members;
}

/// Unnamed Union
public class UnnamedUnion : Union{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (members)
			ret["members"] = members.map!(a => a.toJson).array;
		ret["_name"] = "UnnamedUnion";
		return ret;
	}
public:
	/// member data types
	UnnamedUnionMember[] members;
}

/// union definition
public class UnionDef : DefNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["def"] = def.toJson;
		if (tParams)
			ret["tParams"] = tParams.toJson;
		ret["_name"] = "UnionDef";
		return ret;
	}
public:
	/// template paramters, can be null
	TParamList tParams;
	/// underlying union
	Union def;
}

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

// Conditional Compilation ----------------------------------------------------

/// conditional compilation node
public abstract class CCNode : Statement{
public:
}

/// static if statement node
public class StaticIf : CCNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["condition"] = conditions.map!(a => a.toJson).array;
		ret["onTrue"] = onTrue.map!(a => a.toJson).array;
		if (onFalse)
			ret["onFalse"] = onFalse.toJson;
		ret["_name"] = "StaticIf";
		return ret;
	}
public:
	/// conditions
	Expression[] conditions;
	/// on true body, for each condition in conditions
	AnyBlock[] onTrue;
	/// else body, can be null
	AnyBlock onFalse;
}

/// static for statement node
public class StaticFor : CCNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["countIdent"] = countIdent;
		ret["valType"] = valType.toJson;
		ret["valIdent"] = valIdent;
		ret["range"] = range.toJson;
		ret["body"] = body.toJson;
		ret["_name"] = "StaticFor";
		return ret;
	}
public:
	/// counter name, can be null
	string countIdent;
	/// value type
	Expression valType;
	/// value
	string valIdent;
	/// range
	Expression range;
	/// loop body
	AnyBlock body;
}

/// Static Case statement
public class StaticCase : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.toJson;
		ret["body"] = body.toJson;
		ret["_name"] = "StaticCase";
		return ret;
	}
public:
	/// case value
	Expression val;
	/// case block
	AnyBlock body;
}

/// switch case statement
public class StaticSwitch : CCNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.toJson;
		ret["cases"] = cases.map!(a => a.toJson).array;
		ret["cComp"] = cComp.map!(a => a.toJson).array;
		ret["mixinInits"] = mixinInits.map!(a => a.toJson).array;
		ret["_name"] = "StaticSwitch";
		return ret;
	}
public:
	/// value to switch on
	Expression val;
	/// cases
	StaticCase[] cases;
	/// conditional compilation
	CCNode[] cComp;
	/// mixin inits
	MixinInit[] mixinInits;
}

/// Static Default Case statement
public class StaticCaseDef : StaticCase{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "StaticCaseDef";
		return ret;
	}
}

// Statements -----------------------------------------------------------------

/// statement node
public abstract class Statement : ASTNode{
public:
}

/// Mixin init to definition
public class MixinInitStmnt : Statement{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["mixinInit"] = mixinInit.toJson;
		ret["_name"] = "MixinInitStmnt";
		return ret;
	}
public:
	/// underlying mixinInit
	MixinInit mixinInit;
}

/// Definition as Statement
public class DefStatement : Statement{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "DefStatement";
		ret["def"] = def.toJson;
		return ret;
	}
public:
	/// definition
	DefNode def;
}

/// statement block node
public class Block : Statement{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["statements"] = statements.map!(a => a.toJson).array;
		ret["_name"] = "Block";
		return ret;
	}
public:
	/// statements
	Statement[] statements;
}

/// return statemment
public class Return : Statement{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (val)
			ret["val"] = val.toJson;
		ret["_name"] = "Return";
		return ret;
	}
public:
	/// return value, can be null
	Expression val;
}

/// if statement node
public class If : Statement{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["condition"] = condition.toJson;
		ret["onTrue"] = onTrue.toJson;
		if (onFalse)
			ret["onFalse"] = onFalse.toJson;
		ret["_name"] = "If";
		return ret;
	}
public:
	/// condition
	Expression condition;
	/// on true statement
	Statement onTrue;
	/// on false statement (else), can be null
	Statement onFalse;
}

/// for statement node
public class For : Statement{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["countIdent"] = countIdent;
		ret["valType"] = valType.toJson;
		ret["valIdent"] = valIdent;
		ret["range"] = range.toJson;
		ret["body"] = body.toJson;
		ret["_name"] = "For";
		return ret;
	}
public:
	/// counter name, can be null
	string countIdent;
	/// value type
	Expression valType;
	/// value
	string valIdent;
	/// range
	Expression range;
	/// loop body
	Statement body;
}

/// while statement node
public class While : Statement{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["condition"] = condition.toJson;
		ret["body"] = body.toJson;
		ret["_name"] = "While";
		return ret;
	}
public:
	/// condition
	Expression condition;
	/// loop body
	Statement body;
}

/// do while statement node
public class DoWhile : Statement{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["condition"] = condition.toJson;
		ret["body"] = body.toJson;
		ret["_name"] = "DoWhile";
		return ret;
	}
public:
	/// condition
	Expression condition;
	/// loop body
	Statement body;
}

/// Case statement
public class Case : ASTNode{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.toJson;
		ret["body"] = body.toJson;
		ret["_name"] = "Case";
		return ret;
	}
public:
	/// case value
	Expression val;
	/// case block
	Statement body;
}

/// Default Case statement
public class CaseDef : Case{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "CaseDef";
		return ret;
	}
}

/// switch case statement
public class Switch : Statement{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.toJson;
		ret["cases"] = cases.map!(a => a.toJson).array;
		ret["cComp"] = cComp.map!(a => a.toJson).array;
		ret["mixinInits"] = mixinInits.map!(a => a.toJson).array;
		ret["_name"] = "Switch";
		return ret;
	}
public:
	/// value to switch on
	Expression val;
	/// cases
	Case[] cases;
	/// conditional compilation
	CCNode[] cComp;
	/// mixin inits
	MixinInit[] mixinInits;
}

// Expressions ----------------------------------------------------------------

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

/// comma separated expressions
public class CommaExpr : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["exprs"] = exprs.map!(a => a.toJson).array;
		ret["_name"] = "CommaExpr";
		return ret;
	}
public:
	/// expressions
	Expression[] exprs;
}

// Expressions / AnonDefs -----------------------------------------------------

/// struct type expression
public class StructAnon : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.toJson;
		ret["_name"] = "StructAnon";
		return ret;
	}
public:
	/// underlying struct
	Struct val;
}

/// anonymous union
public class UnionAnon : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.toJson;
		ret["_name"] = "UnionAnon";
		return ret;
	}
public:
	/// underlying union
	Union val;
}

/// anonymous function expression
public class FnAnonExpr : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["params"] = params.toJson;
		ret["body"] = body.toJson;
		ret["_name"] = "FnAnonExpr";
		return ret;
	}
public:
	/// parameters
	FParamList params;
	/// body
	Expression body;
}

// Expressions / Literals -----------------------------------------------------

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

// Expressions / Intrinsics ---------------------------------------------------

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

// Expressions / Idents -------------------------------------------------------

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

// Operators / Base -----------------------------------------------------------

/// postscript operator
public abstract class OpPostExpr : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["op"] = op;
		ret["operand"] = operand.toJson;
		ret["_name"] = "OpPostExpr";
		return ret;
	}
public:
	/// operator
	string op;
	/// operand
	Expression operand;
}

/// overridable postscript operator
public abstract class OpPostExprOverridable : OpPostExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpPostExprOverridable";
		return ret;
	}
}

/// prefix operator
public abstract class OpPreExpr : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["op"] = op;
		ret["operand"] = operand.toJson;
		ret["_name"] = "OpPreExpr";
		return ret;
	}
public:
	/// operator
	string op;
	/// operand
	Expression operand;
}

/// overridable prefix operator
public abstract class OpPreExprOverridable : OpPreExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpPreExprOverridable";
		return ret;
	}
}

/// binary operator
public abstract class OpBinExpr : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["op"] = op;
		ret["lhs"] = lhs.toJson;
		ret["rhs"] = rhs.toJson;
		ret["_name"] = "OpBinExpr";
		return ret;
	}
public:
	/// operator
	string op;
	/// left operand
	Expression lhs;
	/// right operand
	Expression rhs;
}

/// overridable binary operator
public abstract class OpBinExprOverridable : OpBinExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpBinExprOverridable";
		return ret;
	}
}

/// call expression
public class OpCallExpr : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["callee"] = callee.toJson;
		ret["params"] = params.map!(a => a.toJson).array;
		ret["_name"] = "OpCallExpr";
		return ret;
	}
public:
	/// callee
	Expression callee;
	/// parameters
	Expression[] params;
}

/// index expression
public class OpIndexExpr : Expression{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["lhs"] = lhs.toJson;
		ret["indexes"] = indexes.map!(a => a.toJson).array;
		ret["_name"] = "OpIndexExpr";
		return ret;
	}
public:
	/// lhs
	Expression lhs;
	/// indexes
	Expression[] indexes;
}

// Operators / Assign ---------------------------------------------------------

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

// Operators / Post -----------------------------------------------------------

/// OpRefPost postfix operator
public class OpRefPost : OpPostExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpRefPost";
		return ret;
	}
public:
	this(){
		this.op = "@";
	}
}

/// OpInc postfix operator
public class OpIncPost : OpPostExprOverridable{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpIncPost";
		return ret;
	}
public:
	this(){
		this.op = "++";
	}
}

/// OpDec postfix operator
public class OpDecPost : OpPostExprOverridable{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpDecPost";
		return ret;
	}
public:
	this(){
		this.op = "--";
	}
}

/// OpDots postfix operator
public class OpDotsPost : OpPostExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpDotsPost";
		return ret;
	}
public:
	this(){
		this.op = "...";
	}
}

// Operators / Pre ------------------------------------------------------------

/// OpIsPre prefix operator
public class OpIsPre : OpPreExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpIsPre";
		return ret;
	}
public:
	this(){
		this.op = "is";
	}
}

/// OpNotIsPre prefix operator
public class OpNotIsPre : OpPreExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpNotIsPre";
		return ret;
	}
public:
	this(){
		this.op = "!is";
	}
}

/// OpConst prefix operator
public class OpConstPre : OpPreExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpConstPre";
		return ret;
	}
public:
	this(){
		this.op = "const";
	}
}

/// OpNot prefix operator
public class OpNotPre : OpPreExprOverridable{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpNotPre";
		return ret;
	}
public:
	this(){
		this.op = "!";
	}
}

/// OpRef prefix operator
public class OpRefPre : OpPreExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpRefPre";
		return ret;
	}
public:
	this(){
		this.op = "@";
	}
}

/// OpTag prefix operator
public class OpTagPre : OpPreExpr{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpTagPre";
		return ret;
	}
public:
	this(){
		this.op = "#";
	}
}

/// OpBitNot prefix operator
public class OpBitNotPre : OpPreExprOverridable{
protected:
	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "OpBitNotPre";
		return ret;
	}
public:
	this(){
		this.op = "~";
	}
}

// Operators / Bin ------------------------------------------------------------

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

// Operators / Errors ---------------------------------------------------------

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
