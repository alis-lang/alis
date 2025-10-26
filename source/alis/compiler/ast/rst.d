/++
Resolved AST nodes
+/
module alis.compiler.ast.rst;

import alis.common,
			 alis.compiler.common,
			 alis.compiler.ast;

import std.json,
			 std.conv,
			 std.range,
			 std.array,
			 std.format,
			 std.algorithm;

import std.meta;

static import alis.compiler.ast.iter;

public alias RSTIter(Fns...) =
	Instantiate!(alis.compiler.ast.iter.ASTIter!RSTNodes, Fns);

private template GetAll(){
	alias GetAll = AliasSeq!(RFn);
	static foreach (string name; __traits(allMembers, mixin(__MODULE__))){
		static if (is (__traits(getMember, mixin(__MODULE__), name) : RStatement)){
			GetAll = AliasSeq!(GetAll, __traits(getMember, mixin(__MODULE__), name));
		}
	}
}

/// Sequence of all Nodes in this module that are children of RStatement
public alias RSTNodes = GetAll!();

/// Resolved Function
public class RFn : ASTNode{
public:
	/// identifier
	string ident;
	/// body
	RExpr body;
	/// paramter types
	ADataType[] paramsT;
	/// parameter names
	string[] paramsN;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (body)
			ret["body"] = body.jsonOf;
		ret["ident"] = ident;
		ret["locals"] = paramsT.length.iota
			.map!(i => JSONValue(
						["name": paramsN[i], "type": paramsT[i].toString]
						))
			.array;
		ret["_name"] = "RFn";
		return ret;
	}

	override string toString() const pure {
		return format!"fn %s (%s)->%s"(ident, paramsT.length.iota.map!(
					i => format!"%s:%s"(paramsN[i], paramsT[i])), body);
	}
}

/// Resolved Statement
public abstract class RStatement : Statement{
public:
	override abstract string toString() const pure;
}

/// Resolved Block
public class RBlock : RStatement{
public:
	/// statements
	RStatement[] statements;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["statements"] = statements.map!(a => a.jsonOf).array;
		ret["_name"] = "RBlock";
		return ret;
	}

	override string toString() const pure {
		return format!"block{%s}"(statements.map!(s => s.toString).join("; "));
	}
}

/// Resolved Return Statement
public class RReturn : RStatement{
public:
	/// return value, can be null
	RExpr val;
	/// context
	IdentU[] ctx;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (val)
			ret["val"] = val.jsonOf;
		ret["_name"] = "RReturn";
		return ret;
	}

	override string toString() const pure {
		return format!"return(%s)"(val);
	}
}

/// Resolved ReturnFromFunction Statement
public class RReturnFn : RStatement{
public:
	/// return value, can be null
	RExpr val;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		if (val)
			ret["val"] = val.jsonOf;
		ret["_name"] = "RReturnFn";
		return ret;
	}

	override string toString() const pure {
		return format!"returnFn(%s)"(val);
	}
}

/// resolved if statement node
public class RIf : RStatement{
public:
	/// condition
	RExpr condition;
	/// on true statement
	RStatement[] onTrue;
	/// on false statement (else), can be null
	RStatement[] onFalse;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["condition"] = condition.jsonOf;
		ret["onTrue"] = onTrue.map!(s => s.jsonOf).array;
		if (onFalse)
			ret["onFalse"] = onFalse.map!(s => s.jsonOf).array;
		ret["_name"] = "RIf";
		return ret;
	}

	override string toString() const pure {
		return format!"if(%s){%s} else {%s}"(condition, onTrue, onFalse);
	}
}

/// resolved while statement node
public class RWhile : RStatement{
public:
	/// condition
	RExpr condition;
	/// loop body
	RStatement body;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["condition"] = condition.jsonOf;
		ret["body"] = body.jsonOf;
		ret["_name"] = "RWhile";
		return ret;
	}

	override string toString() const pure {
		return format!"while(%s){%s}"(condition, body);
	}
}

/// resolved do while statement node
public class RDoWhile : RStatement{
public:
	/// condition
	RExpr condition;
	/// loop body
	RStatement body;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["condition"] = condition.jsonOf;
		ret["body"] = body.jsonOf;
		ret["_name"] = "RDoWhile";
		return ret;
	}

	override string toString() const pure {
		return format!"do{%s}while(%s)"(body, condition);
	}
}

/// resolved switch case statement
public class RSwitch : RStatement{
public:
	/// value to switch on
	RExpr val;
	/// values to match against
	AVal[] vals;
	/// statements to jump to
	RStatement[] stmnts;
	/// default statement, if any
	RStatement def;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.jsonOf;
		if (def)
			ret["default"] = def.jsonOf;
		JSONValue cases;
		foreach (size_t i; 0 .. vals.length)
			cases[vals[i].toString] = stmnts[i].jsonOf;
		ret["cases"] = cases;
		ret["_name"] = "RSwitch";
		return ret;
	}

	override string toString() const pure {
		return format!"switch(%s, default=%s, %s)"(val, def, vals.length.iota.map!(
					i => format!"%s->%s"(vals[i], stmnts[i])
					));
	}
}

/// Resolved Expression
public abstract class RExpr : RStatement{
protected:
	OptVal!ADataType _type;
public:
	/// Returns: true if `this.type` is applicable
	bool hasType() const pure {
		return _type.isVal;
	}

	/// Returns: type for this expression. Only use if `this.hasType == true`
	@property ADataType type() pure {
		return _type.val;
	}
	@property const(ADataType) type() const pure {
		return _type.val;
	}
	/// ditto
	@property ADataType type(ADataType val) pure {
		_type = val.OptVal!ADataType;
		return _type.val;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RExpr";
		if (hasType)
			ret["type"] = type.toString;
		return ret;
	}

	override abstract string toString() const pure;

	/// Returns: new RST which converts this RST into `target` type, or nothing
	/// if cannot be done
	OptVal!RExpr to(ADataType target, IdentU[] ctx = [IdentU.init]){
		if (!this.type.canCastTo(target, ctx))
			return OptVal!RExpr();
		RToExpr r = new RToExpr(this, target);
		r.pos = this.pos;
		return r.OptVal!RExpr;
	}

	/// Returns: true if this can be casted into a target data type
	bool canCastTo(const ADataType target, IdentU[] ctx = [IdentU.init]){
		return this.type.canCastTo(target, ctx);
	}
}

/// No-op
public class RNoOpExpr : RExpr{
private:
	static RNoOpExpr _instance;
	this(){
		type = ADataType();
	}
public:
	/// instance of this
	static @property RNoOpExpr instance(){
		if (_instance is null)
			_instance = new RNoOpExpr;
		return _instance;
	}

	override string toString() const pure {
		return format!"$noop()->%s"(type);
	}
}

/// Resolved Var Get expression
public class RVarExpr : RExpr{
public:
	/// var
	AVar var;

	this(AVar var, bool isConst = false){
		this.var = var;
		this.type = var.type;
		if (isConst)
			this.type = this.type.constOf;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RVarExpr";
		ret["var"] = var.toString;
		return ret;
	}

	override string toString() const pure {
		return var.format!"$varGet(%s)->%s"(type);
	}
}

/// Resolved Var Get expression
public class RVarRefExpr : RExpr{
public:
	/// var
	AVar var;

	this(AVar var, bool isConst = false){
		this.var = var;
		this.type = var.type;
		if (isConst)
			this.type = this.type.constOf;
		this.type = ADataType.ofRef(this.type);
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RVarRefExpr";
		ret["var"] = var.toString;
		return ret;
	}

	override string toString() const pure {
		return var.format!"$varRef(%s)->%s"(type);
	}
}

/// Resolved Block Expression
public class RBlockExpr : RExpr{
public:
	/// block
	RBlock block;

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RBlockExpr";
		ret["block"] = block.jsonOf;
		return ret;
	}

	override string toString() const pure {
		return block.format!"$block(%s)->%s"(type);
	}
}

/// Resolved Assign Expression
public class RAssignExpr : RExpr{
public:
	/// left side. will evaluate to a reference
	RExpr refExpr;
	/// right side. will evaluate to type being referenced
	RExpr valExpr;

	this(){
		this.type = ADataType();
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RAssignExpr";
		ret["refExpr"] = refExpr.jsonOf;
		ret["valExpr"] = refExpr.jsonOf;
		return ret;
	}

	override string toString() const pure {
		return format!"$refAssign(%s, %s)->%s"(refExpr, valExpr, type);
	}
}

/// Resolved Dereference Expression
public class RDerefExpr : RExpr{
public:
	/// value
	RExpr val;

	this(RExpr val){
		this.val = val;
		assert (val.type.type == ADataType.Type.Ref);
		this.type = *val.type.refT;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RDerefExpr";
		ret["val"] = val.jsonOf;
		return ret;
	}

	override string toString() const pure {
		return format!"$deref(%s)->%s"(val, type);
	}
}

/// Resolved Comma expression
public class RCommaExpr : RExpr{
public:
	/// expressions
	RExpr[] exprs;

	this (RExpr[] exprs){
		this.exprs = exprs;
		this.type = ADataType.ofSeq(exprs.map!(e => e.type).array);
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RCommaExpr";
		ret["exprs"] = exprs.map!(a => a.jsonOf).array;
		return ret;
	}

	override string toString() const pure {
		return format!"$comma(%s)->%s"(
				exprs.map!(e => e.toString).join(", "), type);
	}
}

/// Resolved Function Call Expression
public class RFnCallExpr : RExpr{
public:
	/// callee
	RExpr callee;
	/// parameters
	RExpr[] params;

	this (RExpr callee, RExpr[] params){
		this.callee = callee;
		this.params = params;
		ADataType fnType = callee.type;
		assert (fnType.type == ADataType.Type.Fn);
		this.type = *fnType.retT;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RFnCallExpr";
		ret["callee"] = callee.jsonOf;
		ret["params"] = params.map!(a => a.jsonOf).array;
		return ret;
	}

	override string toString() const pure {
		return format!"$call(%s, %s)->%s"(callee,
				params.map!(p => p.toString).join(", "), type);
	}
}

/// Resolved VTable Member Get Expression
public class RVTGetExpr : RExpr{
public:
	/// value
	RExpr val;
	/// member name
	string member;

	this (RExpr val, string member){
		this.val = val;
		this.member = member;
		// TODO: figure out return type for VT member
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RVTGetExpr";
		ret["val"] = val.jsonOf;
		ret["member"] = member;
		return ret;
	}

	override string toString() const pure {
		return format!"$vtGet(%s, %s)->%s"(val, member, type);
	}
}

/// Resolved Struct Member Get Expression (from struct value)
public class RStructMemberGetExpr : RExpr{
public:
	/// struct value
	RExpr val;
	/// member id
	size_t memId;

	this(RExpr val, size_t memId, bool isConst = false){
		this.val = val;
		this.memId = memId;
		assert (val.type.type == ADataType.Type.Struct);
		AStruct* symC = val.type.structS;
		ADataType memT = symC.types[memId];
		if ((isConst || val.type.isConst) && !memT.isConst)
			memT = memT.constOf;
		this.type = ADataType.ofRef(memT);
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RStructMemberGetExpr";
		ret["val"] = val.jsonOf;
		ret["memId"] = memId;
		return ret;
	}

	override string toString() const pure {
		return format!"$structGet(%s, %s)->%s"(val, memId, type);
	}
}

/// Resolved Struct Member Get Expression (from struct ref)
public class RStructRefMemberGetExpr : RExpr{
public:
	/// struct value (ref)
	RExpr val;
	/// member id
	size_t memId;

	this(RExpr val, size_t memId, bool isConst = false){
		this.val = val;
		this.memId = memId;
		assert (val.type.type == ADataType.Type.Ref &&
				val.type.refT !is null &&
				val.type.refT.type == ADataType.Type.Struct);
		AStruct* symC = val.type.refT.structS;
		ADataType memT = symC.types[memId];
		if ((isConst || val.type.refT.isConst) && !memT.isConst)
			memT = memT.constOf;
		this.type = ADataType.ofRef(memT);
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RStructRefMemberGetExpr";
		ret["val"] = val.jsonOf;
		ret["memId"] = memId;
		return ret;
	}

	override string toString() const pure {
		return format!"$structRefGet(%s, %s)->%s"(val, memId, type);
	}
}

/// Resolved Union Member Get Expression (from union val)
public class RUnionMemberGetExpr : RExpr{
public:
	/// value
	RExpr val;
	/// member id
	size_t memId;

	this (RExpr val, size_t memId, bool isConst){
		this.val = val;
		this.memId = memId;
		assert (val.type.type == ADataType.Type.Union);
		AUnion* symC = val.type.unionS;
		ADataType memT = symC.types[memId];
		if ((isConst || val.type.isConst) && !memT.isConst)
			memT = memT.constOf;
		this.type = ADataType.ofRef(memT);
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RUnionMemberGetExpr";
		ret["val"] = val.jsonOf;
		ret["memId"] = memId;
		return ret;
	}

	override string toString() const pure {
		return format!"$unionGet(%s, %s)->%s"(val, memId, type);
	}
}

/// Resolved Union Member Get Expression (from union ref)
public class RUnionRefMemberGetExpr : RExpr{
public:
	/// value
	RExpr val;
	/// member id
	size_t memId;

	this (RExpr val, size_t memId, bool isConst){
		this.val = val;
		this.memId = memId;
		assert (val.type.type == ADataType.Type.Ref &&
				val.type.refT !is null &&
				val.type.refT.type == ADataType.Type.Union);
		AUnion* symC = val.type.unionS;
		ADataType memT = symC.types[memId];
		if ((isConst || val.type.refT.isConst) && !memT.isConst)
			memT = memT.constOf;
		this.type = ADataType.ofRef(memT);
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RUnionRefMemberGetExpr";
		ret["val"] = val.jsonOf;
		ret["memId"] = memId;
		return ret;
	}

	override string toString() const pure {
		return format!"$unionRefGet(%s, %s)->%s"(val, memId, type);
	}
}

/// Resolved Function Expression
public class RFnExpr : RExpr{
public:
	/// function
	AFn* fn;

	this(AFn* fn){
		this.fn = fn;
		this.type = ADataType.ofFn(fn.retT, fn.paramsT);
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RFnExpr";
		ret["fn"] = fn.toString;
		return ret;
	}

	override string toString() const pure {
		return format!"$fn(%s)->%s"(*fn, type);
	}
}

/// Resolved Struct Literal
public class RStructLiteralExpr : RExpr{
public:
	/// member names
	string[] names;
	/// member values
	RExpr[] vals;

	this (string[] names, RExpr[] vals, IdentU[] id){
		this.names = names;
		this.vals = vals;
		AStruct* symC = new AStruct;
		foreach (size_t i, string name; names){
			symC.names[name] = i;
			symC.nameVis[name] = Visibility.Pub;
		}
		symC.types = vals.map!(v => v.type).array;
		symC.vis = Visibility.Pub;
		symC.ident = id;
		assert (symC.isUnique == false);
		this.type = ADataType.of(symC);
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		JSONValue obj;
		foreach (size_t i; 0 .. names.length){
			JSONValue sub;
			sub["name"] = names[i];
			sub["val"] = vals[i].jsonOf;
			obj[i] = sub;
		}
		ret["members"] = obj;
		ret["_name"] = "RStructLiteralExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$structLit(%s)->%s"(names.length.iota.map!(
					i => format!"%s=%s"(names[i], vals[i])),
				type);
	}
}

/// Resolved Array Literal Expression
public class RArrayLiteralExpr : RExpr{
public:
	/// elements
	RExpr[] elements;

	this (RExpr[] elements){
		if (elements.length == 0){
			this.type = ADataType.ofSlice(ADataType());
			return;
		}
		this.elements = elements;
		foreach (RExpr elem; elements[1 .. $]){
			assert (elem.type == elements[0].type);
		}
		this.type = ADataType.ofSlice(elements[0].type.constOf);
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["elements"] = elements.map!(a => a.jsonOf).array;
		ret["_name"] = "RArrayLiteralExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$arrayLit(%r)->%s"(
				elements.map!(e => e.toString).join(", "), type);
	}
}

/// Resolved Literal Value Expression
public class RLiteralExpr : RExpr{
public:
	AVal val; /// the value + type

	this (AVal val){
		this.val = val;
		this.type = val.type;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.toString;
		ret["_name"] = "RLiteralExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$lit(%s)->%s"(val, type);
	}

	override OptVal!RExpr to(ADataType target, IdentU[] ctx = [IdentU.init]){
		OptVal!AVal r = val.to(target, ctx);
		if (!r.isVal)
			return OptVal!RExpr();
		RLiteralExpr ret = new RLiteralExpr(r.val);
		ret.pos = pos;
		return ret.OptVal!RExpr;
	}

	/// Returns: true if this can be casted into a target data type
	override bool canCastTo(const ADataType target,
			IdentU[] ctx = [IdentU.init]){
		return val.canCastTo(target, ctx);
	}
}

/// array length get expressino
public class RArrayLenExpr : RExpr{
public:
	RExpr arr; /// array to get length of

	this (RExpr arr){
		this.arr = arr;
		this.type = ADataType.ofUInt;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["arr"] = arr.jsonOf;
		ret["_name"] = "RArrayLenExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$arrLen(%s)->%s"(arr, type);
	}
}

/// array length set expression
public class RArrayLenSetExpr : RExpr{
public:
	RExpr arr; /// array to set length of. must be reference to the array
	RExpr len; /// new length

	this (RExpr arr, RExpr len){
		this.arr = arr;
		this.len = len;
		this.type = ADataType();
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["arr"] = arr.jsonOf;
		ret["len"] = len.jsonOf;
		ret["_name"] = "RArrayLenSetExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$arrLen(%s, %s)->%s"(arr, len, type);
	}
}

/// array read at index expression
public class RArrayIndexExpr : RExpr{
public:
	RExpr arr; /// array
	RExpr ind; /// index

	this (RExpr arr, RExpr ind){
		this.arr = arr;
		this.ind = ind;
		ADataType t = arr.type;
		if (t.type == ADataType.Type.Ref)
			t = *t.refT;
		assert (t.type == ADataType.Type.Array || t.type == ADataType.Type.Slice);
		t = *t.refT;
		this.type = ADataType.ofRef(t);
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["arr"] = arr.jsonOf;
		ret["ind"] = ind.jsonOf;
		ret["_name"] = "RArrayIndexExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$arrInd(%s, %s)->%s"(arr, ind, type);
	}
}

/// union member check
public class RUnionIsExpr : RExpr{
public:
	RExpr val; /// the union
	size_t memId; /// member id

	this (RExpr val, size_t memId){
		this.val = val;
		this.memId = memId;
		this.type = ADataType.ofBool;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.jsonOf;
		ret["memId"] = memId;
		ret["_name"] = "RUnionIsExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$unionIs(%s, %s)->%s"(val, memId, type);
	}
}

/// get stack trace
public class RStackTraceExpr : RExpr{
public:
	this(){
		this.type = ADataType.ofSlice(ADataType.ofString);
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["_name"] = "RStackTraceExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$stackTrace()->%s"(type);
	}
}

/// rtWrite
public class RTWriteExpr : RExpr{
public:
	RExpr val; /// value to evaluate & print

	this(){
		this.type = ADataType();
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.jsonOf;
		ret["_name"] = "RTWriteExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$rtWrite(%s)->%s"(val, type);
	}
}

/// multiply with -1 (FloatX or IntX or UIntX)
public class RNegExpr : RExpr{
public:
	RExpr val; /// the thing to multiply with -1

	this (RExpr val){
		this.val = val;
		this.type = val.type;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.jsonOf;
		ret["_name"] = "RNegExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$negate(%s)->%s"(val, type);
	}
}

/// bitwise not
public class RBitNotExpr : RExpr{
public:
	RExpr val;

	this (RExpr val){
		this.val = val;
		this.type = val.type;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.jsonOf;
		ret["_name"] = "RBitNotExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$bitNot(%s)->%s"(val, type);
	}
}

/// bitwise and
public class RBitAndExpr : RExpr{
public:
	RExpr valA;
	RExpr valB;

	this (RExpr valA, RExpr valB){
		this.valA = valA;
		this.valB = valB;
		assert (valA.type == valB.type);
		this.type = valA.type;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["valA"] = valA.jsonOf;
		ret["valB"] = valB.jsonOf;
		ret["_name"] = "RBitAndExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$bitAnd(%s, %s)->%s"(valA, valB, type);
	}
}

/// bitwise or
public class RBitOrExpr : RExpr{
public:
	RExpr valA;
	RExpr valB;

	this (RExpr valA, RExpr valB){
		this.valA = valA;
		this.valB = valB;
		assert (valA.type == valB.type);
		this.type = valA.type;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["valA"] = valA.jsonOf;
		ret["valB"] = valB.jsonOf;
		ret["_name"] = "RBitOrExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$bitOr(%s, %s)->%s"(valA, valB, type);
	}
}

/// bitwise xor
public class RBitXorExpr : RExpr{
public:
	RExpr valA;
	RExpr valB;

	this (RExpr valA, RExpr valB){
		this.valA = valA;
		this.valB = valB;
		assert (valA.type == valB.type);
		this.type = valA.type;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["valA"] = valA.jsonOf;
		ret["valB"] = valB.jsonOf;
		ret["_name"] = "RBitXorExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$bitXor(%s, %s)->%s"(valA, valB, type);
	}
}

/// boolean and
public class RAndExpr : RExpr{
public:
	RExpr lhs;
	RExpr rhs;

	this (RExpr lhs, RExpr rhs){
		this.lhs = lhs;
		this.rhs = rhs;
		assert (lhs.type == ADataType.ofBool);
		assert (rhs.type == ADataType.ofBool);
		this.type = ADataType.ofBool;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["lhs"] = lhs.jsonOf;
		ret["rhs"] = rhs.jsonOf;
		ret["_name"] = "RAndExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$and(%s, %s)->%s"(lhs, rhs, type);
	}
}

/// boolean or
public class ROrExpr : RExpr{
public:
	RExpr lhs;
	RExpr rhs;

	this (RExpr lhs, RExpr rhs){
		this.lhs = lhs;
		this.rhs = rhs;
		assert (lhs.type == ADataType.ofBool);
		assert (rhs.type == ADataType.ofBool);
		this.type = ADataType.ofBool;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["lhs"] = lhs.jsonOf;
		ret["rhs"] = rhs.jsonOf;
		ret["_name"] = "ROrExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$or(%s, %s)->%s"(lhs, rhs, type);
	}
}

/// addition
public class RAddExpr : RExpr{
public:
	RExpr lhs;
	RExpr rhs;

	this (RExpr lhs, RExpr rhs){
		this.lhs = lhs;
		this.rhs = rhs;
		assert (lhs.type == rhs.type);
		this.type = lhs.type;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["lhs"] = lhs.jsonOf;
		ret["rhs"] = rhs.jsonOf;
		ret["_name"] = "RAddExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$add(%s, %s)->%s"(lhs, rhs, type);
	}
}

/// subtraction
public class RSubExpr : RExpr{
public:
	RExpr lhs;
	RExpr rhs;

	this (RExpr lhs, RExpr rhs){
		this.lhs = lhs;
		this.rhs = rhs;
		assert (lhs.type == rhs.type);
		this.type = lhs.type;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["lhs"] = lhs.jsonOf;
		ret["rhs"] = rhs.jsonOf;
		ret["_name"] = "RSubExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$sub(%s, %s)->%s"(lhs, rhs, type);
	}
}

/// multiplication
public class RMulExpr : RExpr{
public:
	RExpr lhs;
	RExpr rhs;

	this (RExpr lhs, RExpr rhs){
		this.lhs = lhs;
		this.rhs = rhs;
		assert (lhs.type == rhs.type);
		this.type = lhs.type;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["lhs"] = lhs.jsonOf;
		ret["rhs"] = rhs.jsonOf;
		ret["_name"] = "RMulExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$mul(%s, %s)->%s"(lhs, rhs, type);
	}
}

/// division
public class RDivExpr : RExpr{
public:
	RExpr lhs;
	RExpr rhs;

	this (RExpr lhs, RExpr rhs){
		this.lhs = lhs;
		this.rhs = rhs;
		assert (lhs.type == rhs.type);
		this.type = lhs.type;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["lhs"] = lhs.jsonOf;
		ret["rhs"] = rhs.jsonOf;
		ret["_name"] = "RDivExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$div(%s, %s)->%s"(lhs, rhs, type);
	}
}

/// modulus
public class RModExpr : RExpr{
public:
	RExpr lhs;
	RExpr rhs;

	this (RExpr lhs, RExpr rhs){
		this.lhs = lhs;
		this.rhs = rhs;
		assert (lhs.type == rhs.type);
		this.type = lhs.type;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["lhs"] = lhs.jsonOf;
		ret["rhs"] = rhs.jsonOf;
		ret["_name"] = "RModExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$mod(%s, %s)->%s"(lhs, rhs, type);
	}
}

/// left shift
public class RShiftLExpr : RExpr{
public:
	RExpr val;
	RExpr by;

	this (RExpr val, RExpr by){
		this.val = val;
		this.by = by;
		this.type = val.type;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.jsonOf;
		ret["by"] = by.jsonOf;
		ret["_name"] = "RShiftLExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$shiftL(%s, %s)->%s"(val, by, type);
	}
}

/// left shift
public class RShiftRExpr : RExpr{
public:
	RExpr val;
	RExpr by;

	this (RExpr val, RExpr by){
		this.val = val;
		this.by = by;
		this.type = val.type;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.jsonOf;
		ret["by"] = by.jsonOf;
		ret["_name"] = "RShiftRExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$shiftR(%s, %s)->%s"(val, by, type);
	}
}

/// `a is b` comparison
public class RCmpIsExpr : RExpr{
public:
	RExpr valA;
	RExpr valB;

	this (RExpr valA, RExpr valB){
		this.valA = valA;
		this.valB = valB;
		assert (valA.sizeof == valB.sizeof);
		this.type = ADataType.ofBool;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["valA"] = valA.jsonOf;
		ret["valB"] = valB.jsonOf;
		ret["_name"] = "RCmpIsExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$is(%s, %s)->%s"(valA, valB, type);
	}
}

/// `a !is b` comparison
public class RCmpNotIsExpr : RExpr{
public:
	RExpr valA;
	RExpr valB;

	this (RExpr valA, RExpr valB){
		this.valA = valA;
		this.valB = valB;
		assert (valA.sizeof == valB.sizeof);
		this.type = ADataType.ofBool;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["valA"] = valA.jsonOf;
		ret["valB"] = valB.jsonOf;
		ret["_name"] = "RCmpNotIsExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$isNot(%s, %s)->%s"(valA, valB, type);
	}
}

/// `a < b` comparison
public class RCmpLessExpr : RExpr{
public:
	RExpr rhs;
	RExpr lhs;

	this (RExpr lhs, RExpr rhs){
		this.lhs = lhs;
		this.rhs = rhs;
		assert (lhs.type == rhs.type);
		this.type = ADataType.ofBool;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["rhs"] = rhs.jsonOf;
		ret["lhs"] = lhs.jsonOf;
		ret["_name"] = "RCmpLessExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$isLess(%s, %s)->%s"(lhs, rhs, type);
	}
}

/// boolean not
public class RNotExpr : RExpr{
public:
	RExpr val;

	this (RExpr val){
		this.val = val;
		this.type = ADataType.ofBool;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.jsonOf;
		ret["_name"] = "RNotExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$not(%s)->%s"(val, type);
	}
}

/// cast between primitives
public class RToExpr : RExpr{
public:
	RExpr val;
	ADataType target;

	this (RExpr val, ADataType target){
		this.val = val;
		this.target = target;
		this.type = target;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["val"] = val.jsonOf;
		ret["target"] = target.toString;
		ret["_name"] = "RToExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$to(%s, %s)->%s"(val, target, type);
	}
}

/// Error short circuit operator
public class RUnwrapExpr : RExpr{
public:
	/// LHS expression to unwrap
	RExpr lhs;
	/// `_.isVal`
	RExpr isVal;
	/// `_.val`
	RExpr val;

	this (RExpr lhs, RExpr isVal, RExpr val){
		this.lhs = lhs;
		this.isVal = isVal;
		this.val = val;
		assert (isVal.type == ADataType.ofBool);
		this.type = val.type;
	}

	override JSONValue jsonOf() const pure {
		JSONValue ret = super.jsonOf;
		ret["lhs"] = lhs.jsonOf;
		ret["isVal"] = isVal.jsonOf;
		ret["val"] = val.jsonOf;
		ret["_name"] = "RUnwrapExpr";
		return ret;
	}

	override string toString() const pure {
		return format!"$errq(%s, %s, %s)->%s"(lhs, isVal, val, type);
	}
}
