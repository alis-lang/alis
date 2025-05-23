/++
Alis Common Data Types
+/
module alis.common;

import alis.utils;

import std.string,
			 std.traits,
			 std.range,
			 std.conv,
			 std.format,
			 std.typecons,
			 std.algorithm,
			 std.digest.crc;

debug import std.stdio;

public import alis.compiler.common : Visibility; // TODO move it here

/// an Alis CompileTime Value
public struct AValCT{
	/// possible types
	enum Type{
		Literal, /// some Literal value
		Symbol, /// an Alias to a symbol
		Type, /// a Data Type
	}
	/// currently stored type
	Type type = Type.Type;
	union{
		struct{
			ubyte[] dataL; /// data for `Literal`
			ADataType typeL; /// data type for `Literal`
		}
		ASymRef symS; /// symbol ref for `Symbol`
		ADataType typeT; /// data type for `Type`
	}

	string toString() const pure {
		final switch (type){
			case Type.Literal:
				return typeL.decodeStr(dataL);
			case Type.Symbol:
				return symS.toString;
			case Type.Type:
				return typeT.toString;
		}
		return null;
	}

	bool opEquals()(const auto ref AValCT rhs) const pure {
		final switch (type){
			case Type.Literal:
				return typeL == rhs.typeL && dataL == rhs.dataL;
			case Type.Symbol:
				return symS == rhs.symS;
			case Type.Type:
				return typeT == rhs.typeT;
		}
		assert(false);
	}

	size_t toHash() const {
		final switch (type){
			case Type.Literal:
				return tuple(Type.Literal, dataL, typeL).toHash;
			case Type.Symbol:
				return tuple(Type.Symbol, symS).toHash;
			case Type.Type:
				return tuple(Type.Type, typeT).toHash;
		}
		assert(false);
	}

	/// constructor
	this (ADataType type, ubyte[] data){
		this.type = Type.Literal;
		this.dataL = data;
		this.typeL = type;
	}
	/// ditto
	this (ASymbol sym){
		this.type = Type.Symbol;
		this.symS = sym.ident.ASymRef;
	}
	/// ditto
	this (ASymRef sym){
		this.type = Type.Symbol;
		this.symS = sym;
	}
	/// ditto
	this (ADataType type){
		this.type = Type.Type;
		this.typeT = type;
	}
}

/// identifier node unit
public struct IdentU{
public:
	/// identifier
	string ident = "_";
	/// parameters, if any
	AValCT[] params;
	/// constructor
	this (string ident, AValCT[] params = null) pure {
		this.ident = ident;
		this.params = params.dup;
	}
	string toString() const pure {
		if (params)
			return format!"%s(%s)"(ident,
					params.map!(p => p.toString).join(","));
		return ident;
	}
	//bool opEquals()(auto ref const IdentU rhs) const pure {
	bool opEquals()(auto ref const IdentU rhs) const pure {
		if (ident != rhs.ident || params.length != rhs.params.length)
			return false;
		foreach (i, param; params){
			if (param != rhs.params[i])
				return false;
		}
		return true;
	}
	size_t toHash() const {
		return tuple(ident, params).toHash;
	}
}

/// Whether an IdentU[] is to [IdentU.init]
public bool isNoId()(auto ref const IdentU[] ident){
	return ident.length == 1 && ident[0] == IdentU.init;
}

/// complete identifier
/*public final class Ident{
public:
	/// identifier
	IdentU ident;
	/// previous, if any, otherwise `null`
	Ident prev;
	/// constructor
	this (string ident, AValCT[] params, Ident prev = null){
		this.ident = IdentU(ident, params);
		this.prev = prev;
	}
	/// ditto
	this (string ident, Ident prev = null){
		this.ident = ident.IdentU;
		this.prev = prev;
	}
	/// ditto
	this (IdentU ident, Ident prev = null){
		this.ident = ident;
		this.prev = prev;
	}
	/// Returns: string representation
	override string toString() const pure {
		if (prev)
			return format!"%s.%s"(prev.toString, ident.toString);
		return ident.toString;
	}
	bool opEquals()(const Ident rhs) const pure {
		return this is rhs || toString == rhs.toString;
	}

	/// Returns: IdentU[] representation
	IdentU[] array() pure {
		// count length
		size_t len;
		Ident c = this;
		while (c !is null)
			len++, c = c.prev;
		IdentU[] ret = new IdentU[len];
		c = this;
		while (len)
			ret[--len] = c.ident, c = c.prev;
		return ret;
	}
}*/
///
/*unittest{
	Ident id = new Ident("foo".IdentU, new Ident("main".IdentU));
	assert(id.array.map!(e => e.toString).array == ["main", "foo"]);
}*/

/// a reference to a symbol (use STab to lookup)
public struct ASymRef{
public:
	/// identifier for symbol
	IdentU[] ident;
	alias ident this;
	string toString() const pure {
		return ident.to!string;
	}
}

/// a symbol
public struct ASymbol{
	/// Returns: identifier, `_` if anonymous
	@property inout(IdentU[]) ident() pure inout {
		final switch (type){
			case Type.Struct:
				return structS.ident;
			case Type.Union:
				return unionS.ident;
			case Type.Enum:
				return enumS.ident;
			case Type.EnumMember:
				return enumMember.ident;
			case Type.EnumConst:
				return enumCS.ident;
			case Type.Fn:
				return fnS.ident;
			case Type.Var:
				return varS.ident;
			case Type.Alias:
				return aliasS.ident;
			case Type.Import:
				return importS.ident;
			case Type.Template:
				return templateS.ident;
		}
		assert(false);
	}

	/// Returns: Visibility, or `Visibility.Pub` if not applicable
	@property Visibility vis() pure {
		final switch (type){
			case Type.Struct:
				return structS.vis;
			case Type.Union:
				return unionS.vis;
			case Type.Enum:
				return enumS.vis;
			case Type.EnumMember:
				return Visibility.Pub;
			case Type.EnumConst:
				return enumCS.vis;
			case Type.Fn:
				return fnS.vis;
			case Type.Var:
				return varS.vis;
			case Type.Alias:
				return aliasS.vis;
			case Type.Import:
				return importS.vis;
			case Type.Template:
				return templateS.vis;
		}
		assert(false);
	}

	/// possible Symbol types
	enum Type{
		Struct,
		Union,
		Enum,
		EnumMember,
		EnumConst,
		Fn,
		Var,
		Alias,
		Import,
		Template,
	}
	/// type of this symbol
	Type type;
	union{
		AStruct structS; /// struct for `Type.Struct`
		AUnion unionS; /// union for `Type.Union`
		struct{
			/// enum for `Type.Enum`, or `Type.EnumMember`
			AEnum enumS;
			/// enum member name for `Type.EnumMember`
			AEnumMember enumMember;
		}
		AEnumConst enumCS; /// enum for `Type.EnumConst`
		AFn fnS; /// function for `Type.Fn`
		AVar varS; /// variable for `Type.Var`
		AAlias aliasS; /// alias for `Type.Alias`
		AImport importS; /// import for `Type.Import`
		ATemplate templateS;
	}
	/// Returns: string representation, equivalent to `ASymbol.ident.toString`
	string toString() const pure {
		final switch (type){
			case Type.Struct:
				return structS.toString;
			case Type.Union:
				return unionS.toString;
			case Type.Enum:
				return enumS.toString;
			case Type.EnumMember:
				return enumMember.toString;
			case Type.EnumConst:
				return enumCS.toString;
			case Type.Fn:
				return fnS.toString;
			case Type.Var:
				return varS.toString;
			case Type.Alias:
				return aliasS.toString;
			case Type.Import:
				return importS.toString;
			case Type.Template:
				return templateS.toString;
		}
		assert(false);
	}

	bool opEquals()(auto ref const AStruct structS) const {
		return this.type == Type.Struct && this.structS == structS;
	}
	bool opEquals()(auto ref const AUnion unionS) const {
		return this.type == Type.Union && this.unionS == unionS;
	}
	bool opEquals()(auto ref const AEnum enumS) const {
		return (this.type == Type.Enum || this.type == Type.EnumMember) &&
			this.enumS == enumS;
	}
	bool opEquals()(auto ref const AEnumMember enumS) const {
		return (this.type == Type.Enum || this.type == Type.EnumMember) &&
			this.enumS == enumS;
	}
	bool opEquals()(auto ref const AEnumConst enumCS) const {
		return this.type == Type.EnumConst && this.enumCS == enumCS;
	}
	bool opEquals()(auto ref const AFn fnS) const {
		return this.type == Type.Fn && this.fnS == fnS;
	}
	bool opEquals()(auto ref const AVar varS) const {
		return this.type == Type.Var && this.varS == varS;
	}
	bool opEquals()(auto ref const AAlias aliasS) const {
		return this.type == Type.Alias && this.aliasS == aliasS;
	}
	bool opEquals()(auto ref const AImport importS) const {
		return this.type == Type.Import && this.importS == importS;
	}
	bool opEquals()(auto ref const ATemplate templateS) const {
		return this.type == Type.Template && this.templateS == templateS;
	}

	bool opEquals()(auto ref const ASymbol rhs) const {
		if (rhs.type != this.type || this.ident != ident)
			return false;
		final switch (type){
			case Type.Struct:
				return rhs == this.structS;
			case Type.Union:
				return rhs == this.unionS;
			case Type.Enum:
				return rhs == this.enumS;
			case Type.EnumMember:
				return rhs == this.enumS && rhs.enumMember == this.enumMember;
			case Type.EnumConst:
				return rhs == this.enumCS;
			case Type.Fn:
				return rhs == this.fnS;
			case Type.Var:
				return rhs == this.varS;
			case Type.Alias:
				return rhs == this.aliasS;
			case Type.Import:
				return rhs == this.importS;
			case Type.Template:
				return this == this.templateS;
		}
		assert (false);
	}

	/// constructor
	this (AStruct structS){
		this.type = Type.Struct;
		this.structS = structS;
	}
	/// ditto
	this (AUnion unionS){
		this.type = Type.Union;
		this.unionS = unionS;
	}
	/// ditto
	this (AEnum enumS){
		this.type = Type.Enum;
		this.enumS = enumS;
	}
	/// ditto
	this (AEnumConst enumCS){
		this.type = Type.EnumConst;
		this.enumCS = enumCS;
	}
	/// ditto
	this (AEnumMember enumMember){
		this.type = Type.EnumMember;
		this.enumMember = enumMember;
	}
	/// ditto
	this (AFn fnS){
		this.type = Type.Fn;
		this.fnS = fnS;
	}
	/// ditto
	this (AVar varS){
		this.type = Type.Var;
		this.varS = varS;
	}
	/// ditto
	this (AAlias aliasS){
		this.type = Type.Alias;
		this.aliasS = aliasS;
	}
	/// ditto
	this (AImport importS){
		this.type = Type.Import;
		this.importS = importS;
	}
	/// ditto
	this (ATemplate templateS){
		this.type = Type.Template;
		this.templateS = templateS;
	}
}

/// an Alis Module. Aggregates all public symbols for a module
public struct AModule{
	/// globals
	ADT globals;
	/// symbols (globals will be repeated in this)
	ASymbol[IdentU] st;
}

/// Alis Data Type.
public struct ADataType{
	/// possible Data Types
	enum Type{
		Seq, /// a sequence of types
		IntX, /// an integer of X bits
		UIntX, /// an unsigned integer of X bits
		FloatX, /// a floating point number of X bits
		CharX, /// a character of X bits
		Bool, /// a boolean
		Slice, /// a slice
		Array, /// an array
		Ref, /// a reference
		Fn, /// a function
		Struct, /// a struct
		Union, /// a union
		Enum, /// an enum
		NoInit, /// `$noinit`
	}
	/// whether it is a const
	bool isConst = false;
	/// type
	Type type = Type.Struct;
	union{
		/// X-bits for `IntX`, `UIntX`, `FloatX`, or `CharX`
		ubyte x;
		struct{
			/// type being referenced, for `Ref`, `Slice`, or `Array`
			ADataType* refT;
			/// size in bytes, if the array is on stack, for `Slice`, or `Array`
			/// if not on stack, will be 0
			size_t sizeOnStack;
		}
		/// type sequence, for `Seq`
		ADataType[] seqT;
		/// symbol reference, for `Struct`, `Union`, or `Enum`
		ASymRef sym;
		struct{
			/// return type, for `Fn`
			ADataType* retT;
			/// parameter types, for `Fn`
			ADataType[] paramT;
		}
	}

	/// Returns: whether this is a primitive type
	@property isPrimitive() const {
		switch (type){
			case Type.IntX, Type.UIntX, Type.FloatX, Type.CharX, Type.Bool:
				return true;
			case Type.Slice:
				return this == ADataType.ofString;
			default:
				return false;
		}
	}

	string toString() const pure {
		string ret = isConst ? "const " : null;
		final switch (type){
			case Type.Seq:
				return ret ~ "(" ~ seqT.map!(t => t.toString).join(",") ~ ")";
			case Type.IntX:
				return ret ~ x.format!"$int(%d)";
			case Type.UIntX:
				return ret ~ x.format!"$uint(%d)";
			case Type.FloatX:
				return ret ~ x.format!"$float(%d)";
			case Type.CharX:
				return ret ~ x.format!"$char(%d)";
			case Type.Bool:
				return ret ~ "bool";
			case Type.Slice:
				return (*refT).toString.format!"$slice(%s)"; // cannot be const
			case Type.Array:
				return (*refT).toString.format!"$array(%s)"; // cannot be const
			case Type.Fn:
				return format!"fn (%s) -> %s"(
						paramT.map!(p => p.toString).join(","), retT.toString);
			case Type.Ref:
				return ret ~ (*refT).toString.format!"@%s";
			case Type.Struct:
				return sym.toString.format!"struct(%s)";
			case Type.Union:
				return sym.toString.format!"union(%s)";
			case Type.Enum:
				return sym.toString.format!"enum(%s)";
			case Type.NoInit:
				return "$noinit";
		}
		assert(false);
	}

	/// Returns: byte size of type
	@property size_t sizeOf() const pure {
		final switch (type){
			case Type.Seq:
				return seqT.fold!((size_t a, const ADataType e) => a + e.sizeOf)(size_t.init);
			case Type.IntX, Type.UIntX, Type.FloatX, Type.CharX:
				return x;
			case Type.Bool:
				return 1;
			case Type.Slice:
				if (sizeOnStack)
					return sizeOnStack;
				return 2 * null.sizeof; // ptr + length
			case Type.Array:
				if (sizeOnStack)
					return sizeOnStack;
				return 3 * null.sizeof; // ptr + length + capacity
			case Type.Fn:
				return 2 * null.sizeof; // ptr + closurePtr
			case Type.Ref:
				return null.sizeof;
			case Type.Struct:
				debug stderr.writeln("STUB: ADataType.sizeOf called on Struct.");
				return 0;
				//assert (false, "thou shall not call ADataType.sizeOf on Struct!");
			case Type.Union:
				debug stderr.writeln("STUB: ADataType.sizeOf called on Union.");
				return 0;
				//assert (false, "thou shall not call ADataType.sizeOf on Union!");
			case Type.Enum:
				debug stderr.writeln("STUB: ADataType.sizeOf called on Enum.");
				return size_t.sizeof;
				//assert (false, "thou shall not call ADataType.sizeOf on Enum!");
			case Type.NoInit:
				return 0;
		}
	}

	/// Decodes a byte array as per this data type into string representation
	/// Returns: string representation
	string decodeStr(const ubyte[] data) const pure {
		// TODO: implement ADataType.decodeStr
		return format!"{type: %s, data: %s}"(this.toString, data);
	}

	/// Returns: `$noinit` data type
	static ADataType ofNoInit() pure {
		ADataType ret;
		ret.type = ADataType.Type.NoInit;
		return ret;
	}

	/// Returns: Sequence data type
	static ADataType ofSeq(ADataType[] seq) pure {
		ADataType ret;
		ret.type = Type.Seq;
		ret.seqT = seq;
		return ret;
	}

	/// Returns: Integer of X bits type
	static ADataType ofInt(ubyte x = size_t.sizeof * 8) pure {
		ADataType ret;
		ret.type = Type.IntX;
		ret.x = x;
		return ret;
	}

	/// Returns: Unsigned Integer of X bits type
	static ADataType ofUInt(ubyte x = ptrdiff_t.sizeof * 8) pure {
		ADataType ret;
		ret.type = Type.UIntX;
		ret.x = x;
		return ret;
	}

	/// Returns: Float of X bits type
	static ADataType ofFloat(ubyte x = double.sizeof * 8) pure {
		ADataType ret;
		ret.type = Type.FloatX;
		ret.x = x;
		return ret;
	}

	/// Returns: Char of X bits type
	static ADataType ofChar(ubyte x = dchar.sizeof * 8) pure {
		ADataType ret;
		ret.type = Type.CharX;
		ret.x = x;
		return ret;
	}

	/// Returns: Bool type
	static ADataType ofBool() pure {
		ADataType ret;
		ret.type = Type.Bool;
		return ret;
	}

	/// Returns: slice type
	static ADataType ofSlice(ADataType elemT) pure {
		ADataType ret;
		ret.type = Type.Slice;
		ret.refT = [elemT].ptr;
		ret.sizeOnStack = 0;
		return ret;
	}

	/// Returns: array type
	static ADataType ofArray(ADataType elemT) pure {
		ADataType ret;
		ret.type = Type.Array;
		ret.refT = [elemT].ptr;
		ret.sizeOnStack = 0;
		return ret;
	}

	static ADataType ofString(size_t sizeOnStack = 0) pure {
		ADataType str;
		str.type = Type.Slice;
		str.refT = [ADataType.ofChar(1)].ptr;
		str.refT.isConst = true;
		str.sizeOnStack = sizeOnStack;
		return str; // $slice(const char)
	}

	/// Returns: function type
	static ADataType ofFn(ADataType retT, ADataType[] paramT) pure {
		ADataType ret;
		ret.type = Type.Fn;
		ret.retT = [retT].ptr;
		ret.paramT = paramT;
		return ret;
	}

	/// Returns: reference type
	static ADataType ofRef(ADataType refT) pure {
		ADataType ret;
		ret.type = Type.Ref;
		ret.refT = [refT].ptr;
		return ret;
	}

	/// Returns: struct type
	static ADataType of()(auto ref const AStruct structT) pure {
		ADataType ret;
		ret.type = Type.Struct;
		ret.sym = structT.ident.ASymRef;
		return ret;
	}
	/// ditto
	static ADataType ofStruct(ASymRef symR) pure {
		ADataType ret;
		ret.type = Type.Struct;
		ret.sym = symR;
		return ret;
	}

	/// Returns: union type
	static ADataType of()(auto ref const AUnion unionT) pure {
		ADataType ret;
		ret.type = Type.Union;
		ret.sym = unionT.ident.ASymRef;
		return ret;
	}
	/// ditto
	static ADataType ofUnion(ASymRef symR) pure {
		ADataType ret;
		ret.type = Type.Union;
		ret.sym = symR;
		return ret;
	}

	/// Returns: enum type
	static ADataType of()(auto ref const AEnum enumT) pure {
		ADataType ret;
		ret.type = Type.Enum;
		ret.sym = enumT.ident.ASymRef;
		return ret;
	}
	/// ditto
	static ADataType ofEnum(ASymRef symR) pure {
		ADataType ret;
		ret.type = Type.Enum;
		ret.sym = symR;
		return ret;
	}

	/// Returns: ADataType against a D type
	static ADataType of(T)() pure {
		static if (is (T == string) || is (T == const char[])){
			return ofString;
		} else
		static if (is (T == char[])){
			return ofArray(ADataType.ofChar(1));
		} else
		static if (isUnsigned!T) {
			return ofUInt(T.sizeof * 8);
		} else
		static if (isSigned!T) {
			return ofInt(T.sizeof * 8);
		} else
		static if (isFloatingPoint!T){
			return ofFloat(T.sizeof * 8);
		} else
		static if (isSomeChar!T){
			return ofChar(T.sizeof * 8);
		} else
		static if (is (T == bool)){
			return ofBool;
		} else
		static if (isFunction!T || isFunctionPointer!T){
			return ofFn(ReturnType!T, [staticMap!(of, Parameters!T)]);
		} else
		static if (is (T == struct)){
			static assert (false, "creating ADataType of D struct not implemented");
			// TODO: convert D struct to AStruct
		} else
		static if (is (T == union)){
			static assert (false, "creating ADataType of D union not implemented");
			// TODO: convert D union to AUnion
		} else
		static if (is (T == enum)){
			static assert (false, "creating ADataType of D enum not implemented");
			// TODO: convert D enum to AEnum or AEnumConst
		}
		return ADataType;
	}

	bool opEquals()(auto ref const ADataType rhs) const pure {
		if (type != rhs.type)
			return false;
		final switch (type){
			case Type.Seq:
				if (seqT.length != rhs.seqT.length)
					return false;
				foreach (size_t i; seqT.length.iota){
					if (seqT[i] != rhs.seqT[i])
						return false;
				}
				return true;
			case Type.IntX:
			case Type.UIntX:
			case Type.FloatX:
			case Type.CharX:
				return x == rhs.x;
			case Type.Bool:
				return true;
			case Type.Slice:
			case Type.Array:
			case Type.Ref:
				return refT == rhs.refT;
			case Type.Fn:
				if (retT != rhs.retT || paramT.length != rhs.paramT.length)
					return false;
				foreach (size_t i; paramT.length.iota){
					if (paramT[i] != rhs.paramT[i])
						return false;
				}
				return true;
			case Type.Struct:
				return sym == rhs.sym;
			case Type.Union:
				return sym == rhs.sym;
			case Type.Enum:
				return sym == rhs.sym;
			case Type.NoInit:
				return true;
		}
		return true;
	}
}

/// Alis data table (structure behind virtual tables & closures etc)
public struct ADT{
	/// types of fields
	ADataType[] types;
	/// byte offsets for fields
	size_t[] offsets;
	/// names (can be null) for fields
	string[] names;
	/// table itself
	ubyte[] tb;
	/// Returns: size of virtual table
	pragma(inline, true) @property size_t sizeOf() const pure {
		return tb.length;
	}

	string toString() const pure {
		string ret = "ADT{";
		size_t off;
		foreach (size_t i; 0 .. types.length){
			ret ~= format!"%d-%d{name:%s,type:%s,tb:%s}"(
					off, off + types[i].sizeOf, names[i], types[i].toString,
					tb[off .. off + types[i].sizeOf]);
			off += types[i].sizeOf;
		}
		ret ~= "}";
		return ret;
	}
}

/// Alis struct
public struct AStruct{
	/// identifier, `_` if anonymous, which also implies not unique
	IdentU[] ident;
	/// if this is unique
	@property bool isUnique() const pure {
		return ident.length &&
			ident[$ - 1].ident == "_" && !ident[$ - 1].params.length;
	}
	/// structure
	ADT dt;
	/// Virtual Table, if any
	ADT vt;
	/// whether this has an `alias this = X`. the member being aliased to `this`
	/// will be at index 0 in `types` and `offsets`
	bool hasBase = false;
	/// Visibility outside its parent module
	Visibility vis;
	/// Returns: size of this struct
	@property size_t sizeOf() const pure {
		return dt.sizeOf + (vt.tb.length > 0);
	}

	string toString() const pure {
		return format!"struct %s{dt:%s,vt:%s}"(ident, dt, vt);
	}
}

/// Alis union
public struct AUnion{
	/// identifier, `_` if anonymous, which also implies not unique
	IdentU[] ident;
	/// if this is unique
	@property bool isUnique() const pure {
		return ident.length &&
			ident[$ - 1].ident == "_" && !ident[$ - 1].params.length;
	}
	/// types of members
	ADataType[] types;
	/// member names
	string[] names;
	/// default type index
	size_t defInd;
	/// initialized value
	ubyte[] dt;
	/// whether this has an `alias this = X`. the member being aliased to `this`
	/// will be at index 0 in `types` and `offsets`
	bool hasBase = false;
	/// Visibility outside its parent module
	Visibility vis;
	/// Returns: true if this is an unnamed union
	@property bool isUnnamed() const pure {
		return names.length == 0;
	}
	/// Returns: size of this union
	@property size_t sizeOf() const pure {
		return dt.length + size_t.sizeof;
	}

	string toString() const pure {
		string ret = format!"union %s{"(ident);
		foreach (size_t i, ref const ADataType type; types){
			ret ~= format!"%s%s%s,"(names.length ? (names[i] ~ " ") : null, type,
					defInd == i ? " default" : null);
		}
		return ret[0 .. $ - 1] ~ "}";
	}
}

/// Alis Enum
public struct AEnum{
	/// identifier
	IdentU[] ident;
	/// Data Type. This will be `struct{}` in case of empty emum
	ADataType type;
	/// members
	AEnumMember[] members;
	/// Visibility outside its parent module
	Visibility vis;

	string toString() const pure {
		string ret = format!"enum %s:%s{"(ident, type);
		foreach (size_t i, const AEnumMember member; members)
			ret ~= format!"%d:%s,"(i, member);
		ret ~= "}";
		return ret;
	}
}

/// Alis Enum Members
public struct AEnumMember{
	/// identifier
	IdentU[] ident;
	/// value
	ubyte[] val;
	string toString() const pure {
		return format!"enumMember %s=%s"(ident, val);
	}
}

/// Alis Enum Constant
public struct AEnumConst{
	/// identifier
	IdentU[] ident;
	/// type
	ADataType type;
	/// value bytes
	ubyte[] data;
	/// Visibility outside its parent module
	Visibility vis;

	string toString() const pure {
		return format!"enum %s:%s=%s;"(ident, type, data);
	}
}

/// Alis Function Information
public struct AFn{
	/// identifier, `_` if anonymous
	IdentU[] ident;
	/// return type
	ADataType retT;
	/// locals, including parameters
	ADT params;
	/// label name in ABC
	string labN;
	/// whether this is an alis function (true) or an external (false)
	bool isAlisFn;
	/// Visibility outside its parent module
	Visibility vis;

	string toString() const pure {
		return format!
			"fn %s%s->%s params={\n%s\n}"(
					(isAlisFn ? "" : "external "),
					ident, retT, params);
	}
}

/// Alis Variable
public struct AVar{
	/// identifier
	IdentU[] ident;
	/// data type
	ADataType type;
	/// offset
	size_t offset;
	/// whether is global or local
	bool isGlobal;
	/// Visibility outside its parent module
	Visibility vis;

	string toString() const pure {
		return format!"var %s%s:%s off=%d"(isGlobal ? "global" : null, ident,
				type, offset);
	}
}

/// Alis Alias
public struct AAlias{
	/// identifier
	IdentU[] ident;
	/// Visibility outside its parent module
	Visibility vis;
	/// TODO: store resolved expression

	string toString() const pure {
		return format!"alias";
	}
}

/// Alis Import
public struct AImport{
	/// module being imported
	string[] modIdent;
	/// aliased name, if any
	IdentU[] ident;
	/// Visibility outside its parent module
	Visibility vis;

	string toString() const pure {
		return format!"import %s as %s"(modIdent, ident);
	}
}

/// Alis Template
public struct ATemplate{
	/// identifier
	IdentU[] ident;
	/// Visibility outside its parent module
	Visibility vis;
	// TODO: what to store in ATemplate

	string toString() const pure {
		return format!"template";
	}
}

/// Encodes function name, using function name and param types
/// Returns: encoded name
public string fnNameEncode(string name, ADataType[] args){
	return format!"%s$%(_%s%)_$"(name, args.map!(a => a.toString));
}
