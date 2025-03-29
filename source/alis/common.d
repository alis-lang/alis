/++
Alis Common Data Types
+/
module alis.common;

import alis.utils;

import std.string,
			 std.traits,
			 std.range,
			 std.format,
			 std.typecons,
			 std.algorithm,
			 std.digest.crc;

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
		ASymbol symS; /// symbol for `Symbol`
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

	bool opEquals()(const auto ref AValCT rhs) const {
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

	/// constructor
	this (ADataType type, ubyte[] data){
		this.type = Type.Literal;
		this.dataL = data;
		this.typeL = type;
	}
	/// ditto
	this (ASymbol sym){
		this.type = Type.Symbol;
		this.symS = sym;
	}
	/// ditto
	this (ADataType type){
		this.type = Type.Type;
		this.typeT = type;
	}
}

/// identifier node.
/// each node will be a name, with optional parameters applied to a it
public struct Ident{
public:
	/// the identifier
	string ident;
	/// parameters, if any
	AValCT[] params;
	/// previous, if any
	Ident* prev;
	/// constructor
	this (string ident, AValCT[] params, Ident* prev = null){
		this.ident = ident;
		this.params = params;
		this.prev = prev;
	}
	/// constructor
	this (string ident, Ident* prev = null){
		this.ident = ident;
		this.prev = prev;
	}
	/// Returns: string representation
	string toString() const pure {
		string ret = ident;
		if (prev)
			ret = format!"%s.%s"(prev.toString, ret);
		if (params.length)
			ret = format!"%s(%s)"(ret, params.map!(p => p.toString).join(","));
		return ret;
	}

	bool opEquals()(auto ref const Ident rhs) const pure {
		return this is rhs || toString == rhs.toString;
	}

	size_t toHash() const pure {
		return *cast(ulong*)toString.crc32Of.ptr;
	}
}

/// a symbol
public struct ASymbol{
	/// identifier, `_` if anonymous
	@property auto inout ident() pure {
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
				//return aliasS.ident;
				return varS.ident; // HACK: just to get it to compile
			case Type.Import:
				return importS.ident;
			case Type.Template:
				//return templateS.ident;
				return varS.ident; // HACK: just to get it to compile
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

	bool opEquals()(auto const ref AStruct structS) const {
		return this.type == Type.Struct && this.structS == structS;
	}
	bool opEquals()(auto const ref AUnion unionS) const {
		return this.type == Type.Union && this.unionS == unionS;
	}
	bool opEquals()(auto const ref AEnum enumS) const {
		return (this.type == Type.Enum || this.type == Type.EnumMember) &&
			this.enumS == enumS;
	}
	bool opEquals()(auto const ref AEnumMember enumS) const {
		return (this.type == Type.Enum || this.type == Type.EnumMember) &&
			this.enumS == enumS;
	}
	bool opEquals()(auto const ref AEnumConst enumCS) const {
		return this.type == Type.EnumConst && this.enumCS == enumCS;
	}
	bool opEquals()(auto const ref AFn fnS) const {
		return this.type == Type.Fn && this.fnS == fnS;
	}
	bool opEquals()(auto const ref AVar varS) const {
		return this.type == Type.Var && this.varS == varS;
	}
	bool opEquals()(auto const ref AAlias aliasS) const {
		return this.type == Type.Alias && this.aliasS == aliasS;
	}
	bool opEquals()(auto const ref AImport importS) const {
		return this.type == Type.Import && this.importS == importS;
	}
	bool opEquals()(auto const ref ATemplate templateS) const {
		return this.type == Type.Template && this.templateS == templateS;
	}

	bool opEquals()(auto const ref ASymbol rhs) const {
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

	size_t toHash() const pure {
		return ident().toHash;
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
	/// structs
	AStruct[] structs;
	/// unions
	AUnion[] unions;
	/// enums
	AEnum[] enums;
	/// enum consts
	AEnumConst[] enumConsts;
	/// functions
	AFn[] fns;
	/// variables
	AVar[] vars;
	/// aliases
	AAlias[] aliases;
	/// imports
	AImport[] imports;
	/// templates
	ATemplate[] templates;
}

/// Alis Data Type
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
		EnumConst, /// an enumConst
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
		struct{
			/// whether it is a unique type, for `Struct` or `Union`
			bool isUnique;
			union{
				/// struct type, for `Struct`
				AStruct structT;
				/// union type, for `Union`
				AUnion unionT;
			}
		}
		/// enum type, for `Enum`
		AEnum* enumT;
		/// EnumConst type, for `EnumConst`
		AEnumConst* enumConstT;
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
				return this == ofString;
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
				return structT.toString;
			case Type.Union:
				return unionT.toString;
			case Type.Enum:
				return enumT.toString;
			case Type.EnumConst:
				return enumConstT.toString;
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
				return structT.sizeOf;
			case Type.Union:
				return unionT.sizeOf;
			case Type.Enum:
				return enumT.type.sizeOf;
			case Type.EnumConst:
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
	static ADataType of(AStruct structT) pure {
		ADataType ret;
		ret.type = Type.Struct;
		ret.structT = structT;
		return ret;
	}

	/// Returns: union type
	static ADataType of(AUnion unionT) pure {
		ADataType ret;
		ret.type = Type.Union;
		ret.unionT = unionT;
		return ret;
	}

	/// Returns: enum type
	static ADataType of(AEnum enumT) pure {
		ADataType ret;
		ret.type = Type.Enum;
		ret.enumT = [enumT].ptr;
		return ret;
	}

	/// Returns: enum const type
	static ADataType of(AEnumConst enumConstT) pure {
		ADataType ret;
		ret.type = Type.EnumConst;
		ret.enumConstT = [enumConstT].ptr;
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

	bool opEquals(const ADataType rhs) const {
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
				return structT == rhs.structT;
			case Type.Union:
				return unionT == rhs.unionT;
			case Type.Enum:
				return enumT == rhs.enumT;
			case Type.EnumConst:
				return enumConstT == rhs.enumConstT;
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
		string ret = "ADT {\n#\ttype\toffset\tname\tval\n";
		size_t off;
		foreach (size_t i; 0 .. types.length){
			ret ~= format!"%d\t%s\t%d\t%s\t%s\n"(i, types[i].toString, offsets[i],
					names[i], types[i].decodeStr(tb[off .. off + types[i].sizeOf]));
			off += types[i].sizeOf;
		}
		ret ~= "}";
		return ret;
	}
}

/// Alis struct
public struct AStruct{
	/// identifier, `_` if anonymous
	Ident ident;
	/// structure
	ADT dt;
	/// Virtual Table, if any
	ADT vt;
	/// whether this has an `alias this = X`. the member being aliased to `this`
	/// will be at index 0 in `types` and `offsets`
	bool hasBase = false;
	/// Returns: size of this struct
	@property size_t sizeOf() const pure {
		return dt.sizeOf + (vt.tb.length > 0);
	}

	string toString() const pure {
		return format!"struct %s{\nDataTable:\n%s\nVirtualTable:\n%s}"(ident, dt,
				vt);
	}
}

/// Alis union
public struct AUnion{
	/// identifier, `_` if anonymous
	Ident ident;
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
	/// Returns: true if this is an unnamed union
	@property bool isUnnamed() const pure {
		return names.length == 0;
	}
	/// Returns: size of this union
	@property size_t sizeOf() const pure {
		return dt.length + size_t.sizeof;
	}

	string toString() const pure {
		string ret = format!"union %s{\n#\ttype\tname\tval\n"(ident);
		foreach (size_t i, ref const ADataType type; types){
			ret ~= format!"%d\t%s\t%s\t%s\n"(i,
					types[i].toString, names.length ? names[i] : null,
					defInd == i ? types[i].decodeStr(dt) : null);
		}
		ret ~= "}";
		return ret;
	}
}

/// Alis Enum
public struct AEnum{
	/// identifier
	Ident ident;
	/// Data Type. This will be `struct{}` in case of empty emum
	ADataType type;
	/// members
	AEnumMember[] members;

	string toString() const pure {
		string ret = format!"enum %s %s{\n#\tname\tval\n"(type, ident);
		foreach (size_t i, const AEnumMember member; members){
			ret ~= format!"%d\t%s"(i, member);
		}
		ret ~= "}";
		return ret;
	}
}

/// Alis Enum Members
public struct AEnumMember{
	/// identifier
	Ident ident;
	/// value
	ubyte[] val;
	string toString() const pure {
		return format!"%s\t%s\n"(ident, val);
	}
}

/// Alis Enum Constant
public struct AEnumConst{
	/// identifier
	Ident ident;
	/// type
	ADataType type;
	/// value bytes
	ubyte[] data;

	string toString() const pure {
		return format!"enum %s %s = %s;"(type.toString, ident,
				type.decodeStr(data));
	}
}

/// Alis Function Information
public struct AFn{
	/// identifier, `_` if anonymous
	Ident ident;
	/// return type
	ADataType retT;
	/// locals, including parameters
	ADT params;
	/// label name in ABC
	string labN;
	/// whether this is an alis function (true) or an external (false)
	bool isAlisFn;

	string toString() const pure {
		return format!
			"fn %s%s->%s params={\n%s\n} ret=%s"(
					(isAlisFn ? "" : "external "),
					ident, retT, params, retT);
	}
}

/// Alis Variable
public struct AVar{
	/// identifier
	Ident ident;
	/// data type
	ADataType type;
	/// offset
	size_t offset;
	/// whether is global or local
	bool isGlobal;

	string toString() const pure {
		return format!"var %s%s %s ,off=%d"(isGlobal ? "global" : null, type,
				ident, offset);
	}
}

/// Alis Alias
public struct AAlias{
	// TODO: what to store in AAlias

	string toString() const pure {
		return format!"alias";
	}
}

/// Alis Import
public struct AImport{
	/// module being imported
	string[] modIdent;
	/// aliased name, if any
	Ident ident;

	string toString() const pure {
		return format!"import %s as %s"(modIdent.join("."), ident);
	}
}

/// Alis Template
public struct ATemplate{
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
