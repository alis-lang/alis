module alis.common;

import std.string,
			 std.range,
			 std.format,
			 std.typecons,
			 std.algorithm;

/// an Alis CompileTime Value
public struct AValCT{
	/// possible types
	enum Type{
		Literal, /// some Literal value
		Symbol, /// an Alias to a symbol
		Type, /// a Data Type
	}
	/// currently stored type
	Type type;
	union{
		struct{
			ubyte[] dataL; /// data for `Literal`
			ADataType* typeL; /// data type for `Literal`
		}
		ASymbol* symS; /// symbol for `Symbol`
		ADataType* typeT; /// data type for `Type`
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
}

/// an identiier node
public struct Ident{
	/// the identifier
	string ident;
	/// parameters, if any
	AValCT[] params;
	/// next Ident, if any, otherwise `null`
	Ident* next;
	/// Returns: string representation
	@property string toString() const pure {
		string ret = ident;
		if (params)
			ret = format!"%s(%s)"(ident, params.map!(p => p.toString).join(","));
		if (next)
			return format!"%s.%s"(ret, next.toString);
		return ret;
	}
}

/// a symbol
public struct ASymbol{
public:
	/// symbol name (in its own local scope)
	string name;
	/// identifier
	Ident* ident;
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
		Template, // TODO add Template sub-types (fn/struct/etc)
	}
	union{
		AStruct* structS; /// struct for `Type.Struct`
		AUnion* unionS; /// union for `Type.Union`
		struct{
			/// enum for `Type.Enum`, or `Type.EnumMember`
			AEnum* enumS;
			/// enum member name for `Type.EnumMember`
			string enumMember;
		}
		AEnumConst* enumCS; /// enum for `Type.EnumConst`
		AFn* fnS; /// function for `Type.Fn`
		AVar* varS; /// variable for `Type.Var`
		AAlias* aliasS; /// alias for `Type.Alias`
		AImport* importS; /// import for `Type.Import`
	}
	/// Returns: string representation, equivalent to `ASymbol.ident.toString`
	@property string toString() const pure {
		return ident.toString;
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

/// A resolution chain node. TODO: what is this?
public struct Resolution{
private:
	Resolution* _leaf;
	Resolution* _next;
	Resolution* _prev;

public:
	/// previous resolution, if any
	@property Resolution* prev() pure {
		return _prev;
	}
	/// next resolution, if any
	@property Resolution* next() pure {
		return _next;
	}

	/// Returns: most resolved Resolution. will be `this` if `next is null`
	@property Resolution* leaf() pure {
		if (_leaf is null){
			if (_next is null)
				return _leaf = &this;
			return _leaf = next.leaf;
		}
		if (_leaf.next){
			_leaf._leaf = null;
			return _leaf = _leaf.leaf;
		}
		return _leaf;
	}
	/// pushes a new resolution at end of this list
	void push(Resolution* r) pure {
		leaf._next = r;
	}

	/// Types of what is being resolved
	enum TypeS{
		Ident, /// identifier resolution
		Alias, /// alias resolution
		OpBinEval, /// binary operator evaluation resolution
		OpPreEval, /// prefix operator evaluation resolution
		OpPostEval, /// postfix operator evaluation resolution
		Intrinsic, /// intrinsic resolution
		IntrinsicCall, /// intrinsic call resolution
		Call, /// call resolution
		OpDot, /// OpDot resolution
	}

	/// Types of what it resolved to
	enum TypeT{
		Alias, /// resolves to Alias
		DataType, /// resolves to a Data Type
		Fn, /// resolves to a function
		Template, /// resolves to a template
		Mixin, /// resolves to a mixin template
		Var, /// resolves to a var
		TParam, /// resolves to a template parameter.
						/// TODO: maybe resolve it completely here?
		FParam, /// resolves to a funtion parameter
						/// TODO: maybe resolve it completely here?
		EnumConst, /// resolves to an enum const definition. Regular enum->DataType
		EnumMemeber, /// resolves to enum member
		Import, /// resolves to an imported module's aliased name
	}

	/// what is being resovled
	TypeS typeS;
	/// result from resolution
	ASymbol sym; // TODO: store ptr?
	union{
		/// subject symbol name, for `Ident`, `Intrinsic`, or `IntrinsicCall`
		string nameS;
		/// subject symbol for `Alias`, or `Call`
		ASymbol* symS;
		struct{
			string op; /// operator, for `OpBinEval`, `OpPreEval`, or `OpPostEval`
			union{
				ASymbol* operand; /// operand, for `OpPreEval`, or `OpPostEval`
				struct{
					ASymbol* operandLhs; /// LHS operand, for `OpBinEval`
					ASymbol* operandRhs; /// RHS operand, for `OpBinEval`
				}
			}
		}
	}
}

/// Alis Data Type
public struct ADataType{
	/// whether it is a const
	bool isConst;
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
		Fn, /// a function
		Ref, /// a reference
		Struct, /// a struct
		Union, /// a union
		Enum, /// an enum
		NoInit, /// `$noinit`
	}
	/// type
	Type type;
	union{
		ubyte x; /// X-bits for `IntX`, `UIntX`, `FloatX`, or `CharX`
		ADataType* refT; /// type being referenced, for `Ref`, `Slice`, or `Array`
		ADataType[] seqT; /// type sequence, for `Seq`
		struct{
			bool isUnique; /// whether it is a unique type, for `Struct` or `Union`
			string nameT; /// name, if any, for `Struct`, `Union`, `Enum`, or `EnumConst`
			union{
				AStruct* structT; /// struct type, for `Struct`
				AUnion* unionT; /// union type, for `Union`
				AEnum* enumT; /// enum type, for `Enum`
				AEnumConst* enumConstT; /// EnumConst type, for `EnumConst`
			}
		}
	}

	string toString() const pure {
		final switch (type){
			case Type.Seq:
				return "(" ~ seqT.map!(t => t.toString).join(",") ~ ")";
			case Type.IntX:
				return x.format!"$int(%d)";
			case Type.UIntX:
				return x.format!"$uint(%d)";
			case Type.FloatX:
				return x.format!"$float(%d)";
			case Type.CharX:
				return x.format!"$char(%d)";
			case Type.Bool:
				return "bool";
			case Type.Slice:
				return (*refT).toString.format!"$slice(%s)";
			case Type.Array:
				return (*refT).toString.format!"$array(%s)";
			case Type.Fn:
				// TODO: implement ADataType.Type.Fn .toString
			case Type.Ref:
				return (*refT).toString.format!"@%s";
			case Type.Struct:
				// TODO: implement ADataType.Type.Struct .toString
			case Type.Union:
				// TODO: implement ADataType.Type.Union .toString
			case Type.Enum:
				// TODO: implement ADataType.Type.Enum .toString
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
				return 2 * null.sizeof; // ptr + length
			case Type.Array:
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
}

/// Alis data table (structure behind virtual tables & closures etc)
public struct ADT{
	/// types of fields
	ADataType[] types;
	/// byte offsets for fields
	size_t[] offsets;
	/// maps member names to index in `types` and `offsets`
	size_t[string] nameInds;
	/// table itself
	ubyte[] tb;
	/// Returns: size of virtual table
	pragma(inline, true) @property size_t sizeOf() const pure {
		return tb.length;
	}
}

/// Alis struct
public struct AStruct{
	/// structure
	ADT dt;
	/// Virtual Table, if any
	ADT* vt;
	/// whether this has an `alias this = X`. the member being aliased to `this`
	/// will be at index 0 in `types` and `offsets`
	bool hasBase = false;
	/// Returns: size of this struct
	@property size_t sizeOf() const pure {
		return dt.sizeOf + vt ? vt.sizeOf : 0;
	}
}

/// Alis union
public struct AUnion{
	/// types of members
	ADataType[] types;
	/// maps member names/aliases to index in `types` and `offsets`
	size_t[string] names;
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
}

/// Alis Enum
public struct AEnum{
	/// Data Type. This will be `struct{}` in case of empty emum
	ADataType* type;
	/// member names, mapped to their values
	ubyte[][string] members;
}

/// Alis Enum Constant
public struct AEnumConst{
	/// type
	ADataType* type;
	/// value bytes
	ubyte[] data;
}

/// Alis Function Information
public struct AFn{
	/// return type
	ADataType* retT;
	/// locals, including parameters
	ADT locals;
	/// label name in ABC
	string labN;
	/// how many parameters must be provided
	size_t paramRequired;
	/// stack frame size
	size_t stackFrameSize;
}

/// Alis Variable
public struct AVar{
	/// data type
	ADataType* type;
	/// offset
	size_t offset;
}

/// Alis Alias
public struct AAlias{
	// TODO: what to store in AAlias
}

/// Alis Import
public struct AImport{
	/// module being imported
	string[] modIdent;
	/// aliased name, if any
	string name;
}

/// Alis Template
public struct ATemplate{
	// TODO: what to store in ATemplate
}

// TODO implement rest of alis/common.d
