module alis.common;

import std.string,
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
			ADataType typeL; /// data type for `Literal`
		}
		ASymbol symS; /// symbol for `Symbol`
		ADataType typeT; /// data type for `Type`
	}

	string toString() const pure {
		final switch (type){
			case Type.Literal:
				// TODO decode `dataL` through `typeL`
				break;
			case Type.Symbol:
				return symS.toString;
			case Type.Type:
				return typeT.toString;
		}
		return null; // TODO
	}
}

/// an identiier node
public struct IdentNode{
	/// the identifier
	string ident;
	/// parameters, if any
	AValCT[] params;
	/// next IdentNode, if any, otherwise `null`
	IdentNode* next;
	/// Returns: string representation
	@property string toString() const pure {
		string ret = ident;
		if (params)
			ret = format!"%s(%s)"(ret, params.map!(p => p.toString).join(","));
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
	IdentNode ident;
	/// possible Symbol types
	enum Type{
		Struct,
		Union,
		Enum,
		EnumMember,
		EnumConst,
		FParam,
		Fn,
		Var,
		Alias,
		Import,
		TParam,
		Template,
	}
	// TODO: complete ASymbol
	/// Returns: string representation
	@property string toString() const pure {
		return ident.toString;
	}
}

/// an Alis Module
public class AModule{
	/// struct definitions
	AStruct[] structs;
	/// union definitions
	AUnion[] unions;
}

/// A resolution chain node.
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
		Int, /// an integer, `ptrdiff_t`
		IntX, /// an integer of X bits
		UInt, /// an unsigned integer, `size_t`
		UIntX, /// an unsigned integer of X bits
		Float, /// a floating point number, `double`
		FloatX, /// a floating point number of X bits
		Char, /// a character of 8 bits
		CharX, /// a character of X bits
		Bool, /// a boolean
		Slice, /// a slice
		Array, /// an array
		Fn, /// a function
		Ref, /// a reference
		Struct, /// a struct
		Union, /// a union
		Enum, /// an enum
		Auto, /// yet to be inferred
	}
	/// type
	Type type;
	union{
		ubyte x; /// X-bits for `IntX`, `UIntX`, `FloatX`, or `CharX`
		ADataType* refT; /// type being referenced, for `Ref`, `Slice`, or `Array`
		ADataType[] seqT; /// type sequence, for `Seq`
		struct{
			bool isUnique; /// whether it is a unique type, for `Struct` or `Union`
			string nameT; /// name, if any, for `Struct` or `Union`
			union{
				AStruct* structT; /// struct type, for `Struct`
				AUnion* unionT; /// union type, for `Union`
			}
		}
	}

	string toString() const pure {
		final switch (type){
			case Type.Seq:
				return "(" ~ seqT.map!(t => t.toString).join(",") ~ ")";
			case Type.Int:
				return "int";
			case Type.IntX:
				return x.format!"$int(%d)";
			case Type.UInt:
				return "uint";
			case Type.UIntX:
				return x.format!"$uint(%d)";
			case Type.Float:
				return "float";
			case Type.FloatX:
				return x.format!"$float(%d)";
			case Type.Char:
				return "char";
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
			case Type.Auto:
				return "auto";
		}
		assert(false);
	}
}

/// Alis virtual table
public struct AVT{

}

/// Alis struct
public struct AStruct{
	/// types of members
	ADataType[] types;
	/// byte offsets for members
	size_t[] offsets;
	/// maps member names/aliases to index in `types` and `offsets`
	size_t[string] nameInds;
	/// Virtual Table, if any
	AVT* vt;
	/// whether this has an `alias this = X`. the member being aliased to `this`
	/// will be at index 0 in `types` and `offsets`
	bool hasBase;
}

/// Alis union
public struct AUnion{
	/// types of members
	ADataType[] types;
	/// byte offsets for members
	size_t[] offsets;
	/// maps member names/aliases to index in `types` and `offsets`
	size_t[string] names;
	/// whether this has an `alias this = X`. the member being aliased to `this`
	/// will be at index 0 in `types` and `offsets`
	bool hasBase;
	/// Returns: true if this is an unnamed union
	@property bool isUnnamed() const pure {
		return names.length == 0;
	}
}

// TODO implement rest of alis/common.d
