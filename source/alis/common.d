module alis.common;

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
		Sym, /// a symbol
	}
	union{
		ubyte x; /// X-bits for `IntX`, `UIntX`, `FloatX`, or `CharX`
		ADataType* refT; /// type being referenced, for `Ref`
		ADataType* elemT; /// element type for `Slice` or `Array`
		string nameS; /// name of symbol, for `Sym`
									/// TODO: maybe should store expression instead?
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
}

/// Alis struct
public struct AStruct{
	/// maps member names to member data types. Aliases are not part of this
	ADataType[string] members;
	/// aliases
	string[string] aliases;
}

/// Alis union
public struct AUnion{
	/// types of members
	ADataType[] types;
	/// member names, mapped to their indexes in `types`
	size_t[string] nameInds;
	/// whether this is an unnamed union
	@property bool isUnnamed() const pure {
		return nameInds.length != types.length;
	}
}

/// a symbol
public struct ASymbol{
public:
	/// symbol name (in its own local scope)
	string name;
	// TODO implement ASymbol
}

/// an Alis Module
public class AModule{
	/// struct definitions
	AStruct[string] defStructs;
	/// union definitions
	AUnion[string] defUnions;
}

// TODO implement rest of alis/common.d
