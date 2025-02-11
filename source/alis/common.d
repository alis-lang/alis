module alis.common;

/// A resolution chain node
public struct ResNode{
	/// next, if any
	ResNode* next;
}

/// Alis Data Type
public struct ADataType{
	/// whether it is a const
	bool isConst;
	/// possible Data Types
	enum Type{
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
		string nameS; /// name of symbol, for `Sym` TODO: fix it? maybe?
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

/// a symbol table entry
public struct ASymbol{
public:
	/// symbol name
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
