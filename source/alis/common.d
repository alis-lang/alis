module alis.common;

/// A resolution chain node
public struct ResNode{

}

/// Alis Data Type
public struct ADataType{
	/// whether this is a reference
	// TODO implement ADataType
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
