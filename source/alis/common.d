/++
Alis Common Data Types
+/
module alis.common;

import alis.utils;

import std.string,
			 std.traits,
			 std.range,
			 std.conv,
			 std.meta,
			 std.format,
			 std.typecons,
			 std.algorithm,
			 std.digest.crc;

import alis.compiler.common;

import meta;

public import alis.compiler.ast.rst : RExpr;

debug import std.stdio;

public import alis.compiler.common : Visibility; // TODO move it here

/// Alis value, with ADataType
public struct AVal{
	void[] data; /// the data
	ADataType type = ADataType.ofNoInit; /// data type

	/// Returns: true if this can be implicitly casted to target type
	public bool canCastTo(const ADataType target) pure {
		if (type.type == ADataType.Type.IntX ||
				type.type == ADataType.Type.UIntX){
			if (target.type == ADataType.Type.IntX){
				switch (target.x){
					static foreach (T; SignedInts){
						case T.sizeof * 8:
							return as!T.isVal;
					}
					default:
						return false;
				}
			} else
			if (target.type == ADataType.Type.UIntX){
				switch (target.x){
					static foreach (T; UnsignedInts){
						case T.sizeof * 8:
							return as!T.isVal;
					}
					default:
						return false;
				}
			}
		}
		return type.canCastTo(target);
	}

	/// converts this value into `target` type
	/// Returns: converted value, or nothing if cannot be done
	public OptVal!AVal to(ADataType target) pure {
		assert (this.canCastTo(target));
		AVal ret;
		switch (type.type){
			case ADataType.Type.IntX:
			case ADataType.Type.UIntX:
			case ADataType.Type.Char:
				switch (target.type){
					case ADataType.Type.IntX:
						switch (target.x){
							static foreach (Type; SignedInts){
								case Type.sizeof * 8:
									OptVal!Type r = this.as!Type;
									if (r.isVal)
										return OptVal!AVal(r.val.AVal);
									return OptVal!AVal();
							}
							default:
								return OptVal!AVal();
						}
					case ADataType.Type.UIntX:
						switch (target.x){
							static foreach (Type; UnsignedInts){
								case Type.sizeof * 8:
									OptVal!Type r = this.as!Type;
									if (r.isVal)
										return OptVal!AVal(r.val.AVal);
									return OptVal!AVal();
							}
							default:
								return OptVal!AVal();
						}
					case ADataType.Type.FloatX:
						switch (target.x){
							static foreach (Type; Floats){
								case Type.sizeof * 8:
									OptVal!Type r = this.as!Type;
									if (r.isVal)
										return OptVal!AVal(r.val.AVal);
									return OptVal!AVal();
							}
							default:
								return OptVal!AVal();
						}
					case ADataType.Type.Char:
						OptVal!ubyte r = this.as!ubyte;
						if (r.isVal)
							return OptVal!AVal(r.val.AVal);
						return OptVal!AVal();
					default:
						return OptVal!AVal();
				}
			default:
				debug stderr.writefln!
					"STUB: AVal.to(ADataType) not implemented for %s->%s"(this, target);
				return OptVal!AVal();
		}
		return ret.OptVal!AVal;
	}

	/// decodes this data into a type `T`
	/// Returns: Optional value of type `T`
	public OptVal!T as(T...)() pure if (T.length && allSatisfy!(isType, T)){
		static if (T.length == 1){
			assert (type.sizeOf == data.length);
			static if (std.traits.isNumeric!(T[0])){
				static if (isFloatingPoint!(T[0])){
					if (!type.canCastTo(ADataType.of!T))
						return OptVal!T();
					if (type.type != ADataType.Type.FloatX ||
							type.x > T[0].sizeof * 8)
						return OptVal!(T[0])();
					switch (data.length){
						static foreach (Type; AliasSeq!(float, double)){
							static if (T[0].sizeof >= Type.sizeof){
								case Type.sizeof:
									return OptVal!(T[0])(data.as!Type);
							}
						}
					default:
						debug stderr.writefln!"invalid data found in AVal.as";
						return OptVal!(T[0])();
					}
				} else {
					if (type.type == ADataType.Type.IntX){
						ptrdiff_t num;
						if (T[0].sizeof * 8 < type.x)
							return OptVal!(T[0])();
signedSwitch:
						switch (data.length){
							static foreach (Type; SignedInts){
								case Type.sizeof:
									num = data.as!Type;
									break signedSwitch;
							}
							default:
							debug stderr.writefln!"invalid data found in AVal.as";
							return OptVal!(T[0])();
						}
						if (num < (T[0]).min || num > (T[0]).max)
							return OptVal!(T[0])();
						return OptVal!(T[0])(cast(T[0])num);
					} else
						if (type.type == ADataType.Type.UIntX){
							size_t num;
unsignedSwitch:
							switch (data.length){
								static foreach (Type; UnsignedInts){
									case Type.sizeof:
										num = data.as!Type;
										break unsignedSwitch;
								}
								default:
								debug stderr.writefln!"invalid data found in AVal.as";
								return OptVal!(T[0])();
							}
							if (num < (T[0]).min || num > (T[0]).max)
								return OptVal!(T[0])();
							return OptVal!(T[0])(cast(T[0])num);
						}
				}

			} else
			static if (is (T[0] == char)){
				return OptVal!(T[0])((cast(char[])data)[0]);
			} else
			static if (isBoolean!T){
				if (type.type != ADataType.Type.Bool)
					return OptVal!(T[0])();
				return OptVal!(T[0])((cast(ubyte[])data)[0] != 0);

			} else
			static if (isPointer!T){
				static assert (false, "AVal for pointers not yet implemented");

			} else
			static if (is (T == string) || (isArray!T && is(ElementType!T == dchar))){
				if (type != ADataType.ofString)
					return OptVal!(T[0])();
				char* ptr = data[0 .. ptrdiff_t.sizeof].as!(char*);
				size_t len = data[ptrdiff_t.sizeof .. $].as!size_t;
				return OptVal!(T[0])(cast(string)ptr[0 .. len]);

			} else
			static if (isArray!T){
				if (type.type != ADataType.Type.Slice &&
						type.type != ADataType.Type.Array)
					return OptVal!(T[0])();
				ADataType elemT = *type.refT;
				if (elemT != ADataType.of!(ElementType!T))
					return OptVal!(T[0])();
				ElementType!T* ptr = data[0 .. ptrdiff_t.sizeof].as!(ElementType!T*);
				size_t len = data[ptrdiff_t.sizeof .. $].as!size_t;
				return OptVal!T(cast(T[0])ptr[0 .. len]);

			} else
			static if (isFunction!(T[0]) || isFunctionPointer!(T[0])){
				static assert (false, "AVal.as does not suppport Fn yet");
			} else
			static if (is (T[0] == struct)){
				static assert (false, "AVal.as does not suppport Struct yet");
			} else
			static if (is (T[0] == union)){
				static assert (false, "AVal.as does not suppport Union yet");
			} else
			static if (is (T[0] == enum)){
				static assert (false, "AVal.as does not suppport Enum yet");
			} else {
				static assert (false,
						T.stringof.format!"Unsupported data type for AVal.as %s");
			}
		} else {
			static assert (false, "AVal does not yet support Sequences fully");
		}
		assert (false);
	}

	/// encodes some data
	/// Returns: Optional AVal if done, empty if failed
	public static OptVal!AVal of(T)(T val) pure {
		static if (std.traits.isNumeric!T){
			void[] d = val.asBytes;
			ADataType t = ADataType.of!T;
			assert (t.sizeOf >= d.length);
			return AVal(t, d).OptVal!AVal;
		} else
		static if (is (T == char)){
			return AVal(ADataType.ofChar, [val]).OptVal!AVal;
		} else
		static if (isBoolean!T){
			return AVal(ADataType.ofBool, val ? [true] : [false]).OptVal!AVal;
		} else
		static if (isPointer!T){
			static assert (false, "AVal for pointers not yet implemented");
		} else
		static if (is (T == string) || isStaticArray!T){
			void[] d = new void[size_t.sizeof * 2];
			d[0 .. size_t.sizeof] = val.ptr.asBytes;
			d[size_t.sizeof .. $] = val.length.asBytes;
			ADataType t = ADataType.of!T;
			assert (t.sizeOf == d.length);
			return AVal(t, d).OptVal!AVal;
		} else
		static if (isArray!T){
			void[] d = new void[size_t.sizeof * 3];
			d[0 .. size_t.sizeof] = val.ptr.asBytes;
			d[size_t.sizeof .. 2 * size_t.sizeof] = val.length.asBytes;
			d[2 * size_t.sizeof .. $][] = val.length.asBytes; // TODO: use capacity
			ADataType t = ADataType.of!T;
			assert (t.sizeOf == d.length);
			return AVal(t, d).OptVal!AVal;
		} else
		static if (isFunction!(T[0]) || isFunctionPointer!(T[0])){
			static assert (false, "AVal.of does not suppport Fn yet");
		} else
		static if (is (T[0] == struct)){
			static assert (false, "AVal.of does not suppport Struct yet");
		} else
		static if (is (T[0] == union)){
			static assert (false, "AVal.of does not suppport Union yet");
		} else
		static if (is (T[0] == enum)){
			static assert (false, "AVal.of does not suppport Enum yet");
		} else {
			static assert (false, "Unsupported data type for AVal.of");
		}
		assert (false);
	}

	public string toString() const pure {
		final switch (type.type){
		case ADataType.Type.Seq:
			break;
		case ADataType.Type.IntX:
			break;
		case ADataType.Type.UIntX:
			break;
		case ADataType.Type.FloatX:
			break;
		case ADataType.Type.Char:
			break;
		case ADataType.Type.Bool:
			break;
		case ADataType.Type.Slice:
			break;
		case ADataType.Type.Array:
			break;
		case ADataType.Type.Ref:
			break;
		case ADataType.Type.Fn:
			break;
		case ADataType.Type.Struct:
			break;
		case ADataType.Type.Union:
			break;
		case ADataType.Type.Enum:
			break;
		case ADataType.Type.NoInit:
			break;
		}
		return format!"{type: %s, data: %s}"(type, cast(ubyte[])data);
		assert (false);
	}

	/// constructor
	this(ADataType type, void[] data) pure {
		assert (data.length == type.sizeOf,
				format!"%d != %d"(data.length, type.sizeOf));
		this.type = type;
		this.data = data;
	}

	/// ditto
	this(T...)(T val) pure if (T.length && allSatisfy!(isType, T)){
		this.type = ADataType.of!T;
		static if (T.length == 1){
			OptVal!AVal v = of(val);
			assert (v.isVal);
			this = v.val;
		} else {
			enum SizeOfSeq = ADataType.of!T.sizeOf;
			data = new void[SizeOfSeq];
			static foreach (size_t I; 0 .. T.length){{
				enum PrefixSum = ADataType.of!(T[0 .. I]).sizeOf;
				enum Size = ADataType.of!(T[I]).sizeOf;
				AVal encoded = encode(val[i]);
				assert(encoded.type == type.seqT[i]);
				data[PrefixSum .. PrefixSum + Size] = encoded.data;
			}}
		}
	}
}

///
unittest{
	assert("hello world".AVal.as!string.val == "hello world");
	assert(AVal('a').as!char.val == 'a');
	assert(AVal(5.5).as!double.val == 5.5);
	assert(AVal(5).as!int.val == 5);
	assert(AVal(5U).as!ulong.val == 5);
	assert(AVal(5).as!long.val == 5);
	assert(true.AVal.as!bool.val == true);
	assert(false.AVal.as!bool.val == false);
	assert([1, 2].AVal.as!(int[]).val == [1, 2]);
}

/// an Alis CompileTime Value
public struct AValCT{
	/// possible types
	enum Type{
		Literal, /// some Literal value
		Symbol, /// an alias to a symbol
		Type, /// a Data Type
		Expr, /// an alias to an RExpr
		Seq, /// alias (sequence)
	}

	/// Returns: true if this can be implicitly casted to target
	bool canCastTo(const ADataType target){
		import alis.compiler.semantic.typeofexpr : typeOf;
		import alis.compiler.semantic.error : SmErrsVal;
		final switch (type){
			case Type.Literal:
				return val.canCastTo(target);
			case Type.Symbol:
				OptVal!ADataType t = symS.valType;
				return t.isVal && t.val.canCastTo(target);
			case Type.Type:
				return typeT.canCastTo(target);
			case Type.Expr:
				SmErrsVal!ADataType t = typeOf(expr);
				if (t.isErr)
					return false;
				return t.val.canCastTo(target);
			case Type.Seq:
				debug stderr.writefln!"STUB: Seq.canCastTo -> false";
				return false;
		}
	}

	/// whether this is a value
	@property bool isVal(){
		import alis.compiler.semantic.typeofexpr;
		final switch (type){
			case Type.Literal:
				return true;
			case Type.Symbol:
				return symS.isVal;
			case Type.Type:
				return false;
			case Type.Expr:
				return !expr.typeOf.isErr;
			case Type.Seq:
				foreach (AValCT val; seq){
					if (!val.isVal)
						return false;
				}
				return true;
		}
	}

	/// Returns: data type of value, if `isVal`
	OptVal!ADataType valType(){
		import alis.compiler.semantic.typeofexpr;
		final switch (type){
			case Type.Literal:
				return val.type.OptVal!ADataType;
			case Type.Symbol:
				return symS.valType;
			case Type.Type:
				return typeT.OptVal!ADataType;
			case Type.Expr:
				return expr.typeOf.val.OptVal!ADataType;
			case Type.Seq:
				return ADataType.ofSeq(seq.map!(s => s.valType.val).array)
					.OptVal!ADataType;
		}
	}

	/// currently stored type
	Type type = Type.Type;
	union{
		AVal val; /// value for `Literal`
		ASymbol* symS; /// symbol for `Symbol`
		ADataType typeT; /// data type for `Type`
		RExpr expr; /// expr in case of `Expr`
		AValCT[] seq; /// sequence in case of `Seq`
	}

	string toString() const pure {
		final switch (type){
			case Type.Literal:
				return val.toString;
			case Type.Symbol:
				return symS.toString;
			case Type.Type:
				return typeT.toString;
			case Type.Expr:
				return expr.toString;
			case Type.Seq:
				return seq.format!"(%(%r,%))";
		}
		return null;
	}

	/// constructor
	this (AVal val){
		this.type = Type.Literal;
		this.val = val;
	}
	/// ditto
	this (ASymbol* symS){
		this.type = Type.Symbol;
		this.symS = symS;
	}
	/// ditto
	this (ADataType typeT){
		this.type = Type.Type;
		this.typeT = typeT;
	}
	/// ditto
	this (RExpr expr){
		this.type = Type.Expr;
		this.expr = expr;
	}
	/// ditto
	this (AValCT[] seq){
		this.type = Type.Seq;
		this.seq = seq.dup;
	}

	/// Gets Data Type associated with this AValCT.
	/// In case of symbol, Struct/Union/Enum becomes the type, otherwise none
	/// In case of Literal, the data's type becomes the type
	/// In case of Type, returned as-is
	/// In case of Expr, typeOf(Expr). if typeOf errors, no value
	/// Returns: Optional ADataType
	OptVal!ADataType asType(){
		import alis.compiler.semantic.typeofexpr;
		final switch (type){
			case Type.Symbol:
				return symS.asType;
			case Type.Literal:
				return val.type.OptVal!ADataType;
			case Type.Type:
				return typeT.OptVal!ADataType;
			case Type.Expr:
				auto res = expr.typeOf;
				if (res.isErr)
					return OptVal!ADataType();
				return res.val.OptVal!ADataType;
			case Type.Seq:
				assert (false, "AValCT.Type.Seq in AValCT.asType");
		}
	}

	/// Returns: new flattened AValCT[]
	public AValCT[] flatten(){
		if (type != AValCT.Type.Seq)
			return [this];
		return seq.dup;
	}
}

/// flattens AValCT[]
/// Returns: new flattened AValCT[]
public AValCT[] flatten(AValCT[] seq){
	size_t i = 0;
	while (i < seq.length){
		if (seq[i].type == AValCT.Type.Seq)
			break;
		i ++;
	}
	if (i >= seq.length) return seq.dup;
	AValCT[] ret = seq[0 .. i].dup;
	foreach (ref AValCT val; seq[i .. $]){
		if (val.type == AValCT.Type.Seq)
			ret ~= val.seq.flatten;
		else
			ret ~= val;
	}
	return ret;
}

/// Returns: true if an AValCT[] is flat
public bool isFlat(AValCT[] seq){
	foreach (AValCT val; seq){
		if (val.type == AValCT.Type.Seq)
			return false;
	}
	return true;
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
	//bool opEquals(const IdentU rhs) const pure {
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

/// Returns: IdentU[] as a human readable string
public string toString()(const auto ref IdentU[] id) pure {
	return id.map!(i => i.toString).join(".");
}

/// a symbol
public struct ASymbol{
	/// Whether this is complete
	bool isComplete = false;
	/// corresponding ASTNode (can be null)
	ASTNode ast;
	/// Returns: whether this is a callable (template or function)
	@property bool isCallable() const pure {
		final switch (type){
			case Type.Struct:
			case Type.Union:
			case Type.Enum:
			case Type.Var:
			case Type.Import:
			case Type.EnumConst:
			case Type.UTest:
				return false;
			case Type.Alias:
				return false; // TODO: implement for Alias
			case Type.Fn:
			case Type.Template:
				return true;
		}
	}

	///Returns: true if this is a value
	bool isVal() const pure {
		final switch (type){
			case ASymbol.Type.Struct:
			case ASymbol.Type.Union:
			case ASymbol.Type.Enum:
			case ASymbol.Type.Import:
			case ASymbol.Type.UTest:
			case ASymbol.Type.Template:
				return false;
			case ASymbol.Type.Var:
			case ASymbol.Type.EnumConst:
			case ASymbol.Type.Fn:
				return true;
			case ASymbol.Type.Alias:
				assert (false, "isVal on Alias is undecided");
		}
	}

	/// Returns: data type of value, if `isVal`
	OptVal!ADataType valType() pure {
		final switch (type){
			case ASymbol.Type.Struct:
			case ASymbol.Type.Union:
			case ASymbol.Type.Enum:
			case ASymbol.Type.Import:
			case ASymbol.Type.UTest:
			case ASymbol.Type.Template:
				return OptVal!ADataType();
			case ASymbol.Type.Var:
				return varS.type.OptVal!ADataType;
			case ASymbol.Type.EnumConst:
				return enumCS.type.OptVal!ADataType;
			case ASymbol.Type.Fn:
				return ADataType.ofFn(fnS.retT, fnS.paramsT)
					.OptVal!ADataType;
			case ASymbol.Type.Alias:
				assert (false, "valType on Alias is undecided");
		}
	}

	/// Returns: identifier, `_` if anonymous
	@property inout(IdentU[]) ident() pure inout {
		final switch (type){
			case Type.Struct:
				return structS.ident;
			case Type.Union:
				return unionS.ident;
			case Type.Enum:
				return enumS.ident;
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
			case Type.UTest:
				return utestS.ident;
		}
		assert(false);
	}

	/// Returns: Visibility
	@property Visibility vis() pure {
		final switch (type){
			case Type.Struct:
				return structS.vis;
			case Type.Union:
				return unionS.vis;
			case Type.Enum:
				return enumS.vis;
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
			case Type.UTest:
				return utestS.vis;
		}
		assert(false);
	}

	/// Returns: true if this symbol can be a data type
	@property bool isDType() const pure {
		final switch (type){
			case ASymbol.Type.Struct:
			case ASymbol.Type.Union:
			case ASymbol.Type.Enum:
				return true;
			case ASymbol.Type.EnumConst:
			case ASymbol.Type.Fn:
			case ASymbol.Type.Var:
			case ASymbol.Type.Import:
			case ASymbol.Type.UTest:
			case ASymbol.Type.Template:
				return false;
			case ASymbol.Type.Alias:
				return false; // TODO: implement for Alias
		}
	}

	/// Returns: this symbol, as a type, in Optional ADataType.
	/// Only valid if `this.isDType`
	@property OptVal!ADataType asType() pure {
		final switch (type){
			case ASymbol.Type.Struct:
				return ADataType.of(&structS).OptVal!ADataType;
			case ASymbol.Type.Union:
				return ADataType.of(&unionS).OptVal!ADataType;
			case ASymbol.Type.Enum:
				return ADataType.of(&enumS).OptVal!ADataType;
			case ASymbol.Type.EnumConst:
			case ASymbol.Type.Fn:
			case ASymbol.Type.Var:
			case ASymbol.Type.Import:
			case ASymbol.Type.UTest:
			case ASymbol.Type.Template:
			case ASymbol.Type.Alias:
				return OptVal!ADataType();
		}
	}

	/// possible Symbol types
	enum Type{
		Struct,
		Union,
		Enum,
		EnumConst,
		Fn,
		Var,
		Alias,
		Import,
		Template,
		UTest,
	}
	/// type of this symbol
	Type type;

	union{
		AStruct structS; /// struct for `Type.Struct`
		AUnion unionS; /// union for `Type.Union`
		AEnum enumS; /// enum for `Type.Enum`, or `Type.EnumMember`
		AEnumConst enumCS; /// enum for `Type.EnumConst`
		AFn fnS; /// function for `Type.Fn`
		AVar varS; /// variable for `Type.Var`
		AAlias aliasS; /// alias for `Type.Alias`
		AImport importS; /// import for `Type.Import`
		ATemplate templateS; /// template for `Type.Template`
		AUTest utestS; /// utest for `Type.UTest`
	}

	/// Returns: string representation, equivalent to `ASymbol.ident.toString`
	string toString() const pure {
		string ret;
		final switch (type){
			case Type.Struct:
				ret = structS.toString; break;
			case Type.Union:
				ret = unionS.toString; break;
			case Type.Enum:
				ret = enumS.toString; break;
			case Type.EnumConst:
				ret = enumCS.toString; break;
			case Type.Fn:
				ret = fnS.toString; break;
			case Type.Var:
				ret = varS.toString; break;
			case Type.Alias:
				ret = aliasS.toString; break;
			case Type.Import:
				ret = importS.toString; break;
			case Type.Template:
				ret = templateS.toString; break;
			case Type.UTest:
				ret = utestS.toString; break;
		}
		if (!isComplete)
			ret = '#' ~ ret;
		return ret;
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
	bool opEquals()(auto ref const AUTest utestS) const {
		return this.type == Type.UTest && this.utestS == utestS;
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
	/// ditto
	this (AUTest utestS){
		this.type = Type.UTest;
		this.utestS = utestS;
	}
}

/// an Alis Module. Aggregates all public symbols for a module
public struct AModule{
	/// globals
	// TODO: HOW TO STORE GLOBALS????
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
		Char, /// a 1 byte character
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
		/// X-bits for `IntX`, `UIntX`, `FloatX`
		ubyte x;
		/// type being referenced, for `Ref`, `Slice`, or `Array`
		ADataType* refT;
		/// type sequence, for `Seq`
		ADataType[] seqT;
		/// Struct reference for `Struct`
		AStruct* structS;
		/// Union reference for `Union`
		AUnion* unionS;
		/// Enum reference for `Enum`
		AEnum* enumS;
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
			case Type.IntX, Type.UIntX, Type.FloatX, Type.Char, Type.Bool:
				return true;
			case Type.Slice:
				return this == ADataType.ofString;
			default:
				return false;
		}
	}

	/// Returns: ADataType that is this, but const
	ADataType constOf() const pure {
		ADataType ret = this.copy;
		final switch (type){
			case Type.Slice:
			case Type.Array:
				ret.refT.isConst = true;
				ret.type = Type.Slice;
				break;
			case Type.Seq:
				ret.type = Type.Seq;
				ret.seqT = new ADataType[seqT.length];
				foreach (size_t i, const ref ADataType t; seqT){
					ret.seqT[i] = t.constOf;
				}
				break;
			case Type.IntX:
			case Type.UIntX:
			case Type.FloatX:
			case Type.Char:
			case Type.Bool:
			case Type.Fn:
			case Type.Ref:
			case Type.NoInit:
				ret.isConst = true;
				break;
			case Type.Struct:
			case Type.Union:
			case Type.Enum:
				debug stderr.writefln!"STUB: constOf(%s) returning as is"(type);
				break;
		}
		return ret;
	}

	/// Returns: true if this can be implicitly casted to target
	bool canCastTo(const ADataType target) const pure {
		if (this == target)
			return true;
		if (isConst && !target.isConst)
			return false;
		final switch (type){
			case Type.Seq:
				if (seqT.length != target.seqT.length)
					return false;
				foreach (size_t i; 0 .. seqT.length){
					if (!seqT[i].canCastTo(target.seqT[i]))
						return false;
				}
				return true;
			case Type.IntX:
				return target.type == Type.IntX && x <= target.x;
			case Type.UIntX:
				return (target.type == Type.UIntX || target.type == Type.IntX) &&
					x <= target.x;
			case Type.FloatX:
				return target.type == Type.FloatX && x <= target.x;
			case Type.Char:
				return target.type == Type.Char ||
					(target.type == Type.IntX && target.x > 8) ||
					(target.type == Type.UIntX);
			case Type.Bool:
				return target.type == Type.Bool ||
					target.type == Type.IntX ||
					target.type == Type.UIntX;
			case Type.Slice:
				// TODO: allow $slice(X) -> $slice(const X)
				return target.type == Type.Slice && *refT == *target.refT;
			case Type.Array:
				// TODO: allow $array(X) -> $slice(const X)
				return (target.type == Type.Array || target.type == Type.Slice) &&
					*refT == *target.refT;
			case Type.Fn:
				if (target.type != Type.Fn || *retT != *target.retT ||
						paramT.length != target.paramT.length)
					return false;
				foreach (size_t i; 0 .. paramT.length){
					if (paramT[i] != target.paramT[i])
						return false;
				}
				return true;
			case Type.Ref:
				if (target.type == Type.Ref){
					// TODO: allow X -> const X
					return *refT == *target.refT;
				}
				// TODO: allow X -> const X
				return *refT == target;
			case Type.NoInit:
				return target.type == Type.NoInit;
			case Type.Struct:
				return false; // TODO: implement canCastTo for Struct
			case Type.Union:
				return false; // TODO: implement canCastTo for Union
			case Type.Enum:
				return false; // TODO: implement canCastTo for Enum
		}
		debug stderr.writefln!"STUB: canCastTo(%s, %s) -> false"(this, target);
		return false;
	}

	/// Gets initializing bytes for this type
	void[] initB() const pure {
		debug stderr.writefln!"STUB: ADataType(%s).initB returning zeroes"(this);
		return new void[sizeOf]; // TODO: implement initB
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
			case Type.Char:
				return ret ~ "$char";
			case Type.Bool:
				return ret ~ "bool";
			case Type.Slice:
				return (*refT).toString.format!"$slice(%s)"; // cannot be const
			case Type.Array:
				return (*refT).toString.format!"$array(%s)"; // cannot be const
			case Type.Fn:
				return format!"fn(%s)->%s"(
						paramT.map!(p => p.toString).join(","), retT.toString);
			case Type.Ref:
				return ret ~ (*refT).toString.format!"@%s";
			case Type.Struct:
				if (structS is null)
					return "struct{}";
				return structS.ident.format!"struct(%s)";
			case Type.Union:
				if (unionS is null)
					return "union{}";
				return unionS.ident.format!"union(%s)";
			case Type.Enum:
				if (enumS is null)
					return "enum{}";
				return enumS.ident.format!"enum(%s)";
			case Type.NoInit:
				return "$noinit";
		}
		assert(false);
	}

	/// Returns: byte size of type
	@property size_t sizeOf() const pure {
		final switch (type){
			case Type.Seq:
				return seqT.fold!((size_t a, const ADataType e) => a + e.sizeOf)
					(size_t.init);
			case Type.IntX, Type.UIntX, Type.FloatX:
				return x / 8;
			case Type.Char:
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
				if (structS is null)
					return 0;
				return structS.sizeOf;
				//assert (false, "thou shall not call ADataType.sizeOf on Struct!");
			case Type.Union:
				assert (unionS !is null);
				return unionS.sizeOf;
				//assert (false, "thou shall not call ADataType.sizeOf on Union!");
			case Type.Enum:
				assert(enumS !is null);
				return enumS.type.sizeOf;
				//assert (false, "thou shall not call ADataType.sizeOf on Enum!");
			case Type.NoInit:
				return 0;
		}
	}

	ADataType copy() const pure {
		ADataType ret;
		ret.type = type;
		ret.isConst = isConst;
		final switch (type){
			case Type.Seq:
				ret.seqT = seqT.map!(t => t.copy).array;
				break;
			case Type.IntX:
			case Type.UIntX:
			case Type.FloatX:
				ret.x = x;
				break;
			case Type.Char:
			case Type.Bool:
			case Type.NoInit:
				break;
			case Type.Slice:
			case Type.Array:
			case Type.Ref:
				ret.refT = [(*refT).copy].ptr;
				break;
			case Type.Fn:
				ret.retT = [(*retT).copy].ptr;
				break;
			case Type.Struct:
				ret.structS = cast(AStruct*)structS;
				break;
			case Type.Union:
				ret.unionS = cast(AUnion*)unionS;
				break;
			case Type.Enum:
				ret.enumS = cast(AEnum*)enumS;
		}
		return ret;
	}

	/// Returns: ADataType equivalent of `T`
	static ADataType of(T...)() pure if (T.length && allSatisfy!(isType, T)) {
		static if (T.length == 1){
			static if (std.traits.isNumeric!(T[0])){
				static if (isFloatingPoint!(T[0]))
					return ADataType.ofFloat(T[0].sizeof * 8);
				static if (isUnsigned!(T[0]))
					return ADataType.ofUInt(T[0].sizeof * 8);
				return ADataType.ofInt(T[0].sizeof * 8);
			} else
			static if (is (T[0] == char)){
				return ADataType.ofChar;
			} else
			static if (isBoolean!(T[0])){
				return ADataType.ofBool;
			} else
			static if (isPointer!(T[0])){
				return ADataType.ofRef(ADataType.of!(PointerTarget!(T[0])));
			} else
			static if (is (T[0] == string)){
				return ADataType.ofString;
			} else
			static if (isStaticArray!(T[0])){
				return ADataType.ofSlice(ADataType.of!(ElementType!(T[0])));
			} else
			static if (isArray!(T[0])){
				return ADataType.ofArray(ADataType.of!(ElementType!(T[0])));
			} else
			static if (isFunction!(T[0]) || isFunctionPointer!(T[0])){
				static assert (false, "ADataType.of(T) does not suppport Fn yet");
			} else
			static if (is (T[0] == struct)){
				static assert (false, "ADataType.of(T) does not suppport Struct yet");
			} else
			static if (is (T[0] == union)){
				static assert (false, "ADataType.of(T) does not suppport Union yet");
			} else
			static if (is (T[0] == enum)){
				static assert (false, "ADataType.of(T) does not suppport Enum yet");
			} else {
				static assert (false, "Unsupported data type for ADataType.of(T)");
			}
		} else {
			return ADataType.ofSeq([staticMap!(ADataType.of, T)]);
		}
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
	static ADataType ofChar() pure {
		ADataType ret;
		ret.type = Type.Char;
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
		return ret;
	}

	/// Returns: array type
	static ADataType ofArray(ADataType elemT) pure {
		ADataType ret;
		ret.type = Type.Array;
		ret.refT = [elemT].ptr;
		return ret;
	}

	static ADataType ofString() pure {
		ADataType str;
		str.type = Type.Slice;
		str.refT = [ADataType.ofChar.constOf].ptr;
		str.refT.isConst = true;
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
	static ADataType of(AStruct* structT) pure {
		ADataType ret;
		ret.type = Type.Struct;
		ret.structS = structT;
		return ret;
	}

	/// Returns: union type
	static ADataType of(AUnion* unionT) pure {
		ADataType ret;
		ret.type = Type.Union;
		ret.unionS = unionT;
		return ret;
	}

	/// Returns: enum type
	static ADataType of(AEnum* enumT) pure {
		ADataType ret;
		ret.type = Type.Enum;
		ret.enumS = enumT;
		return ret;
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
				return x == rhs.x;
			case Type.Char:
			case Type.Bool:
				return true;
			case Type.Slice:
			case Type.Array:
			case Type.Ref:
				return *refT == *rhs.refT;
			case Type.Fn:
				if (retT != rhs.retT || paramT.length != rhs.paramT.length)
					return false;
				foreach (size_t i; paramT.length.iota){
					if (paramT[i] != rhs.paramT[i])
						return false;
				}
				return true;
			case Type.Struct:
				return structS !is null && structS == rhs.structS;
			case Type.Union:
				return unionS !is null && unionS == rhs.unionS;
			case Type.Enum:
				return enumS !is null && enumS == rhs.enumS;
			case Type.NoInit:
				return true;
		}
		return true;
	}
}

/// Alis struct
public struct AStruct{
	/// identifier, `ident.isNoId == true` if anonymous
	IdentU[] ident;
	/// data types for each field
	ADataType[] types;
	/// initialisation data for each field
	void[][] initD;
	/// maps member names to indexes. Many to One
	size_t[string] names;
	/// visibility for each name
	Visibility[string] nameVis;
	/// Visibility of struct
	Visibility vis;

	/// if this is unique
	@property bool isUnique() const pure {
		return ident.length && ident[$ - 1].ident.canFind('$');
	}
	/// Whether a member exists and is accessible
	bool exists(string name, IdentU[] ctx = [IdentU.init]) const pure {
		if (name !in names)
			return false;
		if (!isUnique)
			return true;
		immutable auto len = cast(ptrdiff_t)ident.length - 1;
		if (ctx.length >= len && ident[0 .. len] == ctx[0 .. len])
			return true;
		if (const Visibility* vis = name in nameVis)
			return *vis == Visibility.Pub || *vis == Visibility.IPub;
		return true;
	}
	/// whether the 0th member is aliased to `this`
	@property bool hasBase(IdentU[] ctx = [IdentU.init]) const pure {
		return exists("this", ctx);
	}
	/// Returns: size of this struct
	@property size_t sizeOf() const pure {
		return types.map!(t => t.sizeOf).sum;
	}

	string toString() const pure {
		return format!"struct %s{%(%r,%)}"(ident,
				types.length.iota.map!(i => types[i].format!"%s[%(%r,%)]=%s"(
						names.byKey.filter!(n => names[n] == i)
						.map!(n => (nameVis[n] == Visibility.Default ? ""
							: nameVis[n] == Visibility.Pub ? "pub "
							: nameVis[n] == Visibility.IPub ? "ipub " : "idk ")
							.format!"%s%s"(n)).array, cast(ubyte[])initD[i]
						)));
	}
}

/// Alis union
public struct AUnion{
	/// identifier, `_` if anonymous, which also implies not unique
	IdentU[] ident;
	/// types of members
	ADataType[] types;
	/// maps field names to indexes in `types`. can be null, if unnamed
	size_t[string] names;
	/// name's visibility. can be null, if unnamed
	Visibility[string] nameVis;
	/// initialisation type's index
	size_t initI;
	/// initialisation data
	void[] initD;
	/// Visibility outside its parent module
	Visibility vis;

	/// Whether a member exists and is accessible
	bool exists(string name, IdentU[] ctx = [IdentU.init]) const pure {
		if (name !in names)
			return false;
		if (!isUnique)
			return true;
		immutable auto len = cast(ptrdiff_t)ident.length - 1;
		if (ctx.length >= len && ident[0 .. len] == ctx[0 .. len])
			return true;
		if (const Visibility* vis = name in nameVis)
			return *vis == Visibility.Pub || *vis == Visibility.IPub;
		return true;
	}
	/// whether the 0th member is aliased to `this`
	@property bool hasBase(IdentU[] ctx = [IdentU.init]) const pure {
		return exists("this", ctx);
	}
	/// if this is unique
	@property bool isUnique() const pure {
		return ident.length && ident[$ - 1].ident.canFind('$');
	}
	/// Returns: true if this is an unnamed union
	@property bool isUnnamed() const pure {
		return names.length == 0;
	}
	/// Returns: size of this union
	@property size_t sizeOf() const pure {
		return types.map!(t => t.sizeOf).fold!((a, b) => max(a, b)) + size_t.sizeof;
	}

	string toString() const pure {
		return format!"union %s{%(%r,%)}"(ident,
				types.length.iota.map!(i => types[i].format!"%s[%(%r,%)]%s"(
						names.byKey.filter!(n => names[n] == i)
						.map!(n => (nameVis[n] == Visibility.Default ? ""
							: nameVis[n] == Visibility.Pub ? "pub "
							: nameVis[n] == Visibility.IPub ? "ipub " : "idk ")
							.format!"%s%s"(n)).array,
							initI == i ? (cast(ubyte[])initD).format!"=%s" : ""
						)));

	}
}

/// Alis Enum
public struct AEnum{
	/// identifier
	IdentU[] ident;
	/// Data Type. This will be `struct{}` in case of empty emum
	ADataType type;
	/// member identifiers
	string[] memId;
	/// member values
	void[][] memVal;
	/// Visibility outside its parent module
	Visibility vis;

	string toString() const pure {
		return format!"enum %s:%s{%(%r,%)}"(ident, type,
				memId.length.iota
				.map!(i => format!"%s=%s"(memId[i], cast(ubyte[])memVal[i])));
	}
}

/// Alis Enum Constant
public struct AEnumConst{
	/// identifier
	IdentU[] ident;
	/// type
	ADataType type;
	/// value bytes
	void[] data;
	/// Visibility outside its parent module
	Visibility vis;

	string toString() const pure {
		return format!"enum %s:%s=%s;"(ident, type, cast(ubyte[])data);
	}
}

/// Alis Function Information
public struct AFn{
	/// identifier, `_` if anonymous
	IdentU[] ident;
	/// return type
	ADataType retT;
	/// parameter names
	string[] paramsN;
	/// parameter types
	ADataType[] paramsT;
	/// parameter default values, if any
	void[][] paramsV;
	/// unique id. this is also the label name in ABC
	string uid;
	/// whether this is an alis function (true) or an external (false)
	bool isAlisFn = true;
	/// Visibility outside its parent module
	Visibility vis;

	string toString() const pure {
		return format!
			"fn %s%s[%s](%(%r%))->%s"(
					(isAlisFn ? "" : "external "),
					ident, uid,
					paramsN.length.iota
						.map!(i => format!"%s %s=%s"(paramsN[i], paramsT[i],
								cast(ubyte[])paramsV[i])),
					retT);
	}
}

/// Alis Variable
public struct AVar{
	/// identifier
	IdentU[] ident;
	/// data type
	ADataType type;
	/// initialisation data
	void[] initD;
	/// offset
	size_t offset;
	/// whether is global or local
	bool isGlobal = false;
	/// Visibility outside its parent module
	Visibility vis;
	/// unique id. TODO: set this
	string uid;

	string toString() const pure {
		return format!"var %s%s[%s]:%s=%s off=%d"(isGlobal ? "global" : null,
				ident, uid, type, cast(ubyte[])initD, offset);
	}
}

/// Alis Alias
public struct AAlias{
	/// identifier
	IdentU[] ident;
	/// Visibility outside its parent module
	Visibility vis;
	/// expr being aliased
	RExpr expr;

	string toString() const pure {
		return format!"alias %s%s=%s"(vis == Visibility.Pub ? "pub " : "",
				ident, "<EXPR>");
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

/// Alis Unit Test
public struct AUTest{
	/// identifier
	IdentU[] ident;
	/// Visibility (always default)
	@property Visibility vis(Visibility) const pure { return Visibility.Default; }
	/// ditto
	@property Visibility vis() const pure { return Visibility.Default; }
	/// unique id. this is also the label name in ABC
	string uid;

	string toString() const pure {
		return format!"utest %s[%s]"(ident, uid);
	}
}

/// Encodes function name, using function name and param types
/// Returns: encoded name
public string fnNameEncode(string name, ADataType[] args){
	return format!"%s$%(_%r%)_$"(name, args.map!(a => a.toString));
}
