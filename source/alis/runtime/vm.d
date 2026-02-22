module alis.runtime.vm;

import meta;

import std.traits : hasUDA;
import std.meta : AliasSeq;
import std.format : format;

import navm : Inst;

/// Instruction Set for the Alis VM
public alias InstructionSet = InstructionSet_Impl!();

private template InstructionSet_Impl(){
	alias InstructionSet_Impl = AliasSeq!();
	static foreach (string name; __traits(allMembers, mixin(__MODULE__))){
		pragma(msg, name);
		static if (hasUDA!(__traits(getMember, mixin(__MODULE__), name), Inst)){
			InstructionSet_Impl = AliasSeq!(InstructionSet_Impl,
					__traits(getMember, mixin(__MODULE__), name));
		}
	}
}

private struct Stack{
	void[] arr;
	void* seek;
	@disable this();
	this(void[] arr){
		this.arr = arr;
		if (!this.arr.length)
			arr = new void[2048];
		seek = arr.ptr;
	}

	/// Returns: top element of type `T`
	pragma(inline, true) T top(T)(){
		return *(cast(T*)(seek - T.sizeof));
	}
	/// pushes `val` of type `T` onto stack
	pragma(inline, true) void push(T)(T val){
		*(cast(T*)seek) = val;
		seek += T.sizeof;
	}
	/// pops value of type `T`
	/// Returns: popped value
	pragma(inline, true) T pop(T)(){
		seek -= T.sizeof;
		return *(cast(T*)seek);
	}
	/// pops value of size `size` from stack
	pragma(inline, true) void pop(size_t size){
		seek -= size;
	}
}

private struct State{
	Stack stack;
}

pragma(inline, true) private void _addX(T)(State _state){
	T b = _state.stack.pop!T;
	T a = _state.stack.pop!T;
	_state.stack.push!T(cast(T)(a + b));
}
pragma(inline, true) private void _subX(T)(State _state){
	T b = _state.stack.pop!T;
	T a = _state.stack.pop!T;
	_state.stack.push!T(cast(T)(a - b));
}
pragma(inline, true) private void _mulX(T)(State _state){
	T b = _state.stack.pop!T;
	T a = _state.stack.pop!T;
	_state.stack.push!T(cast(T)(a * b));
}
pragma(inline, true) private void _divX(T)(State _state){
	T b = _state.stack.pop!T;
	T a = _state.stack.pop!T;
	_state.stack.push!T(cast(T)(a / b));
}
pragma(inline, true) private void _modX(T)(State _state){
	T b = _state.stack.pop!T;
	T a = _state.stack.pop!T;
	_state.stack.push!T(cast(T)(a % b));
}
pragma(inline, true) private void _bAndX(T)(State _state){
	T b = _state.stack.pop!T;
	T a = _state.stack.pop!T;
	_state.stack.push!T(cast(T)(a & b));
}
pragma(inline, true) private void _bOrX(T)(State _state){
	T b = _state.stack.pop!T;
	T a = _state.stack.pop!T;
	_state.stack.push!T(cast(T)(a | b));
}
pragma(inline, true) private void _bXorX(T)(State _state){
	T b = _state.stack.pop!T;
	T a = _state.stack.pop!T;
	_state.stack.push!T(cast(T)(a ^ b));
}
pragma(inline, true) private void _bNotX(T)(State _state){
	T a = _state.stack.pop!T;
	_state.stack.push!T(cast(T)(~a));
}

@Inst("addI8")
private void addI8(State _state) => _addX!byte(_state);
@Inst("addI16")
private void addI16(State _state) => _addX!short(_state);
@Inst("addI32")
private void addI32(State _state) => _addX!int(_state);
static if (ptrdiff_t.sizeof > int.sizeof){
	@Inst("addI64")
	private void addI64(State _state) => _addX!ptrdiff_t(_state);
}
@Inst("addU8")
private void addU8(State _state) => _addX!ubyte(_state);
@Inst("addU16")
private void addU16(State _state) => _addX!ushort(_state);
@Inst("addU32")
private void addU32(State _state) => _addX!uint(_state);
static if (size_t.sizeof > uint.sizeof){
	@Inst("addU64")
	private void addU64(State _state) => _addX!size_t(_state);
}
@Inst("addF32")
private void addF32(State _state) => _addX!float(_state);
@Inst("addF64")
private void addF64(State _state) => _addX!double(_state);

@Inst("subI8")
private void subI8(State _state) => _subX!byte(_state);
@Inst("subI16")
private void subI16(State _state) => _subX!short(_state);
@Inst("subI32")
private void subI32(State _state) => _subX!int(_state);
static if (ptrdiff_t.sizeof > int.sizeof){
	@Inst("subI64")
	private void subI64(State _state) => _subX!ptrdiff_t(_state);
}
@Inst("subU8")
private void subU8(State _state) => _subX!ubyte(_state);
@Inst("subU16")
private void subU16(State _state) => _subX!ushort(_state);
@Inst("subU32")
private void subU32(State _state) => _subX!uint(_state);
static if (size_t.sizeof > uint.sizeof){
	@Inst("subU64")
	private void subU64(State _state) => _subX!size_t(_state);
}
@Inst("subF32")
private void subF32(State _state) => _subX!float(_state);
@Inst("subF64")
private void subF64(State _state) => _subX!double(_state);

@Inst("mulI8")
private void mulI8(State _state) => _mulX!byte(_state);
@Inst("mulI16")
private void mulI16(State _state) => _mulX!short(_state);
@Inst("mulI32")
private void mulI32(State _state) => _mulX!int(_state);
static if (ptrdiff_t.sizeof > int.sizeof){
	@Inst("mulI64")
	private void mulI64(State _state) => _mulX!ptrdiff_t(_state);
}
@Inst("mulU8")
private void mulU8(State _state) => _mulX!ubyte(_state);
@Inst("mulU16")
private void mulU16(State _state) => _mulX!ushort(_state);
@Inst("mulU32")
private void mulU32(State _state) => _mulX!uint(_state);
static if (size_t.sizeof > uint.sizeof){
	@Inst("mulU64")
	private void mulU64(State _state) => _mulX!size_t(_state);
}
@Inst("mulF32")
private void mulF32(State _state) => _mulX!float(_state);
@Inst("mulF64")
private void mulF64(State _state) => _mulX!double(_state);

@Inst("divI8")
private void divI8(State _state) => _divX!byte(_state);
@Inst("divI16")
private void divI16(State _state) => _divX!short(_state);
@Inst("divI32")
private void divI32(State _state) => _divX!int(_state);
static if (ptrdiff_t.sizeof > int.sizeof){
	@Inst("divI64")
	private void divI64(State _state) => _divX!ptrdiff_t(_state);
}
@Inst("divU8")
private void divU8(State _state) => _divX!ubyte(_state);
@Inst("divU16")
private void divU16(State _state) => _divX!ushort(_state);
@Inst("divU32")
private void divU32(State _state) => _divX!uint(_state);
static if (size_t.sizeof > uint.sizeof){
	@Inst("divU64")
	private void divU64(State _state) => _divX!size_t(_state);
}
@Inst("divF32")
private void divF32(State _state) => _divX!float(_state);
@Inst("divF64")
private void divF64(State _state) => _divX!double(_state);

@Inst("modI8")
private void modI8(State _state) => _modX!byte(_state);
@Inst("modI16")
private void modI16(State _state) => _modX!short(_state);
@Inst("modI32")
private void modI32(State _state) => _modX!int(_state);
static if (ptrdiff_t.sizeof > int.sizeof){
	@Inst("modI64")
	private void modI64(State _state) => _modX!ptrdiff_t(_state);
}
@Inst("modU8")
private void modU8(State _state) => _modX!ubyte(_state);
@Inst("modU16")
private void modU16(State _state) => _modX!ushort(_state);
@Inst("modU32")
private void modU32(State _state) => _modX!uint(_state);
static if (size_t.sizeof > uint.sizeof){
	@Inst("modU64")
	private void modU64(State _state) => _modX!size_t(_state);
}

@Inst("and8")
private void and8(State _state) => _bAndX!ubyte(_state);
@Inst("and16")
private void and16(State _state) => _bAndX!ushort(_state);
@Inst("and32")
private void and32(State _state) => _bAndX!uint(_state);
static if (size_t.sizeof > uint.sizeof){
	@Inst("and64")
	private void and64(State _state) => _bAndX!size_t(_state);
}

@Inst("or8")
private void or8(State _state) => _bOrX!ubyte(_state);
@Inst("or16")
private void or16(State _state) => _bOrX!ushort(_state);
@Inst("or32")
private void or32(State _state) => _bOrX!uint(_state);
static if (size_t.sizeof > uint.sizeof){
	@Inst("or64")
	private void or64(State _state) => _bOrX!size_t(_state);
}

@Inst("xor8")
private void xor8(State _state) => _bXorX!ubyte(_state);
@Inst("xor16")
private void xor16(State _state) => _bXorX!ushort(_state);
@Inst("xor32")
private void xor32(State _state) => _bXorX!uint(_state);
static if (size_t.sizeof > uint.sizeof){
	@Inst("xor64")
	private void xor64(State _state) => _bXorX!size_t(_state);
}

@Inst("not8")
private void not8(State _state) => _bNotX!ubyte(_state);
@Inst("not16")
private void not16(State _state) => _bNotX!ushort(_state);
@Inst("not32")
private void not32(State _state) => _bNotX!uint(_state);
static if (size_t.sizeof > uint.sizeof){
	@Inst("not64")
	private void not64(State _state) => _bNotX!size_t(_state);
}
