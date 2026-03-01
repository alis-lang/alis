module alis.runtime.vm;

import meta;

import std.traits : hasUDA, isIntegral, isSigned, isUnsigned, isFloatingPoint;
import std.meta : AliasSeq;
import std.format : format;

import navm : Inst, Code, parseCode, execute;

/// Instruction Set for the Alis VM
public alias InstructionSet = InstructionSet_Impl!();

/// Number of registers in the VM
public enum ubyte REG_COUNT = 8;

private template InstructionSet_Impl(){
	alias InstructionSet_Impl = AliasSeq!();
	static foreach (string name; __traits(allMembers, mixin(__MODULE__))){
		static if (hasUDA!(__traits(getMember, mixin(__MODULE__), name), Inst)){
			InstructionSet_Impl = AliasSeq!(InstructionSet_Impl,
					__traits(getMember, mixin(__MODULE__), name));
		}
	}
}

/// program stack
private struct Stack{
	void[] arr;
	void* seek;
	@disable this();
	this(void[] arr, void* seek = null){
		this.arr = arr;
		if (!this.arr.length)
			arr = new void[2048];
		if (seek is null)
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

/// Data type for a register
private alias reg_t = void[size_t.sizeof];

/// program state
private struct State{
	Stack stack;
	reg_t[REG_COUNT] r;
}

// register manipulation ------------------------------------------------------

pragma(inline, true) private void _ld(T)(ref State _state, ubyte r){
	static if (isIntegral!T){
		static if (isSigned!T){
			*cast(ptrdiff_t*)(_state.r[r].ptr) = _state.stack.pop!T;
		} else static if (isUnsigned!T){
			*cast(size_t*)(_state.r[r].ptr) = _state.stack.pop!T;
		}
	} else static if (isFloatingPoint!T){
		*cast(double*)(_state.r[r].ptr) = _state.stack.pop!T;
	}
}

@Inst("ldi8")
private void ldI8(ref State _state, ubyte r) => _ld!byte(_state, r);
@Inst("ldi16")
private void ldI16(ref State _state, ubyte r) => _ld!short(_state, r);
@Inst("ldi32")
private void ldI132(ref State _state, ubyte r) => _ld!int(_state, r);
static if (ptrdiff_t.sizeof > int.sizeof){
	@Inst("ldi64")
	private void ldI164(ref State _state, ubyte r) => _ld!ptrdiff_t(_state, r);
}
@Inst("ldu8")
private void ldU8(ref State _state, ubyte r) => _ld!ubyte(_state, r);
@Inst("ldu16")
private void ldU16(ref State _state, ubyte r) => _ld!ushort(_state, r);
@Inst("ldu32")
private void ldU32(ref State _state, ubyte r) => _ld!uint(_state, r);
static if (size_t.sizeof > uint.sizeof){
	@Inst("ldu64")
	private void ldU164(ref State _state, ubyte r) => _ld!size_t(_state, r);
}

@Inst("ldf32")
private void ldF32(ref State _state, ubyte r) => _ld!float(_state, r);
@Inst("ldf64")
private void ldF64(ref State _state, ubyte r) => _ld!double(_state, r);

@Inst("mov")
private void mov(ref State _state, ubyte src, ubyte dst){
	_state.r[dst] = _state.r[src];
}
@Inst("swp")
private void swp(ref State _state, ubyte a, ubyte b){
	reg_t tmp = _state.r[a];
	_state.r[a] = _state.r[b];
	_state.r[b] = tmp;
}

pragma(inline, true) private void _rd(T)(ref State _state, ubyte r){
	static if (isIntegral!T){
		static if (isSigned!T){
			_state.stack.push!T(cast(T)(*cast(ptrdiff_t*)(_state.r[r].ptr)));
		} else static if (isUnsigned!T){
			_state.stack.push!T(cast(T)(*cast(size_t*)(_state.r[r].ptr)));
		}
	} else static if (isFloatingPoint!T){
		_state.stack.push!T(cast(T)(*cast(double*)(_state.r[r].ptr)));
	}
}

@Inst("rdi8")
private void rdI8(ref State _state, ubyte r) => _rd!byte(_state, r);
@Inst("rdi16")
private void rdI16(ref State _state, ubyte r) => _rd!short(_state, r);
@Inst("rdi32")
private void rdI32(ref State _state, ubyte r) => _rd!int(_state, r);
static if (ptrdiff_t.sizeof > int.sizeof){
	@Inst("rdi64")
	private void rdI64(ref State _state, ubyte r) => _rd!ptrdiff_t(_state, r);
}
@Inst("rdu8")
private void rdU8(ref State _state, ubyte r) => _rd!ubyte(_state, r);
@Inst("rdu16")
private void rdU16(ref State _state, ubyte r) => _rd!ushort(_state, r);
@Inst("rdu32")
private void rdU32(ref State _state, ubyte r) => _rd!uint(_state, r);
static if (size_t.sizeof > uint.sizeof){
	@Inst("rdu64")
	private void rdU64(ref State _state, ubyte r) => _rd!size_t(_state, r);
}

@Inst("rdf32")
private void rdF32(ref State _state, ubyte r) => _rd!float(_state, r);
@Inst("rdf64")
private void rdF64(ref State _state, ubyte r) => _rd!double(_state, r);

// control flow ---------------------------------------------------------------

@Inst("jmp")
private void jmp(ref size_t _ic, size_t label){
	_ic = label;
}
@Inst("cjmp")
private void cJmp(ref State _state, ref size_t _ic, size_t label){
	if (*cast(size_t*)(_state.r[0].ptr))
		_ic = label;
}

// aithmetic ------------------------------------------------------------------

pragma(inline, true) private void _add(T)(ref State _state, ubyte a, ubyte b){
	*cast(T*)(_state.r[0].ptr) =
		*cast(T*)(_state.r[a].ptr) + *cast(T*)(_state.r[b].ptr);
}
pragma(inline, true) private void _sub(T)(ref State _state, ubyte a, ubyte b){
	*cast(T*)(_state.r[0].ptr) =
		*cast(T*)(_state.r[a].ptr) - *cast(T*)(_state.r[b].ptr);
}
pragma(inline, true) private void _mul(T)(ref State _state, ubyte a, ubyte b){
	*cast(T*)(_state.r[0].ptr) =
		*cast(T*)(_state.r[a].ptr) * *cast(T*)(_state.r[b].ptr);
}
pragma(inline, true) private void _div(T)(ref State _state, ubyte a, ubyte b){
	*cast(T*)(_state.r[0].ptr) =
		*cast(T*)(_state.r[a].ptr) / *cast(T*)(_state.r[b].ptr);
}
pragma(inline, true) private void _mod(T)(ref State _state, ubyte a, ubyte b){
	*cast(T*)(_state.r[0].ptr) =
		*cast(T*)(_state.r[a].ptr) % *cast(T*)(_state.r[b].ptr);
}

@Inst("addi")
private void addI(ref State _state, ubyte a, ubyte b)
	=> _add!ptrdiff_t(_state, a, b);
@Inst("subi")
private void subI(ref State _state, ubyte a, ubyte b)
	=> _sub!ptrdiff_t(_state, a, b);
@Inst("muli")
private void mulI(ref State _state, ubyte a, ubyte b)
	=> _mul!ptrdiff_t(_state, a, b);
@Inst("divi")
private void divI(ref State _state, ubyte a, ubyte b)
	=> _div!ptrdiff_t(_state, a, b);
@Inst("modi")
private void modI(ref State _state, ubyte a, ubyte b)
	=> _mod!ptrdiff_t(_state, a, b);

@Inst("addu")
private void addU(ref State _state, ubyte a, ubyte b)
	=> _add!size_t(_state, a, b);
@Inst("subu")
private void subU(ref State _state, ubyte a, ubyte b)
	=> _sub!size_t(_state, a, b);
@Inst("mulu")
private void mulU(ref State _state, ubyte a, ubyte b)
	=> _mul!size_t(_state, a, b);
@Inst("divu")
private void divU(ref State _state, ubyte a, ubyte b)
	=> _div!size_t(_state, a, b);
@Inst("modu")
private void modU(ref State _state, ubyte a, ubyte b)
	=> _mod!size_t(_state, a, b);

@Inst("addf")
private void addF(ref State _state, ubyte a, ubyte b)
	=> _add!double(_state, a, b);
@Inst("subf")
private void subF(ref State _state, ubyte a, ubyte b)
	=> _sub!double(_state, a, b);
@Inst("mulf")
private void mulF(ref State _state, ubyte a, ubyte b)
	=> _mul!double(_state, a, b);
@Inst("divf")
private void divF(ref State _state, ubyte a, ubyte b)
	=> _div!double(_state, a, b);

// conversion -----------------------------------------------------------------

@Inst("itof")
private void iToF(ref State _state, ubyte r){
	*cast(double*)(_state.r[0].ptr) =
		cast(double)(*cast(ptrdiff_t*)(_state.r[r].ptr));
}
@Inst("utof")
private void uToF(ref State _state, ubyte r){
	*cast(double*)(_state.r[0].ptr) =
		cast(double)(*cast(size_t*)(_state.r[r].ptr));
}
@Inst("ftoi")
private void fToI(ref State _state, ubyte r){
	*cast(ptrdiff_t*)(_state.r[0].ptr) =
		cast(ptrdiff_t)(*cast(double*)(_state.r[r].ptr));
}
@Inst("ftou")
private void fToU(ref State _state, ubyte r){
	*cast(size_t*)(_state.r[0].ptr) =
		cast(size_t)(*cast(double*)(_state.r[r].ptr));
}

// bitwise --------------------------------------------------------------------

@Inst("and")
private void and(ref State _state, ubyte a, ubyte b){
	*cast(size_t*)(_state.r[0].ptr) =
		*cast(size_t*)(_state.r[a].ptr) & *cast(size_t*)(_state.r[b].ptr);
}
@Inst("or")
private void or(ref State _state, ubyte a, ubyte b){
	*cast(size_t*)(_state.r[0].ptr) =
		*cast(size_t*)(_state.r[a].ptr) | *cast(size_t*)(_state.r[b].ptr);
}
@Inst("xor")
private void xor(ref State _state, ubyte a, ubyte b){
	*cast(size_t*)(_state.r[0].ptr) =
		*cast(size_t*)(_state.r[a].ptr) ^ *cast(size_t*)(_state.r[b].ptr);
}
@Inst("not")
private void xor(ref State _state, ubyte r){
	*cast(size_t*)(_state.r[0].ptr) = ~(*cast(size_t*)(_state.r[r].ptr));
}

// comparison -----------------------------------------------------------------

pragma(inline, true) private void _ls(T)(ref State _state, ubyte a, ubyte b){
	*cast(size_t*)(_state.r[0].ptr) =
		1 * (*cast(size_t*)(_state.r[a].ptr) < *cast(size_t*)(_state.r[b].ptr));
}

@Inst("lsi")
private void lsI(ref State _state, ubyte a, ubyte b)
	=> _ls!ptrdiff_t(_state, a, b);
@Inst("lsu")
private void lsU(ref State _state, ubyte a, ubyte b)
	=> _ls!size_t(_state, a, b);
@Inst("lsf")
private void lsF(ref State _state, ubyte a, ubyte b)
	=> _ls!double(_state, a, b);

@Inst("cmp")
private void cmp(ref State _state, ubyte a, ubyte b){
	*cast(size_t*)(_state.r[0].ptr) =
		1 * (*cast(size_t*)(_state.r[a].ptr) == *cast(size_t*)(_state.r[b].ptr));
}

pragma(msg, InstructionSet.length.format!"instructions count: %d");
