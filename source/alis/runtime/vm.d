module alis.runtime.vm;

import meta;

import std.traits : hasUDA, isIntegral, isSigned, isUnsigned, isFloatingPoint;
import std.meta : AliasSeq;
import std.format : format;

import navm : Inst, Code, parseCode, execute, toBin, fromBin;

/// Instruction Set for the Alis VM
public alias InstructionSet = InstructionSet_Impl!();

/// Number of registers in the VM
public enum ubyte REG_COUNT = 8;

/// Minimum stack size
public enum size_t STACK_SIZE_MIN = 2048;

private template InstructionSet_Impl(){
	alias InstructionSet_Impl = AliasSeq!();
	static foreach (string name; __traits(allMembers, mixin(__MODULE__))){
		static if (hasUDA!(__traits(getMember, mixin(__MODULE__), name), Inst)){
			InstructionSet_Impl = AliasSeq!(InstructionSet_Impl,
					__traits(getMember, mixin(__MODULE__), name));
		}
	}
}

/// Data type for a register
private alias reg_t = void[size_t.sizeof];

/// program state
private struct State{
	State* prev;
	reg_t[REG_COUNT] r;
	void[] stack;
	void* seek;
	@disable this();
	this(State* prev, reg_t[REG_COUNT] r, void[] stack, void* seek){
		this.prev = prev;
		this.r = r;
		this.stack = stack.length ? stack : new void[2048];
		this.seek = seek ? seek : this.stack.ptr;
	}

	/// Returns: value of type `T` at an offset from `base`
	pragma(inline, true) T get(T)(size_t offset){
		return *cast(T*)(stack.ptr + offset);
	}
	/// Sets value at an offset from `base`
	pragma(inline, true) void set(T)(size_t offset, T val){
		*cast(T*)(stack.ptr + offset) = val;
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

// register manipulation ------------------------------------------------------

pragma(inline, true) private void _ld(ubyte N)(ref State _state, ubyte r){
	*cast(void[N]*)(_state.r[r].ptr) = _state.pop!(void[N]);
}

@Inst("ld8")
private void ld8(ref State _state, ubyte r) => _ld!8(_state, r);
@Inst("ld16")
private void ld16(ref State _state, ubyte r) => _ld!16(_state, r);
@Inst("ld32")
private void ld32(ref State _state, ubyte r) => _ld!32(_state, r);
@Inst("ld64")
private void ld64(ref State _state, ubyte r) => _ld!64(_state, r);

pragma(inline, true) private void _in(T)(ref State _state, ubyte r){
	static if (isIntegral!T){
		static if (isSigned!T){
			*cast(ptrdiff_t*)(_state.r[r].ptr) = *cast(T*)(_state.r[r].ptr);
		} else static if (isUnsigned!T){
			*cast(size_t*)(_state.r[r].ptr) = *cast(T*)(_state.r[r].ptr);
		}
	} else static if (isFloatingPoint!T){
		*cast(double*)(_state.r[r].ptr) = *cast(T*)(_state.r[r].ptr);
	}
}

@Inst("ii8")
private void iI8(ref State _state, ubyte r) => _in!byte(_state, r);
@Inst("ii16")
private void iI16(ref State _state, ubyte r) => _in!short(_state, r);
@Inst("ii32")
private void iI132(ref State _state, ubyte r) => _in!int(_state, r);
static if (ptrdiff_t.sizeof > int.sizeof){
	@Inst("ii64")
	private void iI164(ref State _state, ubyte r) => _in!ptrdiff_t(_state, r);
}
@Inst("iu8")
private void iU8(ref State _state, ubyte r) => _in!ubyte(_state, r);
@Inst("iu16")
private void iU16(ref State _state, ubyte r) => _in!ushort(_state, r);
@Inst("iu32")
private void iU32(ref State _state, ubyte r) => _in!uint(_state, r);
static if (size_t.sizeof > uint.sizeof){
	@Inst("iu64")
	private void iU164(ref State _state, ubyte r) => _in!size_t(_state, r);
}

@Inst("if32")
private void iF32(ref State _state, ubyte r) => _in!float(_state, r);
@Inst("if64")
private void iF64(ref State _state, ubyte r) => _in!double(_state, r);

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

pragma(inline, true) private void _rd(ubyte N)(ref State _state, ubyte r){
	_state.push!(void[N])(*cast(void[N]*)(_state.r[r].ptr));
}

@Inst("rd8")
private void rd8(ref State _state, ubyte r) => _rd!8(_state, r);
@Inst("rd16")
private void rd16(ref State _state, ubyte r) => _rd!16(_state, r);
@Inst("rd32")
private void rd32(ref State _state, ubyte r) => _rd!32(_state, r);
@Inst("rd64")
private void rd64(ref State _state, ubyte r) => _rd!64(_state, r);

pragma(inline, true) private void _out(T)(ref State _state, ubyte r){
	static if (isIntegral!T){
		static if (isSigned!T){
			*cast(T*)(_state.r[r].ptr) = cast(T)*cast(ptrdiff_t*)(_state.r[r].ptr);
		} else static if (isUnsigned!T){
			*cast(T*)(_state.r[r].ptr) = cast(T)*cast(size_t*)(_state.r[r].ptr);
		}
	} else static if (isFloatingPoint!T){
		*cast(T*)(_state.r[r].ptr) = cast(T)*cast(double*)(_state.r[r].ptr);
	}
}

@Inst("oi8")
private void oI8(ref State _state, ubyte r) => _out!byte(_state, r);
@Inst("oi16")
private void oI16(ref State _state, ubyte r) => _out!short(_state, r);
@Inst("oi32")
private void oI32(ref State _state, ubyte r) => _out!int(_state, r);
static if (ptrdiff_t.sizeof > int.sizeof){
	@Inst("oi64")
	private void oI64(ref State _state, ubyte r) => _out!ptrdiff_t(_state, r);
}
@Inst("ou8")
private void oU8(ref State _state, ubyte r) => _out!ubyte(_state, r);
@Inst("ou16")
private void oU16(ref State _state, ubyte r) => _out!ushort(_state, r);
@Inst("ou32")
private void oU32(ref State _state, ubyte r) => _out!uint(_state, r);
static if (size_t.sizeof > uint.sizeof){
	@Inst("ou64")
	private void oU64(ref State _state, ubyte r) => _out!size_t(_state, r);
}

@Inst("of32")
private void oF32(ref State _state, ubyte r) => _out!float(_state, r);
@Inst("of64")
private void oF64(ref State _state, ubyte r) => _out!double(_state, r);

// stack manipulation ---------------------------------------------------------

pragma(inline, true) private void _dup(ubyte N)(ref State _state){
	_state.push(_state.top!(void[N]));
}

@Inst("dup8")
private void dup8(ref State _state) => _dup!8(_state);
@Inst("dup16")
private void dup16(ref State _state) => _dup!16(_state);
@Inst("dup32")
private void dup32(ref State _state) => _dup!32(_state);
@Inst("dup64")
private void dup64(ref State _state) => _dup!64(_state);

pragma(inline, true) private void _get(ubyte N)(
		ref State _state, ubyte r, size_t off){
	*cast(void[N]*)(_state.r[r].ptr) = _state.get!(void[N])(off);
}

@Inst("get8")
private void get8(ref State _state, ubyte r, size_t off)
	=> _get!8(_state, r, off);
@Inst("get16")
private void get16(ref State _state, ubyte r, size_t off)
	=> _get!16(_state, r, off);
@Inst("get32")
private void get32(ref State _state, ubyte r, size_t off)
	=> _get!32(_state, r, off);
@Inst("get64")
private void get64(ref State _state, ubyte r, size_t off)
	=> _get!64(_state, r, off);

pragma(inline, true) private void _set(ubyte N)(
		ref State _state, ubyte r, size_t off){
	_state.set(off, *cast(void[N]*)(_state.r[r].ptr));
}

@Inst("set8")
private void set8(ref State _state, ubyte r, size_t off)
	=> _set!8(_state, r, off);
@Inst("set16")
private void set16(ref State _state, ubyte r, size_t off)
	=> _set!16(_state, r, off);
@Inst("set32")
private void set32(ref State _state, ubyte r, size_t off)
	=> _set!32(_state, r, off);
@Inst("set64")
private void set64(ref State _state, ubyte r, size_t off)
	=> _set!64(_state, r, off);

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

@Inst("itob")
private void iToB(ref State _state, ubyte r){
	*cast(size_t*)(_state.r[0].ptr) =
		(*cast(ptrdiff_t*)(_state.r[r].ptr)) != 0;
}
@Inst("ftob")
private void fToB(ref State _state, ubyte r){
	*cast(size_t*)(_state.r[0].ptr) =
		(*cast(double*)(_state.r[r].ptr)) != 0f;
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
private void not(ref State _state, ubyte r){
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

debug pragma(msg, InstructionSet.length.format!"instructions count: %d");
