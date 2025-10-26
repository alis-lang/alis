module alis.runtime.main;

version (vm){
	import std.stdio,
				 std.file,
				 std.datetime.stopwatch,
				 std.traits,
				 std.meta,
				 std.array,
				 std.algorithm,
				 std.conv : to;

	import core.stdc.stdlib;

	import utils.misc;

	import alis.runtime.vm;

	struct Stack{
		ubyte[4096] stack;
		ushort seek;
		ushort base;
		pragma(inline, true) T pop(T)() if (!isArray!T){
			assert(seek >= T.sizeof, "Cannot pop `" ~ T.stringof ~ "` " ~ " seek is "
					~ seek.to!string);
			seek -= T.sizeof;
			return *(cast(T*)(stack.ptr + seek));
		}
		pragma(inline, true) T top(T)() if (!isArray!T){
			assert(seek >= T.sizeof, "Cannot pop `" ~ T.stringof ~ "` " ~ " seek is "
					~ seek.to!string);
			return *(cast(T*)(stack.ptr + seek - T.sizeof));
		}
		pragma(inline, true) void push(T)(T val) if (!isArray!T){
			assert(seek + T.sizeof <= stack.length, "Cannot push `" ~ T.stringof ~
					"` seek is " ~ seek.to!string);
			stack[seek .. seek + T.sizeof] = (cast(ubyte*)&val)[0 .. T.sizeof];
			seek += T.sizeof;
		}
	}

	///
	unittest{
		Stack stack;
		stack.push(cast(ubyte)127);
		stack.push(cast(ubyte)128);
		stack.push(cast(ptrdiff_t)ptrdiff_t.max);
		assert(stack.pop!ptrdiff_t == ptrdiff_t.max);
		assert(stack.pop!ubyte == 128);
		assert(stack.pop!ubyte == 127);
	}

	static assert(float.sizeof == int.sizeof);

	// math instructions
	void addI1(ref Stack _state) {
		_state.push!ubyte(cast(ubyte)(_state.pop!ubyte + _state.pop!ubyte));
	}

	void addI2(ref Stack _state) {
		_state.push!ushort(cast(ushort)(_state.pop!ushort + _state.pop!ushort));
	}

	void addI4(ref Stack _state) {
		_state.push!int(_state.pop!int + _state.pop!int);
	}

	void addI8(ref Stack _state) {
		_state.push!long(_state.pop!long + _state.pop!long);
	}

	void subI1(ref Stack _state) {
		immutable ubyte a = _state.pop!ubyte;
		immutable ubyte b = _state.pop!ubyte;
		_state.push!ubyte(cast(ubyte)(a - b));
	}

	void subI2(ref Stack _state) {
		immutable ushort a = _state.pop!ushort;
		immutable ushort b = _state.pop!ushort;
		_state.push!ushort(cast(ushort)(a - b));
	}


	void subI4(ref Stack _state) {
		immutable int a = _state.pop!int;
		_state.push!int(a - _state.pop!int);
	}

	void subI8(ref Stack _state) {
		immutable long a = _state.pop!long;
		_state.push!long(a - _state.pop!long);
	}


	void mulI1(ref Stack _state) {
		immutable ubyte a = _state.pop!ubyte;
		immutable ubyte b = _state.pop!ubyte;
		_state.push!ubyte(cast(ubyte)(a * b));
	}

	void mulI2(ref Stack _state) {
		immutable ushort a = _state.pop!ushort;
		immutable ushort b = _state.pop!ushort;
		_state.push!ushort(cast(ushort)(a * b));
	}

	void mulI4(ref Stack _state) {
		_state.push!int(_state.pop!int * _state.pop!int);
	}

	void mulI8(ref Stack _state) {
		_state.push!long(_state.pop!long * _state.pop!long);
	}


	void divI1(ref Stack _state) {
		immutable ubyte a = _state.pop!ubyte;
		_state.push!ubyte(a / _state.pop!ubyte);
	}

	void divI2(ref Stack _state) {
		immutable ushort a = _state.pop!ushort;
		_state.push!ushort(a / _state.pop!ushort);
	}

	void divI4(ref Stack _state) {
		immutable int a = _state.pop!int;
		_state.push!int(a / _state.pop!int);
	}

	void divI8(ref Stack _state) {
		immutable long a = _state.pop!long;
		_state.push!long(a / _state.pop!long);
	}


	void modI1(ref Stack _state) {
		immutable ubyte a = _state.pop!ubyte;
		_state.push!ubyte(a % _state.pop!ubyte);
	}

	void modI2(ref Stack _state) {
		immutable ushort a = _state.pop!ushort;
		_state.push!ushort(a % _state.pop!ushort);
	}

	void modI4(ref Stack _state) {
		immutable int a = _state.pop!int;
		_state.push!int(a % _state.pop!int);
	}

	void modI8(ref Stack _state) {
		immutable long a = _state.pop!long;
		_state.push!long(a % _state.pop!long);
	}


	void addF4(ref Stack _state) {
		_state.push!float(_state.pop!float + _state.pop!float);
	}

	void addF8(ref Stack _state) {
		_state.push!double(_state.pop!double + _state.pop!double);
	}

	void subF4(ref Stack _state) {
		immutable float a = _state.pop!float;
		_state.push!float(a - _state.pop!float);
	}

	void subF8(ref Stack _state) {
		immutable double a = _state.pop!double;
		_state.push!double(a - _state.pop!double);
	}

	void mulF4(ref Stack _state) {
		_state.push!float(_state.pop!float * _state.pop!float);
	}

	void mulF8(ref Stack _state) {
		_state.push!double(_state.pop!double * _state.pop!double);
	}

	void divF4(ref Stack _state) {
		immutable float a = _state.pop!float;
		_state.push!float(a / _state.pop!float);
	}

	void divF8(ref Stack _state) {
		immutable double a = _state.pop!double;
		_state.push!double(a / _state.pop!double);
	}


	void cmpI1(ref Stack _state) {
		immutable ubyte a = _state.pop!ubyte;
		immutable ubyte b = _state.pop!ubyte;
		if (a == b) {
			_state.push!int(0);
		} else if (a < b) {
			_state.push!int(-1);
		} else {
			_state.push!int(1);
		}
	}

	void cmpI2(ref Stack _state) {
		immutable ushort a = _state.pop!ushort;
		immutable ushort b = _state.pop!ushort;
		if (a == b) {
			_state.push!int(0);
		} else if (a < b) {
			_state.push!int(-1);
		} else {
			_state.push!int(1);
		}
	}

	void cmpI4(ref Stack _state) {
		immutable int a = _state.pop!int;
		immutable int b = _state.pop!int;
		if (a == b) {
			_state.push!int(0);
		} else if (a < b) {
			_state.push!int(-1);
		} else {
			_state.push!int(1);
		}
	}

	void cmpI8(ref Stack _state) {
		immutable long a = _state.pop!long;
		immutable long b = _state.pop!long;
		if (a == b) {
			_state.push!int(0);
		} else if (a < b) {
			_state.push!int(-1);
		} else {
			_state.push!int(1);
		}
	}

	void cmpF4(ref Stack _state) {
		immutable float a = _state.pop!float;
		immutable float b = _state.pop!float;
		if (a == b) {
			_state.push!int(0);
		} else if (a < b) {
			_state.push!int(-1);
		} else {
			_state.push!int(1);
		}
	}

	void cmpF8(ref Stack _state) {
		immutable double a = _state.pop!double;
		immutable double b = _state.pop!double;
		if (a == b) {
			_state.push!int(0);
		} else if (a < b) {
			_state.push!int(-1);
		} else {
			_state.push!int(1);
		}
	}

	// boolean

	void notBI1(ref Stack _state) {
		_state.push!int(_state.pop!ubyte == 0);
	}

	void notBI2(ref Stack _state) {
		_state.push!int(_state.pop!ushort == 0);
	}

	void notBI4(ref Stack _state) {
		_state.push!int(_state.pop!int == 0);
	}

	void notBI8(ref Stack _state) {
		_state.push!int(_state.pop!long == 0);
	}


	void andBI1(ref Stack _state) {
		ubyte a = _state.pop!ubyte;
		ubyte b = _state.pop!ubyte;
		_state.push!int((a != 0 && b != 0) ? 1 : 0);
	}

	void andBI2(ref Stack _state) {
		ushort a = _state.pop!ushort;
		ushort b = _state.pop!ushort;
		_state.push!int((a != 0 && b != 0) ? 1 : 0);
	}

	void andBI4(ref Stack _state) {
		int a = _state.pop!int;
		int b = _state.pop!int;
		_state.push!int((a != 0 && b != 0) ? 1 : 0);
	}

	void andBI8(ref Stack _state) {
		long a = _state.pop!long;
		long b = _state.pop!long;
		_state.push!int((a != 0 && b != 0) ? 1 : 0);
	}

	void orBI1(ref Stack _state) {
		ubyte a = _state.pop!ubyte;
		ubyte b = _state.pop!ubyte;
		_state.push!int((a != 0 || b != 0) ? 1 : 0);
	}

	void orBI2(ref Stack _state) {
		ushort a = _state.pop!ushort;
		ushort b = _state.pop!ushort;
		_state.push!int((a != 0 || b != 0) ? 1 : 0);
	}

	void orBI4(ref Stack _state) {
		int a = _state.pop!int;
		int b = _state.pop!int;
		_state.push!int((a != 0 || b != 0) ? 1 : 0);
	}

	void orBI8(ref Stack _state) {
		long a = _state.pop!long;
		long b = _state.pop!long;
		_state.push!int((a != 0 || b != 0) ? 1 : 0);
	}

	// bitwise

	void notI1(ref Stack _state) {
		_state.push!ubyte(cast(ubyte)(~_state.pop!ubyte));
	}

	void notI2(ref Stack _state) {
		_state.push!ushort(cast(ushort)(~_state.pop!ushort));
	}


	void notI4(ref Stack _state) {
		_state.push!int(~_state.pop!int);
	}

	void notI8(ref Stack _state) {
		_state.push!long(~_state.pop!long);
	}

	void andI1(ref Stack _state) {
		ubyte a = _state.pop!ubyte;
		ubyte b = _state.pop!ubyte;
		_state.push!ubyte(a & b);
	}

	void andI2(ref Stack _state) {
		ushort a = _state.pop!ushort;
		ushort b = _state.pop!ushort;
		_state.push!ushort(a & b);
	}

	void andI4(ref Stack _state) {
		int a = _state.pop!int;
		int b = _state.pop!int;
		_state.push!int(a & b);
	}

	void andI8(ref Stack _state) {
		long a = _state.pop!long;
		long b = _state.pop!long;
		_state.push!long(a & b);
	}

	void orI1(ref Stack _state) {
		ubyte a = _state.pop!ubyte;
		ubyte b = _state.pop!ubyte;
		_state.push!ubyte(a | b);
	}

	void orI2(ref Stack _state) {
		ushort a = _state.pop!ushort;
		ushort b = _state.pop!ushort;
		_state.push!ushort(a | b);
	}

	void orI4(ref Stack _state) {
		int a = _state.pop!int;
		int b = _state.pop!int;
		_state.push!int(a | b);
	}

	void orI8(ref Stack _state) {
		long a = _state.pop!long;
		long b = _state.pop!long;
		_state.push!long(a | b);
	}

	// stack manipulation

	void pshI1(ref Stack _state, ubyte val) {
		_state.push!ubyte(cast(ubyte)val);
	}

	void pshI2(ref Stack _state, ushort val) {
		_state.push!ushort(cast(ushort)val);
	}

	void pshI4(ref Stack _state, int val) {
		_state.push!int(cast(int)val);
	}

	void pshI8(ref Stack _state, long val) {
		_state.push!long(cast(long)val);
	}

	void pshF4(ref Stack _state, float val) {
		_state.push!float(val);
	}

	void pshF8(ref Stack _state, double val) {
		_state.push!double(val);
	}

	void popN(ref Stack _state, int n){
		_state.seek -= n;
	}

	void seek(ref Stack _state) {
		_state.push!size_t(_state.top!size_t);
	}

	void off(ref Stack _state, int n){
		_state.base += n;
	}

	void off0(ref Stack _state){
		_state.base = 0;
	}

	void pshO(ref Stack _state){
		_state.push!int(_state.base);
	}

	void popO(ref Stack _state){
		_state.base = cast(ushort)_state.pop!int;
	}

	void get(ref Stack _state, int NBytes, int offset) {
		ubyte[] data = cast(ubyte[]) (_state.stack.ptr + _state.base + offset)[0 .. NBytes];
		foreach (ubyte b; data) {
			_state.push!ubyte(b);
		}
	}

	void getR(ref Stack _state){
		immutable int offset = _state.pop!int;
		_state.push!int(_state.base + offset);
	}

	void putI1(ref Stack _state, int offset) {
		*cast(ubyte*)(_state.stack.ptr + _state.base + offset) = _state.pop!ubyte;
	}

	void putI2(ref Stack _state, int offset) {
		*cast(ushort*)(_state.stack.ptr + _state.base + offset) = _state.pop!ushort;
	}

	void putI4(ref Stack _state, int offset) {
		*cast(int*)(_state.stack.ptr + _state.base + offset) = _state.pop!int;
	}

	void putI8(ref Stack _state, int offset) {
		*cast(long*)(_state.stack.ptr + _state.base + offset) = _state.pop!long;
	}

	void putF4(ref Stack _state, int offset) {
		*cast(float*)(_state.stack.ptr + _state.base + offset) = _state.pop!float;
	}

	void putF8(ref Stack _state, int offset) {
		*cast(double*)(_state.stack.ptr + _state.base + offset) = _state.pop!double;
	}

	void putR(ref Stack _state){
		immutable int val = _state.pop!int, addr = _state.pop!int;
		*cast(int*)(_state.stack.ptr + addr) = val;
	}

	void incAI1(ref Stack _state, int offset) {
		ubyte* ptr = cast(ubyte*)(_state.stack.ptr + _state.base + offset);
		*ptr += 1;
	}

	void incAI2(ref Stack _state, int offset) {
		ushort* ptr = cast(ushort*)(_state.stack.ptr + _state.base + offset);
		*ptr += 1;
	}

	void incAI4(ref Stack _state, int offset) {
		int* ptr = cast(int*)(_state.stack.ptr + _state.base + offset);
		*ptr += 1;
	}

	void incAI8(ref Stack _state, int offset) {
		long* ptr = cast(long*)(_state.stack.ptr + _state.base + offset);
		*ptr += 1;
	}

	void incAF4(ref Stack _state, int offset) {
		float* ptr = cast(float*)(_state.stack.ptr + _state.base + offset);
		*ptr += 1.0f;
	}

	void incAF8(ref Stack _state, int offset) {
		double* ptr = cast(double*)(_state.stack.ptr + _state.base + offset);
		*ptr += 1.0;
	}

	void incR(ref Stack _state){
		int* ptr = cast(int*)(_state.stack.ptr + _state.pop!int);
		*ptr = *ptr + 1;
	}

	// jumps

	void jmp(ref size_t _ic, ref ByteCode _code, uint label){
		_ic = label;
	}

	void jmpC(ref size_t _ic, ref ByteCode _code, ref Stack _state,
			uint label){
		if (_state.pop!int != 0)
			_ic = label;
	}

	void call(ref size_t _ic, ref ByteCode _code, ref Stack _state, uint label){
		_state.push!int(_state.base);
		_state.push!int(cast(int)_ic);
		_state.base = _state.seek;
		_ic = label;
	}

	void ret(ref size_t _ic, ref ByteCode _code, ref Stack _state){
		_ic = _state.pop!int;
		_state.base = cast(ushort)_state.pop!int;
	}

	void dbg(ref Stack _state){
		writefln!"base: %d\tseek: %d"(_state.base, _state.seek);
	}

	void printI1(ref Stack _state) {
		write(_state.pop!ubyte);
	}

	void printI2(ref Stack _state) {
		write(_state.pop!ushort);
	}

	void printI4(ref Stack _state) {
		write(_state.pop!int);
	}

	void printI8(ref Stack _state) {
		write(_state.pop!long);
	}

	void printF4(ref Stack _state) {
		write(_state.pop!float);
	}

	void printF8(ref Stack _state) {
		write(_state.pop!double);
	}


	void printS(string s){
		write(s);
	}

	alias InstructionSet = AliasSeq!(
			addI1, addI2, addI4, addI8,
			subI1, subI2, subI4, subI8,
			mulI1, mulI2, mulI4, mulI8,
			divI1, divI2, divI4, divI8,
			modI1, modI2, modI4, modI8,
			addF4, addF8,
			subF4, subF8,
			mulF4, mulF8,
			divF4, divF8,
			cmpI1, cmpI2, cmpI4, cmpI8, cmpF4, cmpF8,
			notBI1, notBI2, notBI4, notBI8,
			andBI1, andBI2, andBI4, andBI8,
			orBI1, orBI2, orBI4, orBI8,
			notI1, notI2, notI4, notI8,
			andI1, andI2, andI4, andI8,
			orI1, orI2, orI4, orI8,
			pshI1, pshI2, pshI4, pshI8,
			pshF4, pshF8,
			popN,
			seek,
			off, pshO, popO, off0, getR, putR,
			incAI1, incAI2, incAI4, incAI8, incAF4, incAF8,
			incR,
			get,
			putI1, putI2, putI4, putI8, putF4, putF8,
			jmp, jmpC, call, ret, dbg,
			printI1, printI2, printI4, printI8, printF4, printF8,
			printS
			);

	void main(string[] args){
		if (args.length < 2 || args.canFind("-h") || args.canFind("--help")){
			stderr.writefln!"Usage:\n\t%s COMMAND [execCount]"(args[0]);
			stderr.writefln!"valid COMMAND:\n\t%s\n\t%s\n\t%s"(
					"exec - execute text bytecode",
					"execbin - execute binary bytecode",
					"tobin - convert text bytecode to binary bytecode"
					);
			exit(1);
		}
		immutable string cmd = args[1];
		immutable size_t count = args.length > 2 && args[2].isNum
			? args[2].to!size_t : 1;

		ByteCode code;
		switch (cmd){
			case "exec", "tobin":
				code = parseByteCode!InstructionSet(stdin.byLineCopy.array);
				break;
			case "execbin":
				ubyte[8] magicpost;
				ubyte[] meta;
				code = (cast(ubyte[])(stdin
							.byLineCopy(KeepTerminator.yes)
							.fold!((a, e) => a ~ e)))
					.fromBin(magicpost, meta);
				stderr.writefln!"Loaded binary bytecode:";
				stderr.writefln!"\tMagic Postfix: %s\n\tMetadata: %s"(magicpost, meta);
				break;
			default:
				stderr.writefln!"Invalid command. See:\n\t%s --help"(args[0]);
		}

		if (cmd == "tobin"){
			ubyte[8] magicpost = [0, 1, 2, 3, 4, 5, 6, 7];
			ubyte[] meta = [128, 255];
			stderr.writefln!"Writing to binary with:";
			stderr.writefln!"\tMagic Postfix: %s\n\tMetadata: %s"(magicpost, meta);
			stdout.rawWrite(toBin(code, magicpost, meta));
			return;
		}

		exec(code, "start", count);
	}

	void exec(ByteCode code, string label, size_t count = 0){
		Stack state;
		immutable ptrdiff_t startIndex = code.labelNames.countUntil(label);
		if (startIndex == -1){
			writeln("label `start` not found");
			return;
		}

		size_t min = size_t.max ,max = 0 ,avg = 0;
		StopWatch sw = StopWatch(AutoStart.no);
		foreach (i; 0 .. count){
			state = Stack.init;
			sw.start;
			execute!(Stack, InstructionSet)(code, state, startIndex);
			sw.stop;
			immutable size_t currentTime = sw.peek.total!"msecs" - avg;
			min = currentTime < min ? currentTime : min;
			max = currentTime > max ? currentTime : max;
			avg = sw.peek.total!"msecs";
		}
		avg = sw.peek.total!"msecs" / count;

		writeln("\nexecuted ", count, " times:");
		writeln("min\tmax\tavg\ttotal");
		writeln(min, '\t', max, '\t', avg, '\t', sw.peek.total!"msecs");
	}
}
