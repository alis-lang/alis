module alis.utils;

/// Union with array of ubytes
public union ByteUnion(T, ubyte N = T.sizeof){
	T data;
	ubyte[N] bytes;
	this(ubyte[N] bytes){
		this.bytes = bytes;
	}
	this(ubyte[] bytes){
		assert(bytes.length >= N);
		this.bytes = bytes[0 .. N];
	}
	this(T data){
		this.data = data;
	}
}

/// Reads a ubyte[] as a type
/// Returns: value in type T
pragma(inline, true) public T as(T)(ubyte[] data) {
	assert(data.length >= T.sizeof);
	return *(cast(T*)data.ptr);
}

/// Returns: ubyte[] against a value of type T
pragma(inline, true) public ubyte[] asBytes(T)(T val) {
	ubyte[] ret;
	ret.length = T.sizeof;
	return ret[] = (cast(ubyte*)&val)[0 .. T.sizeof];
}

///
unittest{
	assert((cast(ptrdiff_t)1025).asBytes.as!ptrdiff_t == 1025);
	assert("hello".asBytes.as!string == "hello");
	assert((cast(double)50.5).asBytes.as!double == 50.5);
	assert('a'.asBytes.as!char == 'a');
	assert(true.asBytes.as!bool == true);
}
