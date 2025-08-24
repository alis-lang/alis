module alis.utils;

/// Union with array of bytes
public union ByteUnion(T, ubyte N = T.sizeof){
	T data;
	void[N] bytes;
	this(void[N] bytes){
		this.bytes = bytes;
	}
	this(void[] bytes){
		assert(bytes.length >= N);
		this.bytes = bytes[0 .. N];
	}
	this(T data){
		this.data = data;
	}
}

/// Reads a void[] as a type
/// Returns: value in type T
pragma(inline, true) public inout(T) as(T)(inout void[] data) pure {
	assert(data.length >= T.sizeof);
	return *(cast(T*)data.ptr);
}

/// Returns: void[] against a value of type T
pragma(inline, true) public void[] asBytes(T)(T val) pure {
	void[] ret;
	ret.length = T.sizeof;
	return ret[] = (cast(void*)&val)[0 .. T.sizeof];
}

///
unittest{
	assert((cast(ptrdiff_t)1025).asBytes.as!ptrdiff_t == 1025);
	assert("hello".asBytes.as!string == "hello");
	assert((cast(double)50.5).asBytes.as!double == 50.5);
	assert('a'.asBytes.as!char == 'a');
	assert(true.asBytes.as!bool == true);
}
