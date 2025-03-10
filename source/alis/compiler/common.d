/++
Common compiler components
+/
module alis.compiler.common;

import std.conv,
			 std.format;

/// ASTNode location
struct Location{
	size_t line;
	size_t col;
}

/// A possibly erroneous value.
public struct ErrVal(T, E){
	private bool _isErr;
	private union{
		T _val;
		E _err;
	}

	/// Returns: true if this is erroneous
	public @property bool isErr() const pure {
		return _isErr;
	}
	/// Returns: error
	public @property E err() pure {
		return _err;
	}
	/// Returns: value
	public @property T val() pure {
		return _val;
	}

	/// constructor
	this(E err){
		this._isErr = true;
		this._err = err;
	}
	/// ditto
	this(T val){
		this._isErr = false;
		this._val = val;
	}
}

/// unescapes a string. the string must be provided with surrounding quotes
/// stripped
///
/// Returns: unescaped string
package char[] strUnescape(string str){
	uint i, shift;
	char[] r;
	r.length = str.length;
	while (i + shift < str.length && i < r.length){
		if (str[i + shift] == '\\'){
			shift ++;
			if (i + shift < str.length){
				r[i] = charUnescape(str[i + shift]);
				r.length --;
			}
		} else
			r[i] = str[i + shift];
		i ++;
	}
	return r;
}
///
unittest{
	string s = "t\\\"bcd\\\"\\t\\\\";
	assert(strUnescape(s) == "t\"bcd\"\t\\", strUnescape(s));
}

/// Returns: unescaped character, for a character `c` when used as `\c`
package char charUnescape(char c){
	switch (c){
		case 't':	return '\t';
		case 'n':	return '\n';
		case 'b':	return '\b';
		default:	return c;
	}
}

/// Reads a ubyte[] as a type
/// Returns: value in type T
pragma(inline, true) package T as(T)(ubyte[] data) {
	assert(data.length >= T.sizeof);
	return *(cast(T*)data.ptr);
}

/// Returns: ubyte[] against a value of type T
pragma(inline, true) package ubyte[] asBytes(T)(T val) {
	ubyte[] ret;
	ret.length = T.sizeof;
	return ret[] = (cast(ubyte*)&val)[0 .. T.sizeof];
}
