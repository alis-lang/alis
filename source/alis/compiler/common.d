/++
Common compiler components
+/
module alis.compiler.common;

import std.conv,
			 std.format;

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
public char[] strUnescape(string str){
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
public char charUnescape(char c){
	switch (c){
		case 't':	return '\t';
		case 'n':	return '\n';
		case 'b':	return '\b';
		default:	return c;
	}
}
