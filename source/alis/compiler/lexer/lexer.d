/++
Lexer Base
+/
module alis.compiler.lexer.lexer;

import utils.ds;

import std.algorithm,
			 std.format,
			 std.traits,
			 std.conv,
			 std.meta;

debug import std.stdio;

/// A token
public struct Token(T) if (is (T == enum)){
	/// line number and column number
	uint line, col;
	/// token type(s)
	Flags!T type;
	/// token
	string token;
	alias token this;

	/// constructor
	this(Flags!T type, string token){
		this.type = type;
		this.token = token;
	}

	string toString() const {
		string ret = "{";
		static foreach (member; EnumMembers!T){
			if (type[member])
				ret ~= member.to!string ~ ", ";
		}
		ret ~= "`" ~ token ~ "`}";
		return ret;
	}

	/// == operator
	bool opBinary(string op : "==")(const Token rhs) const{
		return type == rhs.type && token == rhs.token;
	}
	/// ditto
	bool opBinary(string op : "==")(const T rhs) const{
		return type.get(rhs);
	}
	/// ditto
	bool opBinary(string op : "==")(const string rhs) const{
		return token == rhs;
	}
	/// != operator
	bool opBinary(string op : "!=")(const Token rhs) const{
		return type != rhs.type || token != rhs.token;
	}
	/// ditto
	bool opBinary(string op : "!=")(const T rhs) const{
		return !type.get(rhs);
	}
	/// ditto
	bool opBinary(string op : "!=")(const string rhs) const{
		return token != rhs;
	}
}

/// Match("..") for exact match
/// Match(&func, "ab") for custom function, where it only need to trigger
/// if string starts with a or b
public struct Match{
	private bool exactMatch;
	string matchStr;
	uint function(string) funcMatch;

	this (string match){
		this.exactMatch = true;
		this.matchStr = match;
	}

	this(uint function(string) match, string matchStr){
		this.funcMatch = match;
		this.exactMatch = false;
		this.matchStr = matchStr;
	}
}

/// Gets exact matching strings, or the stringof
/// Usually helpful for error reporting
public template MatcherDesc(alias TT){
	alias MatcherDesc = AliasSeq!();
	static foreach (Match m; getUDAs!(TT, Match)){
		static if (m.exactMatch)
			MatcherDesc = AliasSeq!(MatcherDesc, m.matchStr.format!"`%s`");
	}
	// TODO: fix this
	//static if (MatcherDesc.length == 0)
	//	MatcherDesc = AliasSeq!(TT.to!string);
}

/// Matches that have given initial character
/// Match alternates with Enum Member
template MatchesWithInitChar(T, char C){
	alias MatchesWithInitChar = AliasSeq!();
	static foreach (member; EnumMembers!T){
		static foreach (matcher; getUDAs!(member, Match)){
			static if (matcher.exactMatch){
				static if (matcher.matchStr.length && matcher.matchStr[0] == C)
					MatchesWithInitChar = AliasSeq!(MatchesWithInitChar, matcher,
							member);
			}else{
				static foreach (char ch; matcher.matchStr){
					static if (ch == C){
						MatchesWithInitChar = AliasSeq!(MatchesWithInitChar, matcher,
								member);
					}
				}
			}
		}
	}
}

private Token!T match(T)(string str){
	Flags!T matches;
	uint maxLen;
	Switch: switch (str[0]){
		static foreach (char C; char.min .. char.max){
			static if (MatchesWithInitChar!(T, C).length){
				case C:
					static foreach (i; 0 .. MatchesWithInitChar!(T, C).length / 2){{
						alias matcher = MatchesWithInitChar!(T, C)[i * 2];
						alias member = MatchesWithInitChar!(T, C)[i * 2 + 1];
						uint localMaxLen = 0;
						static if (matcher.exactMatch){
							if (str.length >= matcher.matchStr.length &&
									(str[0 .. matcher.matchStr.length] == matcher.matchStr)){
								localMaxLen = matcher.matchStr.length;
							}
						}else{
							localMaxLen = matcher.funcMatch(str);
						}
						if (localMaxLen > maxLen){
							maxLen = localMaxLen;
							matches.set(false);
							matches.set!member(true);
						}else if (maxLen && localMaxLen == maxLen){
							matches.set!member(true);
						}
					}}
					break Switch;
			}
		}
		default: break;
	}
	return Token!T(matches, str[0 .. maxLen]);
}

/// Fancy string exploder
public struct Lexer(T, T err, T eof) if (is (T == enum)){
private:
	string _source;
	uint _seek;
	uint _lineno;
	uint _lastNewlineIndex;

	Flags!T _ignore;
	Token!T _next;
	bool _empty;

	/// match a token
	Token!T _getToken(){
		Token!T ret = match!T(_source[_seek .. $]);
		ret.line = _lineno + 1;
		ret.col = _seek - _lastNewlineIndex;
		if (!ret.token.length){
			ret.token = _source[_seek .. min($, _seek + 5)];
			ret.type.set!err;
			// make it empty, so error doesnt repeat, instead it becomes eof next
			_empty = true;
			return ret;
		}

		// increment _lineno if needed
		foreach (i, c; _source[_seek .. _seek + ret.length]){
			if (c == '\n'){
				_lastNewlineIndex = cast(uint)i + _seek;
				_lineno ++;
			}
		}
		_seek += ret.length;
		return ret;
	}

	/// Parses next token.
	///
	/// Returns: Token
	void _parseNext(){
		while (_seek < _source.length){
			auto prev = _seek;
			_next = _getToken;
			if (!(_next.type & _ignore))
				break;
			if (prev == _seek){
				_empty = true;
			}
		}
		if (_next.type & _ignore)
			_empty = true;
		if (_empty){
			_next = Token!T(Flags!T(eof), null);
			_next.line = _lineno + 1;
			_next.col = _seek - _lastNewlineIndex;
		}
	}

public:
	@disable this();
	this (string source, Flags!T ignore = Flags!T.init){
		// HACK: beware, ugly code below. Blame Bill Gates for this
		version (Windows) {
			_source = source;
		} else {
			import std.array : replace;
			_source = replace(source, "\r", "");
		}
		this._ignore = ignore;
		_empty = _source.length == 0;
		_parseNext;
	}

	bool empty(){
		return _empty;
	}

	void popFront(){
		if (_seek >= _source.length)
			_empty = true;
		_parseNext;
	}

	Token!T front(){
		return _next;
	}

	typeof(this) save() const {
		return this;
	}
}

///
unittest{
	static uint identifyWhitespace(string str){
		uint ret = 0;
		while (ret < str.length && (str[ret] == ' ' || str[ret] == '\n'))
			ret ++;
		return ret;
	}

	static uint identifyWord(string str){
		foreach (i, c; str){
			if (c < 'a' || c > 'c')
				return cast(uint)i;
			if (i + 1 == str.length)
				return cast(uint)i + 1;
		}
		return 0;
	}

	import std.range : iota;

	enum Type{
		@Match(`keyword`)										Keyword,
		@Match(&identifyWhitespace, " \n")	Whitespace,
		@Match(&identifyWord, "abc")				Word,
		Error,
		EOF
	}

	Token!Type[] tokens;
	auto toks = Lexer!(Type, Type.Error, Type.EOF)(`  keyword aabbcc`);
	foreach (token; toks)
		tokens ~= token;

	assert (tokens.length == 4);
	assert(tokens[0].type.get!(Type.Whitespace));
	assert(tokens[1].type.get!(Type.Keyword));
	assert(tokens[2].type.get!(Type.Whitespace));
	assert(tokens[3].type.get!(Type.Word));

	while (!toks.empty)
		toks.popFront;
	assert(toks.empty);
	toks.popFront;
	assert(toks.front.type.get!(Type.EOF));
}
