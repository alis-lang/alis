/++
Token definitions for Alis
+/
module alis.compiler.lexer.tokens;

import alis.compiler.lexer.lexer;

import utils.ds;

debug import std.stdio;

import std.algorithm,
			 std.range;

/// An Alis source code Token
public alias Tok = alis.compiler.lexer.lexer.Token!TokType;

/// Range type for tokenizer
public alias TokRange = Lexer!(TokType, TokType.UnexpectedToken, TokType.EOF);

/// Returns: Tokenizer Range on source code
public TokRange tokenize(string source,
		Flags!TokType ignore = Flags!TokType(
			TokType.Whitespace, TokType.Comment, TokType.CommentMultiline
			)){
	return TokRange(source, ignore);
}

/// possible token types
enum TokType{
	// basic
	@Match(&identifyWhitespace, "\t \r\n")	Whitespace,
	@Match(&identifyComment, "/")						Comment,
	@Match(&identifyCommentMultiline, "/")	CommentMultiline,
	@Match(&identifyLiteralInt, "-0123456789")
																					LiteralInt,
	@Match(&identifyLiteralFloat, "-0123456789")
																					LiteralFloat,
	@Match(&identifyLiteralString, "\"")		LiteralString,
	@Match(&identifyLiteralStringML, "\"\"\"\n")
																					LiteralStringML,
	@Match(&identifyLiteralChar, "'")				LiteralChar,
	@Match(&identifyLiteralHexadecimal, "0")
																					LiteralHexadecimal,
	@Match(&identifyLiteralBinary, "0")			LiteralBinary,
	@Match(&identifyLiteralScientifc,
			"+-0123456789")											LiteralScientific,
	@Match(&identifyIdentifier, cast(string)
			iota('a', 'z' + 1).
			chain(iota('A', 'Z' + 1)).
			map!(a => cast(char)a).array ~ '_')	Identifier,

	// keywords
	@Match(`template`)											Template,
	@Match(`mixin`)													Mixin,
	@Match(`import`)												Import,
	@Match(`as`)														As,
	@Match(`this`)													This,
	@Match(`alias`)													Alias,
	@Match(`fn`)														Fn,
	@Match(`var`)														Var,
	@Match(`enum`)													Enum,
	@Match(`struct`)												Struct,
	@Match(`union`)													Union,
	@Match(`utest`)													UTest,
	@Match(`pub`)														Pub,
	@Match(`ipub`)													IPub,
	@Match(`return`)												Return,
	@Match(`auto`)													Auto,
	@Match(`const`)													Const,
	@Match(`static`)												Static,
	@Match(`int`)														Int,
	@Match(`uint`)													UInt,
	@Match(`float`)													Float,
	@Match(`char`)													Char,
	@Match(`string`)												String,
	@Match(`bool`)													Bool,
	@Match(`true`)													True,
	@Match(`false`)													False,
	@Match(`if`)														If,
	@Match(`else`)													Else,
	@Match(`while`)													While,
	@Match(`do`)														Do,
	@Match(`for`)														For,
	@Match(`switch`)												Switch,
	@Match(`case`)													Case,
	@Match(`break`)													Break,
	@Match(`continue`)											Continue,

	// conditional commpilation
	@Match(&identifyIntrinsic, "$")					Intrinsic,
	@Match(`$if`)														StaticIf,
	@Match(`$for`)													StaticFor,
	@Match(`$switch`)												StaticSwitch,

	// intrinsics
	@Match(`$type`)													IntrType,
	@Match(`$noinit`)											IntrNoInit,
	@Match(`$noinitval`)										IntrNoInitVal,
	@Match(`$int`)													IntrInt,
	@Match(`$uint`)													IntrUInt,
	@Match(`$float`)												IntrFloat,
	@Match(`$char`)													IntrChar,
	@Match(`$slice`)												IntrSlice,
	@Match(`$array`)												IntrArray,
	@Match(`$arrayLen`)											IntrArrayLen,
	@Match(`$arrayInd`)											IntrArrayInd,
	@Match(`$unionIs`)											IntrUnionIs,
	@Match(`$vt`)														IntrVt,
	@Match(`$attrsOf`)											IntrAttrsOf,
	@Match(`$byAttrs`)											IntrByAttrs,
	@Match(`$debug`)												IntrDebug,
	@Match(`$stackTrace`)										IntrStackTrace,
	@Match(`$isType`)												IntrIsType,
	@Match(`$seqLen`)												IntrSeqLen,
	@Match(`$seqInd`)												IntrSeqInd,
	@Match(`$err`)													IntrErr,
	@Match(`$typeOf`)												IntrTypeOf,

	// binary operators
	@Match(`(`)
	@Match(`[`)
	@Match(`{`)
	@Match(`->`)
	@Match(`,`)
	@Match(`.`)
	@Match(`!!`)
	@Match(`??`)
	@Match(`*`)
	@Match(`/`)
	@Match(`%`)
	@Match(`+`)
	@Match(`-`)
	@Match(`<<`)
	@Match(`>>`)
	@Match(`==`)
	@Match(`!=`)
	@Match(`>=`)
	@Match(`<=`)
	@Match(`>`)
	@Match(`<`)
	@Match(`:`)
	@Match(`is`)
	@Match(`!is`)
	@Match(`&`)
	@Match(`|`)
	@Match(`^`)
	@Match(`&&`)
	@Match(`||`)
	@Match(`=`)
	@Match(`+=`)
	@Match(`-=`)
	@Match(`*=`)
	@Match(`/=`)
	@Match(`%=`)
	@Match(`&=`)
	@Match(`|=`)
	@Match(`^=`)
	@Match(`@=`)														OpBin,

	// prefix operators
	@Match(`(`)
	@Match(`!`)
	@Match(`is`)
	@Match(`!is`)
	@Match(`@`)
	@Match(`#`)
	@Match(`const`)
	@Match(`~`)															OpPre,

	// postfix operators
	@Match(`!`)
	@Match(`++`)
	@Match(`--`)
	@Match(`?`)
	@Match(`@`)
	@Match(`...`)														OpPost,

	// misc
	@Match(`$(`)														TmBracketOpen,
	@Match(`(`)															BracketOpen,
	@Match(`)`)															BracketClose,
	@Match(`[`)															IndexOpen,
	@Match(`]`)															IndexClose,
	@Match(`{`)															CurlyOpen,
	@Match(`}`)															CurlyClose,
	@Match(`;`)															Semicolon,

	// named operators
	@Match(`(`)															OpCall,
	@Match(`[`)															OpIndex,
	@Match(`{`)															OpCurly,
	@Match(`->`)														OpArrow,
	@Match(`,`)															OpComma,
	@Match(`.`)															OpDot,
	@Match(`!!`)														OpNotNot,
	@Match(`??`)														OpQQ,
	@Match(`*`)															OpMul,
	@Match(`/`)															OpDiv,
	@Match(`%`)															OpMod,
	@Match(`+`)															OpAdd,
	@Match(`-`)															OpSub,
	@Match(`<<`)														OpLS,
	@Match(`>>`)														OpRS,
	@Match(`==`)														OpEq,
	@Match(`!=`)														OpNotEq,
	@Match(`>=`)														OpGrEq,
	@Match(`<=`)														OpLsEq,
	@Match(`>`)															OpGr,
	@Match(`<`)															OpLs,
	@Match(`:`)															OpColon,
	@Match(`is`)														OpIs,
	@Match(`!is`)														OpNotIs,
	@Match(`&`)															OpBitAnd,
	@Match(`|`)															OpBitOr,
	@Match(`^`)															OpBitXor,
	@Match(`&&`)														OpAnd,
	@Match(`||`)														OpOr,
	@Match(`=`)															OpAssign,
	@Match(`+=`)														OpAssignAdd,
	@Match(`-=`)														OpAssignSub,
	@Match(`*=`)														OpAssignMul,
	@Match(`/=`)														OpAssignDiv,
	@Match(`%=`)														OpAssignMod,
	@Match(`&=`)														OpAssignAnd,
	@Match(`|=`)														OpAssignOr,
	@Match(`^=`)														OpAssignXor,
	@Match(`@=`)														OpAssignRef,

	// prefix operators
	@Match(`!`)															OpNot,
	@Match(`@`)															OpRef,
	@Match(`#`)															OpTag,
	@Match(`~`)															OpBitNot,

	// postfix operators
	@Match(`++`)														OpInc,
	@Match(`--`)														OpDec,
	@Match(`?`)															OpQ,
	@Match(`...`)														OpDots,


	UnexpectedToken, /// Lexer error
	EOF, /// End of File
}

private uint identifyWhitespace(string str){
	foreach (index, ch; str){
		if (ch != ' ' && ch != '\t' && ch != '\n')
			return cast(uint)index;
	}
	return cast(uint)(str.length);
}

unittest{
	assert("a".identifyWhitespace == 0);
	assert(" ".identifyWhitespace == 1);
	assert("\t".identifyWhitespace == 1);
	assert("\t\t".identifyWhitespace == 2);
	assert(" \t".identifyWhitespace == 2);
	assert("\t ".identifyWhitespace == 2);
}

private uint identifyComment(string str){
	if (str.length < 2 || str[0 .. 2] != "//")
		return cast(uint)0;
	foreach (index, ch; str[2 .. $]){
		if (ch == '\n')
			return cast(uint)index + 3;
	}
	return cast(uint)(str.length);
}

unittest{
	assert("a".identifyComment == 0);
	assert("/".identifyComment == 0);
	assert("//".identifyComment == 2);
	assert("//12\n".identifyComment == 5);
}

private uint identifyCommentMultiline(string str){
	if (str.length < 2 || str[0 .. 2] != "/*")
		return 0;
	for (uint i = 4; i <= str.length; i ++){
		if (str[i - 2 .. i] == "*/")
			return i;
	}
	return 0;
}

private uint identifyLiteralInt(string str){
	immutable ubyte neg = str[0] == '-';
	foreach (index, ch; str[neg .. $]){
		if (ch < '0' || ch > '9')
			return cast(uint)index + neg;
	}
	return cast(uint)str.length;
}

private uint identifyLiteralFloat(string str){
	int charsAfterDot = -1;
	immutable ubyte neg = str[0] == '-';
	foreach (index, ch; str[neg .. $]){
		if (ch == '.' && charsAfterDot == -1){
			charsAfterDot = 0;
			continue;
		}
		if (ch < '0' || ch > '9')
			return (charsAfterDot > 0) * (cast(uint)index + neg);
		charsAfterDot += charsAfterDot != -1;
	}
	return (charsAfterDot > 0) * cast(uint)str.length;
}

private uint identifyLiteralString(string str){
	if (str.length < 2 || str[0] != '"')
		return 0;
	for (uint i = 1; i < str.length; i ++){
		if (str[i] == '\\'){
			i ++;
			continue;
		}
		if (str[i] == '"')
			return i + 1;
	}
	return 0;
}

private uint identifyLiteralStringML(string str){
	if (str.length < 8 || str[0 .. 4] != "\"\"\"\n")
		return 0;
	for (uint i = 4; i + 3 < str.length; i ++){
		if (str[i] != '\n')
			continue;
		import std.stdio;
		if (str[i + 1 .. i + 4] == "\"\"\"")
			return i + 4;
	}
	return 0;
}

unittest{
	import std.conv;
	assert("".identifyLiteralStringML == 0);
	assert("//".identifyLiteralStringML == 0);
	assert("//\n".identifyLiteralStringML == 0);
	assert("\"\"\"".identifyLiteralStringML == 0);
	assert("\"\"\" \"\"\"".identifyLiteralStringML == 0);
	assert("\"\"\"\n \n\"\"\"".identifyLiteralStringML == 9);
}

private uint identifyLiteralChar(string str){
	if (str.length < 3 || str[0] != '\'')
		return 0;
	if (str[1] == '\\' && str.length > 3 && str[3] == '\'')
		return 4;
	if (str[1] != '\'' && str[2] == '\'')
		return 3;
	return 0;
}

private uint identifyLiteralHexadecimal(string str){
	if (str.length < 3 || str[0] != '0' || (str[1] != 'x' && str[1] != 'X'))
		return 0;
	foreach (index, ch; str[2 .. $]){
		if ((ch < '0' || ch > '9')
				&& (ch < 'A' || ch > 'F')
				&& (ch < 'a' || ch > 'f'))
			return cast(uint)(index > 0) * (cast(uint)index + 2);
	}
	return cast(uint)str.length;
}

private uint identifyLiteralBinary(string str){
	if (str.length < 3 || str[0] != '0' || (str[1] != 'b' && str[1] != 'B'))
		return 0;
	foreach (index, ch; str[2 .. $]){
		if (ch != '0' && ch != '1')
			return cast(uint)(index > 0) * (cast(uint)index + 2);
	}
	return cast(uint)str.length;
}

private uint identifyLiteralScientifc(string str){
	uint num = identifyLiteralFloat(str).max(identifyLiteralInt(str));
	if (!num)
		return 0;
	if (str[num] != 'e')
		return 0;
	uint exp = identifyLiteralInt(str[num + 1 .. $]);
	if (!exp)
		return 0;
	return num + 1 + exp;
}

private uint identifyIdentifier(string str){
	uint len;
	while (len < str.length && str[len] == '_')
		len ++;
	if (len == 0 &&
			(str[len] < 'a' || str[len] > 'z') &&
			(str[len] < 'A' || str[len] > 'Z'))
		return 0;
	for (; len < str.length; len ++){
		const char ch = str[len];
		if ((ch < '0' || ch > '9') &&
				(ch < 'a' || ch > 'z') &&
				(ch < 'A' || ch > 'Z') && ch != '_')
			return len;
	}
	return cast(uint)str.length;
}

private uint identifyIntrinsic(string str){
	if (str.length < 2 || str[0] != '$')
		return 0;
	const uint len = identifyIdentifier(str[1 .. $]);
	if (!len)
		return 0;
	return 1 + len;
}

unittest{
	import std.string;

	string source =
`fn main(){ // comment
return 2 + 0B10; //another comment
voidNot
}`;
	Tok[] tokens;
	foreach (token; source.tokenize(Flags!TokType.init))
		tokens ~= token;

	const string[] expectedStr = [
		"fn", " ", "main", "(", ")", "{", " ", "// comment\n",
		"return", " ", "2", " ",  "+", " ", "0B10", ";", " ",
		"//another comment\n", "voidNot", "\n", "}"
	];
	const TokType[] expectedType = [
		TokType.Fn, TokType.Whitespace,
		TokType.Identifier, TokType.BracketOpen,
		TokType.BracketClose, TokType.CurlyOpen, TokType.Whitespace,
		TokType.Comment, TokType.Return,
		TokType.Whitespace, TokType.LiteralInt, TokType.Whitespace,
		TokType.OpBin, TokType.Whitespace, TokType.LiteralBinary,
		TokType.Semicolon, TokType.Whitespace, TokType.Comment,
		TokType.Identifier, TokType.Whitespace,
		TokType.CurlyClose
	];
	foreach (i, token; tokens){
		assert (token.token == expectedStr[i] && token.type[expectedType[i]],
				"failed to match at index %d: `%s` != `%s`, type match: %s".format(i,
					token.token, expectedStr[i], token.type.get(expectedType[i])));
	}
}
