/++
Testing program for Lexer

Build with `dub build -c=lexer`
+/
module alis.compiler.lexer.main;

import alis.compiler.lexer.tokens;

import utils.ds : Flags;

version(lexer):
import std.stdio;
void main(){
	string source;
	while (!stdin.eof)
		source ~= stdin.readln;
	foreach (Tok tok; source.tokenize)
		tok.writeln;
}
