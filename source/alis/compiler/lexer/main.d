/++
Testing program for Lexer

Build with `dub build -c=lexer`
+/
module alis.compiler.lexer.main;

import alis.compiler.lexer.tokens;

import utils.ds : Flags;

version(lexer):
import std.stdio,
			 std.datetime.stopwatch;
void main(string[] args){
	string source;
	while (!stdin.eof)
		source ~= stdin.readln;
	if (args.length > 1 && args[1] == "bench"){
		StopWatch sw = StopWatch(AutoStart.yes);
		auto lex = source.tokenize;
		while (!lex.empty)
			lex.popFront;
		sw.stop;
		stderr.writefln!"tokenized stdin in: %d msecs"(sw.peek.total!"msecs");
		return;
	}
	foreach (Tok tok; source.tokenize)
		tok.writeln;
}
