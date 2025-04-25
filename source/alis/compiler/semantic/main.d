module alis.compiler.semantic.main;

version (semantic):
import std.stdio,
			 std.json,
			 std.array,
			 std.algorithm,
			 std.datetime.stopwatch;

import alis.common,
			 alis.compiler.error,
			 alis.compiler.lexer,
			 alis.compiler.parser,
			 alis.compiler.semantic,
			 alis.compiler.semantic.error,
			 alis.compiler.semantic.common,
			 alis.compiler.ast;

import core.stdc.stdlib;

void main(){
	string source;
	while (!stdin.eof) source ~= stdin.readln;
	StopWatch sw = StopWatch(AutoStart.yes);
	CmpErrVal!Module node = source.tokenize.parse;
	sw.stop;
	stderr.writefln!"parsed stdin in: %d msecs"(sw.peek.total!"msecs");
	sw.reset;
	if (node.isErr){
		JSONValue err;
		err["_error"] = node.err.toString;
		writeln(err.toPrettyString);
		exit(1);
	}
	sw.start;
	SmErrsVal!AModule modVal = node.val.aModOf;
	sw.stop;
	if (modVal.isErr)
		modVal.err.writeln;
	else
		modVal.val.writeln;
	stderr.writefln!"done in: %d msecs"(sw.peek.total!"msecs");
}
