module alis.compiler.semantic.main;

version (semantic):
import std.stdio,
			 std.json,
			 std.array,
			 std.algorithm,
			 std.typecons,
			 std.datetime.stopwatch;

import alis.common,
			 alis.compiler.common,
			 alis.compiler.error,
			 alis.compiler.lexer,
			 alis.compiler.parser,
			 alis.compiler.semantic,
			 alis.compiler.semantic.error,
			 alis.compiler.semantic.common,
			 alis.compiler.ast;

import core.stdc.stdlib;

debug import alis.compiler.semantic.sym0;

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
	node.val.ident = "alis-main";
	SmErrsVal!S0R stabVal = node.val.stab0Of;
	sw.stop;
	if (stabVal.isErr)
		stderr.writefln!"Errors:\n%(%s%)"(stabVal.err);
	else
		stabVal.val.stab.writeln;
	stderr.writefln!"done in: %d msecs"(sw.peek.total!"msecs");
}
