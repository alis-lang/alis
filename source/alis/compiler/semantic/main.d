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
			 alis.compiler.semantic.sym0,
			 alis.compiler.semantic.sym1,
			 alis.compiler.ast;

import core.stdc.stdlib;

void main(){
	string source;
	while (!stdin.eof) source ~= stdin.readln;
	StopWatch sw = StopWatch(AutoStart.yes);
	CmpErrVal!Module node = source.tokenize.parse("alis-main");
	sw.stop;
	stderr.writefln!"parsed stdin in: %d msecs"(sw.peek.total!"msecs");
	sw.reset;
	if (node.isErr){
		stderr.writefln!"Errors:\n%s"(node.err.toString);
		exit(1);
	}

	sw.start;
	node.val.ident = "alis-main";
	SmErrsVal!S1R stabVal = node.val.stabOf;
	if (stabVal.isErr){
		stderr.writefln!"Errors:\n%(%s\n%)"(stabVal.err);
		stderr.writefln!"done in: %d msecs"(sw.peek.total!"msecs");
		exit(1);
	}
	stabVal.val.writeln;
	stderr.writefln!"done in: %d msecs"(sw.peek.total!"msecs");
}
