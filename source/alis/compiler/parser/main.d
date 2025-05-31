module alis.compiler.parser.main;

version(parser):
import std.stdio,
			 std.json,
			 std.datetime.stopwatch;

import alis.compiler.parser,
			 alis.compiler.error,
			 alis.compiler.lexer;

import core.stdc.stdlib;

void main(){
	string source;
	while (!stdin.eof) source ~= stdin.readln;
	StopWatch sw = StopWatch(AutoStart.yes);
	CmpErrVal!Module node = source.tokenize.parse!();
	sw.stop;
	stderr.writefln!"parsed stdin in: %d msecs"(sw.peek.total!"msecs");
	if (node.isErr){
		JSONValue err;
		err["_error"] = node.err.toString;
		writeln(err.toPrettyString);
		exit(1);
	} else {
		stdout.writeln(node.val.jsonOf.toPrettyString);
	}
}
