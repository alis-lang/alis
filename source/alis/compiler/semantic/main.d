module alis.compiler.semantic.main;

version (semantic):
import std.stdio,
			 std.json,
			 std.array,
			 std.algorithm,
			 std.datetime.stopwatch;

import alis.compiler.error,
			 alis.compiler.lexer,
			 alis.compiler.parser,
			 alis.compiler.semantic;

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
	CmpErr[] errs;
	// TODO: do the semantic analysis. HOW??? HOW????
	sw.stop;
	stderr.writefln!"semantic analysis in: %d msecs"(sw.peek.total!"msecs");
	if (errs){
		JSONValue err;
		err["_error"] = errs.map!(e => e.toString).array;
		writeln(err.toPrettyString);
		exit(1);
	} else {
		stdout.writeln(node.val.toJson.toPrettyString);
	}
}
