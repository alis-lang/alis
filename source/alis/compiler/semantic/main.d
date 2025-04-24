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
	CmpErr[] errs;
	// TODO: do the semantic analysis. HOW??? HOW????
	sw.stop;
	stderr.writefln!"semantic analysis in: %d msecs"(sw.peek.total!"msecs");
	doTheStuff(node.val);
	/*if (errs){
		JSONValue err;
		err["_error"] = errs.map!(e => e.toString).array;
		writeln(err.toPrettyString);
		exit(1);
	} else {
		stdout.writeln(node.val.toJson.toPrettyString);
	}*/
}

void doTheStuff(Module node){
	node.ident = "alis-main";
	import alis.compiler.semantic.symbols;
	auto modVal = aModOf(node);
	if (modVal.isErr)
		modVal.err.writeln;
	else
		modVal.val.writeln;
}
