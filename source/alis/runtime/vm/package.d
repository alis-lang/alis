module alis.runtime.vm;

import std.conv,
			 std.meta,
			 std.traits;

import alis.runtime.vm.common;
public import alis.runtime.vm.bytecode;

/// Execute a Code
public void execute(S, T...)(
		ref ABC code,
		ref S state,
		size_t label = size_t.max) if (allSatisfy!(isCallable, T)){
	size_t ic;
	if (label < code.labels.length)
		ic = code.labels[label];
	while (ic < code.end){
		immutable ushort inst = code.code[ic .. $].as!ushort;
		ic += ushort.sizeof;
		switcher: switch (inst){
			foreach (ind, Inst; T){
				case ind:
					{
						InstArgs!Inst p;
						static foreach (i, Arg; InstArgs!Inst){
							static if (is (Arg == string)){
								immutable size_t
									start = *(cast(size_t*)(code.code.ptr + ic)),
									end = *(cast(size_t*)(code.code.ptr + ic + size_t.sizeof));
								p[i] = cast(string)(code.code[start .. end]);
								ic += size_t.sizeof * 2;
							} else {
								p[i] = *(cast(Arg*)(code.code.ptr + ic));
								ic += Arg.sizeof;
							}
						}
						mixin(InstCallStatement!Inst);
					}
					break switcher;
			}
			default:
				break;
		}
	}
}

///
unittest{
	struct State{ int i; }
	void inc1(ref State _state){
		_state.i += 1;
	}
	void inc2(ref State _state){
		_state.i += 2;
	}
	ABC code = parseABC!(inc1, inc2)([
		"inc1", "inc2", "inc1"
	]);
	State state;
	execute!(State, inc1, inc2)(code, state);
	assert(state.i == 4);
}

/// ditto
public void execute(T...)(ref ABC code, size_t label = size_t.max) if (
		allSatisfy!(isCallable, T) && !InstsIsStateful!T){
	ubyte dummyState;
	execute!(ubyte, T)(code, dummyState, label);
}

///
unittest{
	int i;
	void inc1(){
		i += 1;
	}
	void inc2(){
		i += 2;
	}
	ABC code = parseABC!(inc1, inc2)([
		"inc1", "inc2", "inc1"
	]);
	execute!(inc1, inc2)(code);
	assert(i == 4);
}
