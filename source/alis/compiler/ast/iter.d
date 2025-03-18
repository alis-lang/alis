/++
AST Iterator module
+/
module alis.compiler.ast.iter;

import alis.compiler.common;

import std.meta,
			 std.traits,
			 std.algorithm;

debug import std.stdio;

import meta;

/// UDA for tagging as pre-order iteration
public enum ItPre;
/// UDA for tagging as post-order iteration
public enum ItPost;
/// UDA for tagging as iteration terminator
public enum ItTerm;

/// Sequence of Iterator Functions in a container (module etc)
/// Template Params:
/// `S` - State Type
/// `M` - container
public template ItFnsOf(S, alias M){
	alias ItFnsOf = AliasSeq!();
	alias Checker = IsItFn!S;
	static foreach (string N; __traits(allMembers, M)){
		static if (Checker!(__traits(getMember, M, N))){
			ItFnsOf = AliasSeq!(ItFnsOf, __traits(getMember, M, N));
		}
	}
}

/// Whether something is a ItFn, for state type `S`
private template IsItFn(S) if (is (S == struct)){
	template IsItFn(alias F){
		static if (isCallable!F &&
				(hasUDA!(F, ItPre) || hasUDA!(F, ItPost)) &&
				Parameters!F.length == 2 &&
				is (Parameters!F[0] : ASTNode) &&
				is (S : Parameters!F[1])){
			enum IsItFn = true;
		} else {
			enum IsItFn = false;
		}
	}
}

/// Gets subset iterating functions for Node Type N, among `Us...`
private template ItFnsFor(N, F...) if (
		is (N : ASTNode) && allSatisfy!(isCallable, F)){
	alias ItFnsFor = AliasSeq!();
	static foreach (T; AliasSeq!(N, BaseClassesTuple!N)){
		static if (staticIndexOf!(T, FirstParamsOf!F) != -1){
			ItFnsFor = AliasSeq!(ItFnsFor,
					F[staticIndexOf!(T, FirstParamsOf!F)]);
		}
	}
}

/// Whether a type T is relevant for iteration
private template IsRel(T){
	static if (isArray!T){
		alias IsRel = IsRel!(ForeachType!T);
	} else {
		static if (isAssociativeArray!T){
			static assert(false, "assoc_array not yet supported sadly");
		}
		enum IsRel = is (T : ASTNode);
	}
}

/// Gets relevant fields' names of N
private template FieldNamesRel(N) if (is (N : ASTNode)){
	alias FieldNamesRel = AliasSeq!();
	static foreach (size_t i, F; Fields!N){
		static if (IsRel!F){
			FieldNamesRel = AliasSeq!(FieldNamesRel, FieldNameTuple!N[i]);
		}
	}
}

/// Gets relevant fields' types of N
private template FieldTypesRel(N) if (is (N : ASTNode)){
	alias FieldTypesRel = Filter!(IsRel, Fields!N);
}

/// AST Iterator
///
/// Template Params:
/// `S` - State Type. Must be `struct`
/// `Fns` - Iterator Functions
public struct ASTIter(S, Fns...) if (
		is (S == struct) /*&& allSatisfy!(IsItFn!S, Fns)*/){
	/// types being handles
	private alias RelT = NoDuplicates!(FirstParamsOf!Fns);

	/// calls most suited ItFn
	pragma(inline, true)
	private static _callItFn(N)(N node, auto ref S state){
		// TODO: this is only a stub
		alias F = ItFnsFor!(N, Fns);
		static if (F.length){
			F[0](node, state);
		}
	}

	/// iterates over array or single type
	pragma(inline, true)
	private static void _iterate(N)(N nodes, auto ref S state){
		debug stderr.writefln!"_iterate: %s"(N.stringof);
		static if (isArray!N){
			foreach (n; nodes){
				_iterate(n, state);
			}
		} else static if (!isArray!N){
			iterate(nodes, state);
		}
	}

	/// Descends down on node
	pragma(inline, true)
	private static void _descend(N)(N node, auto ref S state){
		debug stderr.writefln!"_descend: %s"(N.stringof);
		static foreach (size_t i, string name; FieldNamesRel!N){
			_iterate!(FieldTypesRel!N[i])(__traits(getMember, node, name), state);
		}
	}

	/// Default iterator for any type T
	public static void iterate(N)(N node, auto ref S state){
		if (node is null)
			return;
		debug stderr.writefln!"iterate: %s"(N.stringof);
		alias F = ItFnsFor!(N, Fns);
		static if (F.length == 0){
			_descend(node, state);
			return;
		}
		static if (F.length){
			static if (hasUDA!(F[0], ItPre)){
				_callItFn(node, state);
			}
			static if (!hasUDA!(F[0], ItTerm)){
				_descend(node, state);
			}
			static if (hasUDA!(F[0], ItPost)){
				_callItFn(node, state);
			}
		}
	}
}
