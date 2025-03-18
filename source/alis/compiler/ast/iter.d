/++
AST Iterator module
+/
module alis.compiler.ast.iter;

import alis.compiler.common;

import std.meta,
			 std.traits,
			 std.algorithm;

import meta;

/// UDA for tagging as iterator function
public enum IT;

/// Sequence of Iterator Functions in a container (module etc)
public template ITFnsOf(alias M){
	alias ITFnsOf = AliasSeq!();
	static foreach (string N; __traits(allMembers, M)){
		static if (IsITFn!(__traits(getMember, M, N))){
			ITFnsOf = AliasSeq!(ITFnsOf, __traits(getMember, M, N));
		}
	}
}

/// Whether something is a ITFn
private template IsITFn(alias F){
	static if (isCallable!F && hasUDA!(F, IT)){
		enum IsITFn = true;
	} else {
		enum IsITFn = false;
	}
}

/// Gets subset iterating functions for Node Type N, among `Us...`
private template ITFnsFor(N, F...) if (
		is (N : ASTNode) && allSatisfy!(isCallable, F)){
	alias ITFnsFor = AliasSeq!();
	static foreach (T; AliasSeq!(N, BaseClassesTuple!N)){
		static if (staticIndexOf!(T, FirstParamsOf!F) != -1){
			ITFnsFor = AliasSeq!(ITFnsFor,
					F[staticIndexOf!(T, FirstParamsOf!F)]);
		}
	}
}

/// Gets relevant fields of T
private template FieldsRel(N) if (is (N : ASTNode)){
	template IsRel(T){
		static if (isArray!T){
			alias IsRel = IsRel!(ForeachType!T);
		} else {
			static if (isAssociativeArray!T){
				static assert(false, "assoc_array not yet supported sadly");
			}
			enum IsRel = is (T : ASTNode);
		}
	}
	alias FieldsRel = Filter!(IsRel, Fields!N);
}

/// AST Iterator
public struct Iterator(Fns...){
	/// Default iterator for any type T
	void _iterate(N)(N node){
		// TODO continue from here
	}
}
