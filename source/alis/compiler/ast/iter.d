/++
AST Iterator module
+/
module alis.compiler.ast.iter;

import meta,
			 alis.compiler.common;

import std.meta,
			 std.traits;

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
template IsItFn(alias F){
	static if (isCallable!F &&
			(hasUDA!(F, ItPre) || hasUDA!(F, ItPost)) &&
			Parameters!F.length == 2 &&
			is (Parameters!F[0] : ASTNode) &&
			is (Parameters!F[1] == struct)){
		enum IsItFn = true;
	} else {
		enum IsItFn = false;
	}
}

/// Gets relevant BaseClassesTuple of `T`, given relevant types `R...`
private template RelBaseClasses(R...){
	alias RelBaseClasses(T) = Filter!(CanFind!R, BaseClassesTuple!T);
}

/// Gets subset iterating functions for Node Type N, among `F...`
private template ItFnsFor(N, F...) if (
		IsASTNode!N && allSatisfy!(isCallable, F)){
	alias ItFnsFor = AliasSeq!();
	static foreach (T; AliasSeq!(N,
				Instantiate!(RelBaseClasses!(FirstParamsOf!F), N))){
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
/// `N...` - AST Node Types
/// `F...` - Iterator Functions
public template ASTIter(N...) if (allSatisfy!(IsASTNode, N)){
	struct ASTIter(Fns...) if (allSatisfy!(IsItFn, Fns)){
		/// types being handles
		private alias RelT = NoDuplicates!(FirstParamsOf!Fns);
		/// least derived children, where children are only relevant ones
		alias LDC = LeastDerivedChildren!RelT;
		/// least derived children
		alias LDC_all = LeastDerivedChildren!N;

		/// iterates over array
		pragma(inline, true)
		private static void _arrayHandle(N, S)(N nodes, auto ref S state){
			static if (isArray!N){
				foreach (n; nodes){
					_arrayHandle(n, state);
				}
			} else static if (!isArray!N){
				return iterate(nodes, state);
			}
		}

		/// Descends down on node
		pragma(inline, true)
		private static void _descend(N, S)(N node, auto ref S state){
			static foreach (C; LDC_all!N){
				if (auto sub = cast(C)node){
					return _descend(sub, state);
				}
			}
			static foreach (size_t i, string name; FieldNamesRel!N){
				_arrayHandle(__traits(getMember, node, name), state);
			}
		}

		/// Default iterator for any non-array type `N` with state of type `S`
		public static void iterate(N, S)(N node, auto ref S state){
			if (node is null)
				return;
			static if (RelT.length == 0){
				return _descend(node, state);
			}
			static foreach (C; LDC!N){
				if (auto sub = cast(C)node){
					return iterate(sub, state);
				}
			}

			alias F = ItFnsFor!(N, Fns);
			static if (F.length){
				static if (hasUDA!(F[0], ItPre))
					F[0](node, state);
				static if (!hasUDA!(F[0], ItTerm))
					_descend(node, state);
				static if (hasUDA!(F[0], ItPost))
					F[0](node, state);
				return;
			}
			return _descend(node, state);
		}
	}
}
