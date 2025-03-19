/++
Some D Traits used in Alis
+/
module meta;

import std.meta,
			 std.traits;

/// T.stringof
enum StringOf(alias T) = T.stringof;
/// Joins sequences of strings `T` with `S` as separator
template Join(string S, T...){
	enum Join = join();
	string join(){
		string ret = T[0];
		foreach (t; T[1 .. $])
			ret ~= S ~ t;
		return ret;
	}
}

/// if `T` is not an abstract class
enum IsNotAbstractClass(T) = !isAbstractClass!T;

/// if `T` can be found in `List`
template CanFind(List...){
	enum CanFind(T) = staticIndexOf!(T, List) != -1;
}

/// If any of the `U` UDAs exist on `T`
template HasAnyUDA(U...){
	template HasAnyUDA(alias T){
		enum HasAnyUDA = function bool (){
			static foreach (u; U){
				static if (hasUDA!(T, u))
					return true;
			}
			return false;
		}();
	}
}

/// Gets most derived common parent class across all `T...`
template CommonParent(T...) if (T.length > 0){
	static if (T.length == 1){
		alias CommonParent = T;
	} else {
		private alias Base(C) = AliasSeq!(C, BaseClassesTuple!C);
		alias CommonParent = Base!(T[0])[getInd()];
		private size_t getInd(){
			foreach (i, P; Base!(T[0])){
				bool found = true;
				static foreach (C; T[1 .. $]){
					static if (staticIndexOf!(P, Base!C) == -1)
						found = false;
				}
				if (found)
					return i;
			}
			return cast(ptrdiff_t)Base!(T[0]).length - 1;
		}
	}
}

/// Gets least derived Child of `T` among `N`
template LeastDerivedChild(N...) if (N.length > 0){
	alias LeastDerivedChild(T) = N[(){
		size_t min = size_t.max,
					 minI = size_t.max;
		static foreach (size_t i, C; N){{
			enum Ind = staticIndexOf!(T, BaseClassesTuple!C);
			import std.conv : to;
			pragma(msg, C.stringof ~ " = " ~ Ind.to!string);
			if (Ind < min && Ind != -1){
				min = Ind;
				minI = i;
			}
		}}
		if (min == size_t.max)
			return staticIndexOf!(T, N);
		return minI;
	}()];
}

/// Gets the first parameter type for functions
template FirstParamsOf(F...) if (allSatisfy!(isCallable, F)){
	alias FirstParamsOf = AliasSeq!();
	static foreach (fn; F){
		static if (Parameters!fn.length == 0)
			static assert(false, "function in FirstParamsOf has zero parameters");
		FirstParamsOf = AliasSeq!(FirstParamsOf, Parameters!fn[0]);
	}
}

/// Gets parent classes for `C`, among `T...`
template ParentSubset(C, T...) if (T.length > 0){
	static if (T.length == 1){
		static if (is (C : T)){
			alias ParentSubset = T;
		} else {
			alias ParentSubset = AliasSeq!();
		}
	} else {
		enum IsInT(X) = staticIndexOf!(X, T) != -1;
		alias ParentSubset = Filter!(IsInT, AliasSeq!(C, BaseClassesTuple!C));
	}
}
