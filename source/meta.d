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

/// Gets least derived Children of `T` among `N`
template LeastDerivedChildren(N...) if (N.length > 0){
	template LeastDerivedChildren(T){
		enum IndOf(TT) = staticIndexOf!(T, BaseClassesTuple!TT);
		enum Indexes = staticMap!(IndOf, N);
		enum Min = (){
			size_t ret = size_t.max;
			static foreach (size_t ind; Indexes){
				if (ind != -1 && ind < ret)
					ret = ind;
			}
			return ret;
		}();
		alias LeastDerivedChildren = AliasSeq!();
		static foreach (size_t i, C; N){
			static if (Indexes[i] == Min && !is (C == T) && is (C : T))
				LeastDerivedChildren = AliasSeq!(LeastDerivedChildren, C);
		}
	}
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

/// Signed integers, ordered from lowest to highest size
alias SignedInts = SignedInts_Impl!();

private template SignedInts_Impl(){
	alias SignedInts_Impl = AliasSeq!(byte, short, int);
	static if (ptrdiff_t.sizeof > int.sizeof)
		SignedInts_Impl = AliasSeq!(SignedInts_Impl, ptrdiff_t);
}

/// Unsigned integers, ordered from lowest to highest size
alias UnsignedInts = UnsignedInts_Impl!();

private template UnsignedInts_Impl(){
	alias UnsignedInts_Impl = AliasSeq!(ubyte, ushort, uint);
	static if (size_t.sizeof > uint.sizeof)
		UnsignedInts_Impl = AliasSeq!(UnsignedInts_Impl, size_t);
}

/// Floats, ordered from lowest to highest size
alias Floats = AliasSeq!(float, double);
