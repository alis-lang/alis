/++
Semantic Analysis common stuff
+/
module alis.compiler.semantic.common;

import alis.common,
			 alis.compiler.ast,
			 meta;

import std.algorithm,
			 std.range,
			 std.json,
			 std.meta,
			 std.traits,
			 std.conv;

debug import std.stdio;

/// Iterator Function Level
public struct ITL{
	size_t level;
	@disable this();
	this(size_t level){
		this.level = level;
	}
}

/// AST Iterator, using ItFns from module `M`, that are tagged with `ITL(L)`
public template ItL(alias M, size_t L){
	// yes, I know Fns not used. go ahead, remove it, I dare you.
	alias Fns = AliasSeq!();
	static foreach (F; ItFnsOf!M){
		static if (hasUDA!(F, ITL(L))){
			Fns = AliasSeq!(Fns, F);
		}
	}
	alias ItL = ASTIter!(Filter!(HasAnyUDA!(ITL(L)), ItFnsOf!M));
}

/// Symbol Tree Node
private struct STNode(T){
	T val;
	STNode!T[IdentU] next;
}

/// Symbol Tree
public alias STree(T) = STNode!T[IdentU];

/// whether a path exists in STree
public bool canFind(T)(auto ref const STree!T, IdentU[]){
	assert(false, "canFind on STree not implemented");
}

/// Symbol Table
final class STab(T){
public:
	struct Node(E){
		E val;
		IdentU[] vis;
		this (E val, IdentU[] vis) pure {
			this.val = val;
			this.vis = vis.dup;
		}
	}

	/// the symbol table
	Node!T[][IdentU] map;
	/// sub-tables
	Node!(STab!T)[IdentU] next;

	static struct ResRange{
	private:
		IdentU _head;
		IdentU _id;
		IdentU[] ctx;
		Node!T[][IdentU][] _maps;

		this(IdentU id, IdentU[] ctx, STab!T st) pure {
			this._id = id;
			this.ctx = ctx.dup;
			this._head = ctx.length ? ctx[0] : IdentU.init;
			_maps ~= st.map;
			foreach (IdentU i; ctx){
				if (i !in st.next)
					break;
				Node!(STab!T) nextNode = st.next[i];
				if (!nextNode.vis.isNoId && nextNode.vis != ctx)
					break;
				_maps ~= nextNode.val.map;
				st = nextNode.val;
			}
			_maps ~= [];
			popFront;
		}

	public:
		@disable this();
		this(this){}
		bool empty() pure {
			return _maps.length == 0;
		}
		void popFront() pure {
			if (empty)
				return;
			_maps = _maps[0 .. $ - 1];
			while (_maps.length && front.empty)
				_maps = _maps[0 .. $ - 1];
		}
		auto front() pure {
			Node!T[] arr;
			if (_maps.length && (_id in _maps[$ - 1]) !is null)
				arr = _maps[$ - 1][_id];
			return arr.filter!(node => node.vis.isNoId || node.vis == ctx);
		}
	}

	/// finds all candidates that match an IdentU, given a scope ctx
	/// Returns: range of candidates
	ResRange find(IdentU id, IdentU[] ctx) pure {
		return ResRange(id, ctx, this);
	}

	/// Returns: true if id can be found from ctx scope
	bool canFind(IdentU id, IdentU[] ctx) pure {
		return !find(id, ctx).empty;
	}

	/// Add a new value.
	void valAdd(IdentU id, T val, IdentU[] vis) pure {
		if (auto ptr = id in map){
			*ptr ~= Node!T(val, vis);
			return;
		}
		map[id] = [Node!T(val, vis)];
	}
	/// ditto
	void valAdd(IdentU id, T val, Visibility vis, IdentU[] ctx) pure {
		return valAdd(id, val, vis == Visibility.Default ? ctx : [IdentU.init]);
	}

	/// Add a new Symbol Table. **Will overwrite existing, if any.**
	void stAdd(IdentU id, STab!T st, IdentU[] vis) pure {
		next[id] = Node!(STab!T)(st, vis);
	}
	/// ditto
	void stAdd(IdentU id, STab!T st, Visibility vis, IdentU[] ctx) pure {
		return stAdd(id, st, vis == Visibility.Default ? ctx : [IdentU.init]);
	}

	JSONValue toJson() const {
		JSONValue ret;
		foreach (IdentU key; map.byKey){
			ret[key.toString] = map[key]
				.map!((const Node!T n){
						JSONValue obj;
						static if (__traits(compiles, n.val.toJson)){
							obj["val"] = n.val.toJson;
						} else {
							obj["val"] = n.val.toString;
						}
						obj["vis"] = n.vis.to!string;
						return obj;
						})
				.array;
		}
		foreach (IdentU key; next.byKey){
			immutable string s = key.toString;
			const Node!(STab!T) n = next[key];
			JSONValue obj;
			obj["val"] = n.val.toJson;
			obj["vis"] = n.vis.to!string;
			continue;
			if (s in ret)
				ret[s] ~= obj;
			else
				ret[s] = [obj];
		}
		return ret;
	}

	override string toString() const {
		return toJson().toPrettyString;
	}
}

/// Alis Template Resolution Node. Usually for tracking resolution across
/// aliases
package struct ATResN{
private:
	ATResN* _leaf;
	ATResN* _next;
	ATResN* _prev;

public:
	/// what is being resolved
	IdentU[] subject;
	/// result from resolution
	ASymbol sym;

	/// previous resolution, if any
	@property ATResN* prev() const pure {
		return cast(ATResN*)_prev;
	}
	/// next resolution, if any
	@property ATResN* next() const pure {
		return cast(ATResN*)_next;
	}
	/// Returns: most resolved Resolution. will be `this` if `next is null`
	@property ATResN* leaf() pure {
		if (_leaf is null){
			if (_next is null)
				return _leaf = &this;
			return _leaf = next.leaf;
		}
		if (_leaf.next){
			_leaf._leaf = null;
			return _leaf = _leaf.leaf;
		}
		return _leaf;
	}
	/// pushes a new resolution at end of this list
	void push(ATResN* r) pure {
		leaf._next = r;
	}
}

/// Imports
package struct Imports{
	AModule[] imports; /// unnamed imports
	AModule[string] importsN; /// named imports
}

/// Gets a module, for importing
/// Returns: symbol table of a module
package AModule mod(string[] modId) pure {
	/// TODO: implement modSTab
	return AModule();
}
