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
			 std.traits;

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
		IdentU vis;
	}
	struct EndNode{
		/// value
		T val;
		/// only visible to contexts that begin with this IdentU
		IdentU vis;
		this (T val, IdentU vis) pure {
			this.val = val;
			this.vis = vis;
		}
	}
	struct STNode{
		/// value
		STab!T st;
		/// only visible to contexts that begin with this IdentU
		IdentU vis;
		this (STab!T st, IdentU vis) pure {
			this.st = st;
			this.vis = vis;
		}
	}

	/// the symbol table
	EndNode[][IdentU] map;
	/// sub-tables
	STNode[IdentU] next;

	static struct ResRange{
	private:
		IdentU _head;
		IdentU _id;
		EndNode[][IdentU][] _maps;

		this(IdentU id, IdentU[] ctx, STab!T st) pure {
			this._id = id;
			this._head = ctx.length ? ctx[0] : IdentU.init;
			_maps ~= st.map;
			foreach (IdentU i; ctx){
				if (i !in st.next)
					break;
				STNode nextNode = st.next[i];
				if (nextNode.vis != IdentU.init && nextNode.vis != _head)
					break;
				_maps ~= nextNode.st.map;
				st = nextNode.st;
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
			EndNode[] arr;
			if (_maps.length && (_id in _maps[$ - 1]) !is null)
				arr = _maps[$ - 1][_id];
			return arr.filter!(node => node.vis == IdentU.init || node.vis == _head);
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
	void valAdd(IdentU id, T val, IdentU vis) pure {
		if (auto ptr = id in map){
			*ptr ~= EndNode(val, vis);
			return;
		}
		map[id] = [EndNode(val, vis)];
	}
	/// ditto
	void valAdd(IdentU[] id, T val, IdentU vis) pure {
		STab!T st = this;
		IdentU[] ids = id.array;
		foreach (IdentU i; ids[0 .. $ - 1]){
			if (auto next = i in st.next){
				st = next.st;
			} else {
				assert(false);
			}
		}
		st.valAdd(ids[$ - 1], val, vis);
	}

	/// Add a new Symbol Table. **Will overwrite existing, if any.**
	void stAdd(IdentU id, STab!T st, IdentU vis) pure {
		next[id] = STNode(st, vis);
	}
	/// ditto
	void stAdd(IdentU[] id, STab!T stab, IdentU vis) pure {
		STab!T st = this;
		IdentU[] ids = id.array;
		foreach (IdentU i; ids[0 .. $ - 1]){
			if (auto next = i in st.next){
				st = next.st;
			} else {
				assert(false);
			}
		}
		st.stAdd(ids[$ - 1], stab, vis);
	}

	/// Returns: count of end nodes with unique IdentU at this level
	size_t endKeyCount() const pure {
		return map.length;
	}

	JSONValue toJson() const {
		JSONValue ret;
		foreach (IdentU key; map.byKey){
			ret[key.toString] = map[key]
				.map!((const EndNode n){
						JSONValue obj;
						static if (__traits(compiles, n.val.toJson)){
							obj["val"] = n.val.toJson;
						} else {
							obj["val"] = n.val.toString;
						}
						obj["vis"] = n.vis.toString;
						return obj;
						})
				.array;
		}
		foreach (IdentU key; next.byKey){
			immutable string s = key.toString;
			const STNode n = next[key];
			JSONValue obj;
			obj["val"] = n.st.toJson;
			obj["vis"] = n.vis.toString;
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

/// Gets symbol table of a module, for importing
/// Returns: symbol table of a module
package STab!ASymbol modSTab(string[] modId) pure {
	/// TODO: implement modSTab
	return new STab!ASymbol;
}
