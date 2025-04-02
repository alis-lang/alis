/++
Semantic Analysis common stuff
+/
module alis.compiler.semantic.common;

import alis.common;

import std.algorithm,
			 std.range;

/// Symbol Table single level
struct STab1L(T){
private:
	struct EndNode{
		/// value
		T val;
		/// only visible to contexts that begin with this IdentU
		IdentU vis;
		this (T val, IdentU vis){
			this.val = val;
			this.vis = vis;
		}
	}
	struct STNode{
		/// value
		STab1L!T st;
		/// only visible to contexts that begin with this IdentU
		IdentU vis;
		this (STab1L!T st, IdentU vis){
			this.st = st;
			this.vis = vis;
		}
	}

	/// the symbol table
	EndNode[][IdentU] _map;
	/// sub-tables
	STNode[IdentU] _next;
public:

	static struct ResRange{
	private:
		IdentU _head;
		IdentU _id;
		EndNode[][IdentU][] _maps;

		this(IdentU id, Ident ctx, ref STab1L!T st) pure {
			this._id = id;
			IdentU[] scopes = ctx.array;
			this._head = scopes.length ? scopes[0] : IdentU.init;
			_maps ~= st._map;
			STab1L!T* s = &st;
			foreach (IdentU i; scopes){
				if (i !in s._next)
					break;
				STNode nextNode = s._next[i];
				if (nextNode.vis != IdentU.init && nextNode.vis != _head)
					break;
				_maps ~= nextNode.st._map;
				s = &nextNode.st;
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
	ResRange find(IdentU id, Ident ctx) pure {
		return ResRange(id, ctx, this);
	}

	/// Returns: true if id can be found from ctx scope
	bool canFind(IdentU id, Ident ctx) pure {
		return !find(id, ctx).empty;
	}

	/// Add a new value.
	void valAdd(IdentU id, T val, IdentU vis) pure {
		if (auto ptr = id in _map){
			*ptr ~= EndNode(val, vis);
			return;
		}
		_map[id] = [EndNode(val, vis)];
	}
	/// ditto
	void valAdd(Ident id, T val, IdentU vis) pure {
		STab1L!T* st = &this;
		IdentU[] ids = id.array;
		foreach (IdentU i; ids[0 .. $ - 1]){
			if (auto next = i in st._next){
				st = &next.st;
			} else {
				assert(false);
			}
		}
		st.valAdd(ids[$ - 1], val, vis);
	}

	/// Add a new Symbol Table. **Will overwrite existing, if any.**
	void stAdd(IdentU id, STab1L!T st, IdentU vis) pure {
		_next[id] = STNode(st, vis);
	}
	/// ditto
	void stAdd(Ident id, STab1L!T stab, IdentU vis) pure {
		STab1L!T* st = &this;
		IdentU[] ids = id.array;
		foreach (IdentU i; ids[0 .. $ - 1]){
			if (auto next = i in st._next){
				st = &next.st;
			} else {
				assert(false);
			}
		}
		st.stAdd(ids[$ - 1], stab, vis);
	}

	/// remove all symbols with an id
	/// Returns: true if done, false if does not exist
	bool removeAll(IdentU id) pure {
		if (id !in _map)
			return false;
		_map.remove(id);
		return true;
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
	Ident subject;
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
