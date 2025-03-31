/++
Semantic Analysis common stuff
+/
module alis.compiler.semantic.common;

import alis.common;

/// Symbol Table single level
struct STab1L(T){
private:
	struct Node{
		/// if this is a Symbol Table (true) or value (false)
		bool isST = false;
		union{
			/// value
			T val;
			/// symbol table
			STab1L!T st;
		}
		/// visibility limited to this head IdentU, if not `"_"`
		/// the actual limited visibility will be stored in symbol itself
		IdentU vis;
		@disable this();
		this (IdentU id, T val, IdentU vis){
			this.isST = false;
			this.val = val;
			this.vis = vis;
		}
		this (IdentU id, STab1L!T st, IdentU vis){
			this.isST = true;
			this.st = st;
			this.vis = vis;
		}
	}
	/// the symbol table
	Node[IdentU] _map;

public:
	/// Returns: true if an identifier can be resolved from within a scope `s`
	bool canFind(Ident subject, Ident s) const pure {
		// TODO: implement STab1L.canFind
		return false;
	}

	/// finds all candidates

	/// Add a new value. Will overwrite existing.
	void add(IdentU id, T val, IdentU vis) pure {
		_map[id] = Node(id, val, vis);
	}
	/// Add a new Symbol Table. Will overwrite existing.
	void add(IdentU id, STab1L!T st, IdentU vis) pure {
		_map[id] = Node(id, st, vis);
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
