/++
Semantic Analysis common stuff
+/
module alis.compiler.semantic.common;

import alis.common;

/// Symbol Table single level
struct STab1L(T){
private:
	struct Node{
		bool isST = false; /// if this is a Symbol Table (true) or value (false)
		union{
			T val; /// value
			STab1L!T st;
		}
		IdentU id; /// identifier
		Ident vis; /// visibility limited to this ident, if not null
		@disable this();
		this (IdentU id, T val, Ident vis){
			this.isST = false;
			this.val = val;
			this.id = id;
			this.vis = vis;
		}
		this (IdentU id, STab1L!T st, Ident vis){
			this.isST = true;
			this.st = st;
			this.id = id;
			this.vis = vis;
		}
	}
	/// the symbol table
	Node[IdentU] _map;

public:
	/// Returns: true if an identifier exists
	bool exists(IdentU id) const pure {
		return (id in _map) !is null;
	}
	/// ditto
	bool exists(Ident id) const pure {
		/// TODO: implement
		return false;
	}
	/// Add a new value. Will overwrite existing.
	void add(IdentU id, T val, Ident vis) pure {
		_map[id] = Node(id, val, vis);
	}
	/// Add a new Symbol Table. Will overwrite existing.
	void add(IdentU id, STab1L!T st, Ident vis) pure {
		_map[id] = Node(id, st, vis);
	}
	/// remove a value
	/// Returns: true if done, false if does not exist
	bool remove(IdentU id) pure {
		if (!exists(id))
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
