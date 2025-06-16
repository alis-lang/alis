/++
Semantic Analysis common stuff
+/
module alis.compiler.semantic.common;

import alis.common,
			 alis.compiler.ast,
			 alis.compiler.ast.rst,
			 meta;

import std.algorithm,
			 std.range,
			 std.json,
			 std.meta,
			 std.traits,
			 std.conv;

debug import std.stdio;

/// Iterator Function Level
package struct ITL{
	size_t level;
	@disable this();
	this(size_t level){
		this.level = level;
	}
}

/// AST Iterator, using ItFns from module `M`, that are tagged with `ITL(L)`
package template ItL(alias M, size_t L){
	// yes, I know Fns not used. go ahead, remove it, I dare you.
	alias Fns = AliasSeq!();
	static foreach (F; ItFnsOf!M){
		static if (hasUDA!(F, ITL(L))){
			Fns = AliasSeq!(Fns, F);
		}
	}
	alias ItL = ASTIter!(Filter!(HasAnyUDA!(ITL(L)), ItFnsOf!M));
}

/// RST Iterator, using ItFns from module `M`, that are tagged with `ITL(L)`
package template RtL(alias M, size_t L){
	// yes, I know Fns not used. go ahead, remove it, I dare you.
	alias Fns = AliasSeq!();
	static foreach (F; ItFnsOf!M){
		static if (hasUDA!(F, ITL(L))){
			Fns = AliasSeq!(Fns, F);
		}
	}
	alias RtL = RSTIter!(Filter!(HasAnyUDA!(ITL(L)), ItFnsOf!M));
}

/// Semantic Analysis Context
package struct SmCtx{
	@disable this();
	/// symbol table local
	STab stab;
	/// symbol table root
	STab stabR;
	/// context
	IdentU[] ctx;
	/// symbols dependent upon current call
	void[0][ASymbol*] dep;

	this(STab stabR, void[0][ASymbol*] dep, IdentU[] ctx){
		this.stabR = stabR;
		this.ctx = ctx;
		this.dep = dep;
	}
	this(STab stab, STab stabR, void[0][ASymbol*] dep, IdentU[] ctx){
		this.stab = stab;
		this.stabR = stabR;
		this.ctx = ctx;
		this.dep = dep;
	}
}

/// Symbol Table
public final class STab{
public:
	struct Node(E){
		E val;
		IdentU[] vis;
		this (E val, IdentU[] vis) pure {
			this.val = val;
			this.vis = vis.dup;
		}
		/// Returns: whether this is visible from a ctx
		bool isVis(IdentU[] ctx) const pure {
			if (ctx.isNoId || vis.isNoId)
				return true;
			if (ctx.length < vis.length)
				return false;
			return vis == ctx[0 .. vis.length];
		}
	}

	/// the symbol table
	Node!(ASymbol*)[][IdentU] map;
	/// sub-tables
	Node!STab[IdentU] next;

	static struct ResRange{
	private:
		IdentU _id;
		IdentU[] _ctx;
		Node!(ASymbol*)[][IdentU][] _maps;

		this(IdentU id, IdentU[] ctx, STab st) pure {
			this._id = id;
			this._ctx = ctx.dup;
			_maps ~= st.map;
			foreach (IdentU i; _ctx){
				if (i !in st.next)
					break;
				Node!STab nextNode = st.next[i];
				if (!nextNode.isVis(_ctx))
					break;
				_maps ~= nextNode.val.map;
				st = nextNode.val;
			}
			_maps ~= ForeachType!(typeof(_maps)).init;
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
			Node!(ASymbol*)[] arr;
			if (_maps.length && (_id in _maps[$ - 1]) !is null)
				arr = _maps[$ - 1][_id];
			return arr
				.filter!(node => node.isVis(_ctx))
				.map!(node => node.val);
		}
	}

	/// finds all candidates that match an IdentU, given a scope ctx
	/// Returns: range of candidates
	ResRange find(IdentU id, IdentU[] ctx) pure {
		return ResRange(id, ctx, this);
	}

	/// Returns: true if id can be found from ctx scope
	bool canFind(IdentU id, IdentU[] ctx) pure {
		return !(find(id, ctx).empty);
	}

	/// finds STab
	/// Returns: STab or null
	STab findSt(IdentU id, IdentU[] ctx) pure {
		if (Node!STab* ptr = id in next){
			if (ptr.isVis(ctx))
				return ptr.val;
		}
		return null;
	}

	/// ditto
	STab findSt(IdentU[] id, IdentU[] ctx) pure {
		STab ret = this;
		foreach (IdentU i; id){
			ret = ret.findSt(i, ctx);
			if (ret is null)
				return null;
		}
		return ret;
	}

	/// Add a new value.
	void add(IdentU id, ASymbol* sym, IdentU[] vis) pure {
		if (!vis.length)
			vis = [IdentU.init];
		if (auto ptr = id in map){
			*ptr ~= Node!(ASymbol*)(sym, vis);
			return;
		}
		map[id] = [Node!(ASymbol*)(sym, vis)];
	}
	/// ditto
	void add(IdentU id, ASymbol* sym, Visibility vis, IdentU[] ctx) pure {
		return add(id, sym, vis == Visibility.Default ? ctx : [IdentU.init]);
	}

	/// Add a new Symbol Table. **Will overwrite existing, if any.**
	void add(IdentU id, STab st, IdentU[] vis) pure {
		if (!vis.length)
			vis = [IdentU.init];
		next[id] = Node!STab(st, vis);
	}
	/// ditto
	void add(IdentU id, STab st, Visibility vis, IdentU[] ctx) pure {
		return add(id, st, vis == Visibility.Default ? ctx : [IdentU.init]);
	}

	/// Returns: nonCallable symbol at this level, by its IdentU. Will return
	/// `null` if none present, or none visible.
	ASymbol* localNonCallable(IdentU id, IdentU[] ctx) pure {
		if (id !in map)
			return null;
		auto range = map[id]
			.filter!(node => node.isVis(ctx) && !node.val.isCallable);
		if (range.empty)
			return null;
		return range.front.val;
	}

	/// Returns: whether a nonCallable symbol exists by certain IdentU, at this
	/// level
	@property bool hasLocalNonCallable(IdentU id, IdentU[] ctx) pure {
		return localNonCallable(id, ctx) !is null;
	}

	JSONValue jsonOf() const {
		JSONValue ret;
		foreach (IdentU key; map.byKey){
			ret[key.toString] = map[key]
				.map!((const Node!(ASymbol*) n){
						JSONValue obj;
						static if (__traits(compiles, n.val.jsonOf)){
							obj["val"] = n.val.jsonOf;
						} else {
							obj["val"] = n.val.toString;
						}
						obj["vis"] = n.vis.to!string;
						return obj;
						})
				.array;
		}
		if (ret.isNull)
			ret = JSONValue.emptyObject;
		foreach (IdentU key; next.byKey){
			immutable string s = key.toString;
			const Node!STab n = next[key];
			JSONValue obj;
			obj["val"] = n.val.jsonOf;
			obj["vis"] = n.vis.to!string;
			if (s in ret.object)
				ret[s] ~= obj;
			else
				ret[s] = [obj];
		}
		return ret;
	}

	override string toString() const {
		return jsonOf.toPrettyString;
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
	ASymbol* sym;

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
