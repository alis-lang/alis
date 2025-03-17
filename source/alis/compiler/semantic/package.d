/++
Semantic Analysis Package
+/
module alis.compiler.semantic;

import alis.common,
			 alis.compiler.ast;

/// Alis Template Resolution Node. TODO: move this to semantic package
public struct ATResN{
private:
	ATResN* _leaf;
	ATResN* _next;
	ATResN* _prev;

public:
	/// what is being resolved
	Ident* subject;
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
