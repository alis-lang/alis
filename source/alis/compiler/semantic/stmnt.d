/++
Statement Resolution (Statement -> RStatement conversion)
+/
module alis.compiler.semantic.stmnt;

import alis.common,
			 alis.compiler.common,
			 alis.compiler.semantic.common,
			 alis.compiler.semantic.error,
			 alis.compiler.semantic.sym0,
			 alis.compiler.semantic.eval,
			 alis.compiler.ast,
			 alis.compiler.ast.iter,
			 alis.compiler.ast.rst;

import meta;

import std.algorithm,
			 std.array,
			 std.range,
			 std.format;

debug import std.stdio;

private struct St{
	/// errors
	SmErr[] errs;
	/// root stab
	STab stabR;
	/// local STab
	STab stab;
	/// context
	IdentU[] ctx;
	/// symbols dependent on current call
	void[0][ASymbol*] dep;
	/// resulting expression(s)
	RStatement[] res;
	/// expected return type. can be null if auto expected
	ADataType* rTypePtr;
	/// resolved `return` nodes
	LList!RReturn* rNodes;
	/// `RFn` for each `AFn.uid`
	RFn[string] fns;
}

private struct LList(T){
	private struct Node{
		T val;
		Node* next;
	}
	Node* head;
	Node* tail;

	void add(T val) pure {
		if (head is null){
			head = new Node(val, null);
			tail = head;
			return;
		}
		tail.next = new Node(val, null);
	}

	@property T front() pure {
		assert (head !is null);
		return head.val;
	}
	@property bool empty() const pure {
		return head !is null;
	}
	void popFront() pure {
		assert (head !is null);
		head = head.next;
	}
}

private alias It = ItL!(mixin(__MODULE__), 0);

@ItFn @ITL(0){
	void staticIfIter(StaticIf node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}

	void staticForIter(StaticFor node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}

	void staticSwitchIter(StaticSwitch node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}

	void mixinInitStmntIter(MixinInitStmnt node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}

	void blockIter(Block node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
		RBlock r = new RBlock;
		r.pos = node.pos;
	}

	void returnIter(Return node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}

	void ifIter(If node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}

	void forIter(For node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}

	void whileIter(While node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}

	void doWhileIter(DoWhile node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}

	void switchIter(Switch node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}
}

/// Resolves Statement to RStatement
/// Params:
/// - `stmnt` - The statemant to resolve
/// - `stab` - The root level Symbol Table
/// - `ctx` - Context where the `expr` occurs
/// Returns: RStatement or SmErr[]
pragma(inline, true)
package SmErrsVal!(RStatement[]) resolveStmnt(Statement stmnt, STab stabR,
		IdentU[] ctx, void[0][ASymbol*] dep, RFn[string] fns, ADataType* rTypePtr){
	assert (fns);
	St st;
	st.dep = dep;
	st.ctx = ctx.dup;
	st.stabR = stabR;
	st.stab = stabR.findSt(ctx, ctx);
	st.fns = fns;
	st.rTypePtr = rTypePtr;
	It.exec(stmnt, st);
	if (st.errs.length)
		return SmErrsVal!(RStatement[])(st.errs);
	if (st.res is null)
		return SmErrsVal!(RStatement[])([errUnxp(stmnt.pos, "resolve stmnt -> null")]);
	return SmErrsVal!(RStatement[])(st.res);
}
