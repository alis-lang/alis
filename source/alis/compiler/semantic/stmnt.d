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
			 alis.compiler.semantic.expr,
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

	void push(T val) pure {
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
		IdentU subCtx = format!"$_%d_%d_$"(
				node.pos.line, node.pos.col).IdentU;
		IdentU[] ctx = st.ctx ~ subCtx;
		st.stab.add(subCtx, new STab, []);
		RBlock r = new RBlock;
		r.pos = node.pos;
		foreach (Statement stmnt; node.statements){
			SmErrsVal!(RStatement[]) res = resolveStmnt(stmnt, st.stabR, ctx,
					st.dep, st.fns, st.rTypePtr, st.rNodes);
			if (res.isErr){
				st.errs ~= res.err;
				return;
			}
			r.statements ~= res.val;
		}
		st.res ~= r;
	}

	void returnIter(Return node, ref St st){
		RReturn ret = new RReturn;
		ret.pos = node.pos;
		ret.ctx = st.ctx;
		SmErrsVal!RExpr valRes = resolve(node.val, st.stabR, st.ctx,
				st.dep, st.fns);
		if (valRes.isErr){
			st.errs ~= valRes.err;
			return;
		}
		ret.val = valRes.val;
		if (st.rNodes !is null)
			st.rNodes.push(ret);
		st.res ~= ret;
	}

	void ifIter(If node, ref St st){
		size_t ec = st.errs.length;
		RIf ret = new RIf;
		ret.pos = node.pos;
		SmErrsVal!RExpr cnd = resolve(node.condition, st.stabR, st.ctx,
				st.dep, st.fns);
		SmErrsVal!(RStatement[]) onTrue = resolveStmnt(node.onTrue, st.stabR,
				st.ctx, st.dep, st.fns, st.rTypePtr, st.rNodes);
		ret.condition = cnd.val;
		ret.onTrue = onTrue.val;
		if (node.onFalse){
			SmErrsVal!(RStatement[]) onFalse = resolveStmnt(node.onFalse, st.stabR,
					st.ctx, st.dep, st.fns, st.rTypePtr, st.rNodes);
			if (onFalse.isErr){
				st.errs ~= onFalse.err;
			} else {
				ret.onFalse = onFalse.val;
			}
		}
		if (cnd.isErr)
			st.errs ~= cnd.err;
		if (onTrue.isErr)
			st.errs ~= onTrue.err;
		if (st.errs.length != ec) return;
		st.res ~= ret;
	}

	void forIter(For node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}

	void whileIter(While node, ref St st){
		immutable size_t ec = st.errs.length;
		RWhile ret = new RWhile;
		ret.pos = node.pos;
		SmErrsVal!RExpr cnd = resolve(node.condition, st.stabR, st.ctx,
				st.dep, st.fns);
		SmErrsVal!(RStatement[]) body = resolveStmnt(node.body, st.stabR,
				st.ctx, st.dep, st.fns, st.rTypePtr, st.rNodes);
		if (cnd.isErr)
			st.errs ~= cnd.err;
		if (body.isErr)
			st.errs ~= body.err;
		if (st.errs.length != ec) return;
		st.res ~= ret;
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
	st.returnTypeBuild(stmnt.pos);
	if (st.errs.length)
		return SmErrsVal!(RStatement[])(st.errs);
	if (st.res is null)
		return SmErrsVal!(RStatement[])([errUnxp(stmnt.pos, "resolve stmnt -> null")]);
	return SmErrsVal!(RStatement[])(st.res);
}

/// ditto
private SmErrsVal!(RStatement[]) resolveStmnt(Statement stmnt, STab stabR,
		IdentU[] ctx, void[0][ASymbol*] dep, RFn[string] fns,
		ADataType* rTypePtr, LList!RReturn* rNodes){
	St st;
	st.dep = dep;
	st.ctx = ctx.dup;
	st.stabR = stabR;
	st.stab = stabR.findSt(ctx, ctx);
	st.fns = fns;
	st.rTypePtr = rTypePtr;
	st.rNodes = rNodes;
	It.exec(stmnt, st);
	if (st.errs.length)
		return SmErrsVal!(RStatement[])(st.errs);
	if (st.res is null)
		return SmErrsVal!(RStatement[])([errUnxp(stmnt.pos, "resolve stmnt -> null")]);
	return SmErrsVal!(RStatement[])(st.res);
}

/// Builds return type for when expected return type is auto
private void returnTypeBuild(ref St st, Location pos){
	if (st.rTypePtr !is null){
		if (st.rNodes.empty){
			st.errs ~= errNoReturn(pos, st.rTypePtr.toString);
			return;
		}
		foreach (RReturn ret; *(st.rNodes)){
			if (!ret.val.type.canCastTo(*(st.rTypePtr), ret.ctx)){
				st.errs ~= errIncompatType(ret.pos, st.rTypePtr.toString,
						ret.val.type.toString);
				continue;
			}
			ret.val = ret.val.to(*(st.rTypePtr), ret.ctx).val;
		}
		return;
	}
	// TODO: build a union return type
}
