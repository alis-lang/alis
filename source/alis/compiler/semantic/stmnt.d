/++
Statement Resolution (Statement -> RStatement conversion)
+/
module alis.compiler.semantic.stmnt;

import alis.common,
			 alis.compiler.common,
			 alis.compiler.semantic.common,
			 alis.compiler.semantic.error,
			 alis.compiler.semantic.sym0,
			 alis.compiler.semantic.sym1,
			 alis.compiler.semantic.eval,
			 alis.compiler.semantic.expr,
			 alis.compiler.semantic.types,
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
	/// whether return type is auto
	bool isAuto;
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

	this(LList!T from){
		head = from.head;
		tail = from.tail;
	}

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
		return head is null;
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

	void defIter(DefNode node, ref St st){
		S0R stab0; {
			SmErrsVal!S0R res = stab0Of(node, st.stab, st.ctx);
			if (res.isErr){
				st.errs ~= res.err;
				return;
			}
			stab0 = res.val;
		}
		S1R stab1; {
			SmErrsVal!S1R res = stab1Of(node, st.stabR, stab0.sMap, null, st.ctx);
			if (res.isErr){
				st.errs ~= res.err;
				return;
			}
			stab1 = res.val;
		}
		foreach (string k, RFn v; stab1.fns){
			assert (k !in st.fns);
			st.fns[k] = v;
		}
		foreach (const(ASymbol*) sym, RExpr val; stab1.initers){
			assert(sym.type == ASymbol.Type.Var);
			AVar symC = cast(AVar)(sym.varS);
			RVarRefExpr varRef = new RVarRefExpr(symC, false);
			varRef.pos = node.pos;
			RAssignExpr assign = new RAssignExpr();
			assign.pos = val.pos;
			assign.refExpr = varRef;
			assign.valExpr = val;
			st.res ~= assign;
		}
	}

	void blockIter(Block node, ref St st){
		IdentU subCtx = format!"$_%d_%d_$"(node.pos.line, node.pos.col).IdentU;
		IdentU[] ctx = st.ctx ~ subCtx;
		st.stab.add(subCtx, new STab, []);
		RBlock r = new RBlock;
		r.pos = node.pos;
		foreach (Statement stmnt; node.statements){
			SmErrsVal!(RStatement[]) res = resolveStmnt(stmnt, st.stabR, ctx,
					st.dep, st.fns, st.rTypePtr, st.isAuto, st.rNodes);
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
				st.ctx, st.dep, st.fns, st.rTypePtr, st.isAuto, st.rNodes);
		ret.condition = cnd.val;
		ret.onTrue = onTrue.val;
		if (node.onFalse){
			SmErrsVal!(RStatement[]) onFalse = resolveStmnt(node.onFalse, st.stabR,
					st.ctx, st.dep, st.fns, st.rTypePtr, st.isAuto, st.rNodes);
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
				st.ctx, st.dep, st.fns, st.rTypePtr, st.isAuto, st.rNodes);
		if (cnd.isErr)
			st.errs ~= cnd.err;
		if (body.isErr)
			st.errs ~= body.err;
		if (st.errs.length != ec) return;
		st.res ~= ret;
	}

	void doWhileIter(DoWhile node, ref St st){
		immutable size_t ec = st.errs.length;
		RDoWhile ret = new RDoWhile;
		ret.pos = node.pos;
		SmErrsVal!RExpr cnd = resolve(node.condition, st.stabR, st.ctx,
				st.dep, st.fns);
		SmErrsVal!(RStatement[]) body = resolveStmnt(node.body, st.stabR,
				st.ctx, st.dep, st.fns, st.rTypePtr, st.isAuto, st.rNodes);
		if (cnd.isErr)
			st.errs ~= cnd.err;
		if (body.isErr)
			st.errs ~= body.err;
		if (st.errs.length != ec) return;
		st.res ~= ret;

	}

	void switchIter(Switch node, ref St st){
		st.errs ~= errUnsup(node); // TODO: implement
	}

	void exprIter(Expression node, ref St st){
		SmErrsVal!RExpr res = resolve(node, st.stabR, st.ctx, st.dep, st.fns);
		if (res.isErr){
			st.errs ~= res.err;
			return;
		}
		st.res ~= res.val;
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
		IdentU[] ctx, void[0][ASymbol*] dep, RFn[string] fns,
		ADataType* rTypePtr, bool isAuto){
	assert (fns);
	St st;
	st.dep = dep;
	st.ctx = ctx.dup;
	st.stabR = stabR;
	st.stab = stabR.findSt(ctx, ctx);
	st.fns = fns;
	st.rTypePtr = rTypePtr;
	st.rNodes = new LList!RReturn;
	st.isAuto = isAuto;
	It.exec(stmnt, st);
	st.returnTypeBuild(stmnt.pos);
	if (st.errs.length)
		return SmErrsVal!(RStatement[])(st.errs);
	if (st.res is null)
		return SmErrsVal!(RStatement[])([
				errUnxp(stmnt.pos, "resolve stmnt -> null")]);
	return SmErrsVal!(RStatement[])(st.res);
}

/// ditto
private SmErrsVal!(RStatement[]) resolveStmnt(Statement stmnt, STab stabR,
		IdentU[] ctx, void[0][ASymbol*] dep, RFn[string] fns,
		ADataType* rTypePtr, bool isAuto, LList!RReturn* rNodes){
	assert (rTypePtr !is null);
	St st;
	st.dep = dep;
	st.ctx = ctx.dup;
	st.stabR = stabR;
	st.stab = stabR.findSt(ctx, ctx);
	st.fns = fns;
	st.rTypePtr = rTypePtr;
	st.rNodes = rNodes;
	st.isAuto = isAuto;
	It.exec(stmnt, st);
	if (st.errs.length)
		return SmErrsVal!(RStatement[])(st.errs);
	return SmErrsVal!(RStatement[])(st.res);
}

/// Builds return type for when expected return type is auto
private void returnTypeBuild(ref St st, Location pos){
	if (!st.isAuto){
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
	if (st.rNodes.empty){
		*st.rTypePtr = ADataType.init;
		return;
	}

	// maybe all the types are cast-able to one?
	ADataType* cType = LList!RReturn(*st.rNodes)
		.map!(n => n.val.type).array.commonType(st.ctx);
	if (cType != null){
		foreach (RReturn ret; *(st.rNodes)){
			if (!ret.val.type.canCastTo(*(cType), ret.ctx)){
				st.errs ~= errIncompatType(ret.pos, cType.toString,
						ret.val.type.toString);
				continue;
			}
			ret.val = ret.val.to(*(cType), ret.ctx).val;
		}
		*st.rTypePtr = *cType;
		return;
	}

	// sadly nothing worked. just build a union type
	ASymbol* sym = new ASymbol(AUnion());
	sym.isComplete = true;
	string name = format!"union$_%d_%d_$"(pos.line, pos.col);
	st.stab.add(name.IdentU, sym, st.ctx);
	AUnion* u = &sym.unionS;
	u.vis = Visibility.Pub;
	u.ident = st.ctx ~ name.IdentU;
	foreach (RReturn ret; LList!RReturn(*st.rNodes)){
		if (!u.types.canFind(ret.val.type))
			u.types ~= ret.val.type;
	}
	*st.rTypePtr = ADataType.of(u);
	foreach (RReturn ret; *st.rNodes){
		if (!ret.val.type.canCastTo(*(st.rTypePtr), st.ctx))
			assert (false, "w a t");
		ret.val = ret.val.to(*(st.rTypePtr), st.ctx).val;
	}
}
