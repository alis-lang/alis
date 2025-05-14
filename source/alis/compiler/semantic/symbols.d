/++
Symbols Resolution
+/
module alis.compiler.semantic.symbols;

import std.algorithm,
			 std.typecons,
			 std.range,
			 std.traits,
			 std.format,
			 std.meta;

import utils.ds;

debug import std.stdio;

import alis.common,
			 alis.compiler.common,
			 alis.compiler.semantic.common,
			 alis.compiler.semantic.error,
			 alis.compiler.ast,
			 alis.compiler.ast.iter,
			 alis.compiler.rst;

import meta;

/// Gets imports at any node in the AST (yes I know it's a struct, it has an
/// opCall override)
///
/// Returns: Imports
private struct importsOf{
private:
	alias It = ASTIter!(globDefIter, localDefIter, importIter);
static:
	struct St{
		SmErr[] errs;
		Imports imp;
	}

	@ItFn void globDefIter(GlobDef n, ref St st){
		It.exec(n.def, st);
	}
	@ItFn void localDefIter(DefStatement n, ref St st){
		It.exec(n.def, st);
	}
	@ItFn void importIter(Import n, ref St st){
		if (n.name == "_"){
			if (n.name in st.imp.importsN){
				st.errs ~= errIdentReuse(n.pos, n.name);
				return;
			}
			st.imp.importsN[n.name] = n.moduleIdent.mod;
			return;
		}
		st.imp.imports ~= n.moduleIdent.mod;
	}

public static:
	SmErrsVal!Imports opCall(ASTNode node){
		St st;
		It.exec(node, st);
		if (st.errs)
			return SmErrsVal!Imports(st.errs);
		return SmErrsVal!Imports(st.imp);
	}
}

package struct exprRes{
private:
	alias It = ASTIter!(identExprIter);
static:
	struct St{
		SmErr[] errs;
		ASymbol[IdentU] syms;
		ASymbol[IdentU][] symStack;
		IdentU modId;
		RExpr expr;
	}

	@ItFn void identExprIter(IdentExpr node, ref St st){}
	// TODO make this
public:

	SmErrsVal!RExpr opCall(Expression expr, IdentU modId,
			ASymbol[IdentU][] symStack = null){
		St st;
		st.symStack = symStack.dup;
		if (st.symStack.length)
			st.syms = st.symStack[$ - 1];
		st.modId = modId;
		It.exec(expr, st);
		if (st.errs.length)
			return SmErrsVal!RExpr(st.errs);
		return SmErrsVal!RExpr(st.expr);
	}
}

package struct typeRes{
private:
	alias It = ASTIter!(identExprIter, noinitExpr, callExpr, dotExpr);
static:
	struct St{
		SmErr[] errs;
		STab!ASymbol stab;
		IdentU modId;
		ADataType type;
	}

	@ItFn void identExprIter(IdentExpr node, ref St st){
		st.errs ~= errUnsup(node);
	}

	@ItFn void noinitExpr(IntrNoInit, ref St st){
		st.type = ADataType.ofNoInit;
	}

	@ItFn void callExpr(OpCallExpr node, ref St st){
		if (auto sub = cast(IntrinsicExpr)node.callee){
			intrExpr(sub, node.params, st);
			return;
		}
	}

	@ItFn void dotExpr(OpDotBin node, ref St st){
		if (auto sub = cast(IntrinsicExpr)node.rhs){
			Expression[] params;
			if (auto cExpr = cast(CommaExpr)node.lhs){
				params = cExpr.exprs;
			} else {
				params = [node.lhs];
			}
			intrExpr(sub, params, st);
		}
	}

	void intrExpr(IntrinsicExpr node, Expression[] params, ref St st){
		switch (node.name){
			case "noinit":
				if (params.length != 0)
					st.errs ~= errParamCount(node, "$noinit", 0, params.length);
				st.type = ADataType.ofNoInit;
				return;

			case "int":
				if (params.length != 1)
					st.errs ~= errParamCount(node, "int", 1, params.length);
				SmErrsVal!AValCT xVal = exprEval(params[0], st.stab);
				if (xVal.isErr){
					st.errs ~= xVal.err;
					return;
				}
				AValCT x = xVal.val; // TODO: continue from here
				if (x.typeL != ADataType.ofInt){
					st.errs ~= errTypeMis(node, ADataType.ofInt, x.typeL);
					return;
				}
				st.type = ADataType.ofInt(); // TODO: decode x into int
				return;
			default:
				st.errs ~= errUnsup(node);
		}
	}

	// TODO make this
public:

	SmErrsVal!ADataType opCall(Expression expr, STab!ASymbol stab){
		St st;
		st.stab = stab;
		It.exec(expr, st);
		if (st.errs.length)
			return SmErrsVal!ADataType(st.errs);
		return SmErrsVal!ADataType(st.type);
	}
}

/// evaluates an Expression
/// Returns: AVAlCT, or SmErr[]
package SmErrsVal!AValCT exprEval(Expression expr, STab!ASymbol stab){
	// TODO: implement expression evaluation
	return SmErrsVal!AValCT(AValCT(ADataType.ofInt, 1024.asBytes));
}

package struct symOf{
private:
	alias It = ASTIter!(moduleIter,
			globDefIter, mixinInitDefIter, templateIter, cCompIter,
			mixinInitIter, globDefIter, enumConstIter, enumSmIter, structIter,
			varIter, aliasIter, unionIter, utestIter);
static:
	struct St{
		SmErr[] errs;
		STab!ASymbol stabMain;
		STab!ASymbol stab;
		IdentU[] ctx;
		Visibility[] visStack;
		Imports imports;
		IdentU modId;
	}

	@ItFn void moduleIter(Module mod, ref St st){
		st.stab = new STab!ASymbol;
		if (st.stabMain)
			st.stabMain.stAdd(mod.ident.IdentU, st.stab, Visibility.Pub, st.ctx);
		else
			st.stabMain = st.stab;
		if (st.ctx)
			st.ctx ~= mod.ident.IdentU;
		else
			st.ctx = [mod.ident.IdentU];
		It.descend(mod, st);
	}

	@ItFn void mixinInitDefIter(MixinInitDef node, ref St st){
		st.errs ~= errUnsup(node);
	}
	@ItFn void templateIter(TemplateDef node, ref St st){
		st.errs ~= errUnsup(node);
	}
	@ItFn void cCompIter(CCNode node, ref St st){
		st.errs ~= errUnsup(node);
	}
	@ItFn void mixinInitIter(MixinInit node, ref St st){
		st.errs ~= errUnsup(node);
	}

	@ItFn void globDefIter(GlobDef node, ref St st){
		st.visStack ~= node.visibility;
		It.descend(node, st);
	}
	@ItFn void enumConstIter(EnumConstDef node, ref St st){
		SmErrsVal!AEnumConst val = conv(node, st.stabMain,
				st.visStack[$ - 1], st.ctx);
		if (val.isErr){
			st.errs ~= val.err;
			return;
		}
		AEnumConst ret = val.val;
		st.stab.valAdd(ret.ident[$ - 1], ASymbol(ret), st.visStack[$ - 1], st.ctx);
	}
	@ItFn void enumSmIter(EnumSmDef node, ref St st){}
	@ItFn void structIter(StructDef node, ref St st){}
	@ItFn void varIter(VarDef node, ref St st){}
	@ItFn void aliasIter(AliasDef node, ref St st){}
	@ItFn void unionIter(UnionDef node, ref St st){}
	@ItFn void utestIter(UTest node, ref St st){}

public static:
	SmErrsVal!(STab!ASymbol) opCall(Module mod,
			STab!ASymbol stab = null, IdentU[] ctx = null){
		St st;
		SmErrsVal!Imports imports = mod.importsOf;
		if (imports.isErr)
			return SmErrsVal!(STab!ASymbol)(imports.err);
		st.imports = imports.val;
		st.stabMain = stab;
		st.ctx = ctx;
		It.exec(mod, st);
		if (st.errs)
			return SmErrsVal!(STab!ASymbol)(st.errs);
		return SmErrsVal!(STab!ASymbol)(st.stab);
	}
}

/// Builds AModule from Module
/// Returns: AModule
/*package SmErrsVal!AModule aModOf(Module node){
	SmErrsVal!(STab!ASymbol) val = symOf(node, null);
	if (val.isErr)
		return SmErrsVal!(Stack!ASymbol)(val.err);
	return SmErrsVal!(Stack!ASymbol)(AModule(ADT(), val.val));
}*/

/// converts an EnumConstDef to AEnumConst
/// Returns: AEnumConst, or errors
private SmErrsVal!AEnumConst conv(EnumConstDef node,
		STab!ASymbol stabMod,
		Visibility vis = Visibility.Default, IdentU[] ctx = null){
	AEnumConst ret;
	ret.ident = ctx ~ node.name.IdentU;
	ret.vis = vis;
	SmErrsVal!ADataType typeVal = typeRes(node.type, stabMod);
	if (typeVal.isErr)
		return SmErrsVal!AEnumConst(typeVal.err);
	ret.type = typeVal.val;
	SmErrsVal!AValCT dataVal = exprEval(node.val, stabMod);
	if (dataVal.isErr)
		return SmErrsVal!AEnumConst(dataVal.err);
	AValCT data = dataVal.val;
	if (data.type != AValCT.Type.Literal)
		return SmErrsVal!AEnumConst([errExprValExpected(node.val)]);
	ret.data = data.dataL;
	return SmErrsVal!AEnumConst(ret);
}
