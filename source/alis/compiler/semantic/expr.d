/++
Expression Resolution (Expression -> RExpr conversion)
+/
module alis.compiler.semantic.expr;

import alis.common,
			 alis.compiler.common,
			 alis.compiler.semantic.common,
			 alis.compiler.semantic.error,
			 alis.compiler.semantic.symbols,
			 alis.compiler.ast,
			 alis.compiler.ast.iter,
			 alis.compiler.rst;

import meta;

/// Expression Resolver (to RExpr)
package struct ExpressionResolver{
	@disable this();
private:
	alias It = ASTIter!(identExprIter, noinitExpr, callExpr, dotExpr);
static:
	struct St{
		/// errors
		SmErr[] errs;
		/// main STab, for lookups
		STab stabMain;
		/// local STab
		STab stab;
		/// context
		IdentU[] ctx;
		/// resulting expression
		RExpr res;
		/// parameter types if resuslting expression is expected to be callable
		ADataType[] paramsT;
	}

	@ItFn void identExprIter(IdentExpr node, ref St st){
		st.errs ~= errUnsup(node); // TODO
	}

	@ItFn void noinitExpr(IntrNoInit node, ref St st){
		RIntrinsicExpr res = new RIntrinsicExpr;
		res.name = node.name;
		st.res = res;
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
		// TODO: implement IntrinsicExpr -> RExpr
		/*switch (node.name){
			case "noinit":
				if (params.length != 0)
					st.errs ~= errParamCount(node, "$noinit", 0, params.length);
				st.type = ADataType.ofNoInit;
				return;

			case "int":
				if (params.length != 1)
					st.errs ~= errParamCount(node, "int", 1, params.length);
				SmErrsVal!AValCT xVal = eval(params[0], st.stab);
				if (xVal.isErr){
					st.errs ~= xVal.err;
					return;
				}
				AValCT x = xVal.val;
				if (x.typeL != ADataType.ofInt){
					st.errs ~= errTypeMis(node, ADataType.ofInt, x.typeL);
					return;
				}
				st.type = ADataType.ofInt(); // TODO: decode x into int
				return;
			default:
				st.errs ~= errUnsup(node);
		}*/
	}
public static:

	/// Resolves Expression to RExpr
	/// Returns: RExpr, or SmErr[]
	SmErrsVal!RExpr resolve(Expression expr, STab stab, IdentU[] ctx,
			ADataType[] paramsT = null){
		St st;
		st.ctx = ctx.dup;
		st.stabMain = stab;
		st.stab = stab;
		st.paramsT = paramsT.dup;
		It.exec(expr, st);
		if (st.errs.length)
			return SmErrsVal!RExpr(st.errs);
		return SmErrsVal!RExpr(st.res);
	}
}

/// Resolves Expression to RExpr
/// Params:
/// - `expr` - The expression to resolve
/// - `stab` - The root level Symbol Table
/// - `ctx` - Context where the `expr` occurs
/// - `paramsT` - Parameter Data Types if `expr` is to be used as a callable
/// Returns: RExpr or SmErr[]
pragma(inline, true)
package SmErrsVal!RExpr resolve(Expression expr, STab stab, IdentU[] ctx,
		ADataType[] paramsT = null){
	return ExpressionResolver.resolve(expr, stab, ctx, paramsT);
}
