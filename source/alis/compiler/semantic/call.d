/++
Dealing with callables (functions, templates)
+/
module alis.compiler.semantic.call;

import alis.common,
			 alis.compiler.common,
			 alis.compiler.semantic.common,
			 alis.compiler.semantic.error,
			 alis.compiler.semantic.types,
			 alis.compiler.ast,
			 alis.compiler.ast.rst;

import std.algorithm,
			 std.format,
			 std.array,
			 std.conv;

debug import std.stdio;

/// calculates callability score.
/// `size_t.max` -> not callable
/// `0` -> highest callability
package size_t callabilityOf(ASymbol* sym, AValCT[] params, IdentU[] ctx){
	assert (sym !is null);
	assert(params.isFlat);
	final switch (sym.type){
		case ASymbol.Type.Struct:
			if (params.length != 1)
				return size_t.max;
			return params[0].canCastTo(ADataType.of(&sym.structS), ctx)
				? 0 : size_t.max;

		case ASymbol.Type.Union:
			if (params.length != 1)
				return size_t.max;
			return params[0].canCastTo(ADataType.of(&sym.unionS), ctx)
				? 0 : size_t.max;

		case ASymbol.Type.Enum:
			if (params.length != 1)
				return size_t.max;
			return params[0].canCastTo(ADataType.of(&sym.enumS), ctx)
				? 0 : size_t.max;

		case ASymbol.Type.Var:
		case ASymbol.Type.Import:
		case ASymbol.Type.EnumConst:
		case ASymbol.Type.UTest:
			return size_t.max;
		case ASymbol.Type.Alias:
			return size_t.max; // TODO: implement for Alias
		case ASymbol.Type.Template:
			return size_t.max; // TODO: implement for Template
		case ASymbol.Type.Fn:
			return callabilityOf(&sym.fnS, params, ctx);
	}
	assert(false);
}

/// ditto
private size_t callabilityOf(AFn* symC, AValCT[] params, IdentU[] ctx){
	assert(params.isFlat);
	if (params.length > symC.paramsT.length)
		return size_t.max;
	size_t ret = 0;
	foreach (size_t i; 0 .. symC.paramsT.length){
		if (i >= params.length){
			if (symC.paramsV[i].isVal)
				continue;
			return size_t.max;
		}
		if (!params[i].isVal){
			if (symC.paramsN[i] != "_")
				return size_t.max;
		}
		OptVal!ADataType type = params[i].asType;
		if (!type.isVal)
			return size_t.max;
		OptVal!CastLevel cl = type.val.castability(symC.paramsT[i], ctx);
		if (!cl.isVal)
			return size_t.max;
		if (cl.val > CastLevel.Ref)
			ret ++;
	}
	return ret;
}

/// ditto
package size_t callabilityOf(RExpr callee, AValCT[] params, IdentU[] ctx){
	if (RFnExpr fnExpr = cast(RFnExpr)callee){
		return callabilityOf(fnExpr.fn, params, ctx);
	}
	if (RAValCTExpr aValExpr = cast(RAValCTExpr)callee){
		AValCT val = aValExpr.res;
		final switch (val.type){
			case AValCT.Type.Expr:
				return callabilityOf(val.expr, params, ctx);
			case AValCT.Type.Literal:
			case AValCT.Type.Seq:
				return size_t.max;
			case AValCT.Type.Symbol:
				return callabilityOf(val.symS, params, ctx);
			case AValCT.Type.Type:
				if (params.length == 0 || !params[0].isVal)
					return size_t.max;
				return params[0].valType.val.canCastTo(val.typeT, ctx);
		}
	}
	if (!callee.hasType)
		return size_t.max;
	ADataType type = callee.type;
	if (type.type != ADataType.Type.Fn ||
			type.paramT.length != params.length)
		return size_t.max;
	size_t ret = 0;
	foreach (size_t i, const(ADataType) expected; type.paramT){
		if (!params[i].isVal)
			return size_t.max;
		OptVal!ADataType paramType = params[i].asType;
		OptVal!CastLevel cl = paramType.val.castability(expected, ctx);
		if (!cl.isVal)
			return size_t.max;
		if (cl.val > CastLevel.Ref)
			ret ++;
	}
	return ret;
}

/// generates RST for calling something.
/// Returns: SmErr[] or RExpr
package SmErrsVal!RExpr call(RExpr callee, Location pos, AValCT[] params,
		IdentU[] ctx = [IdentU.init]){
	if (RFnExpr fn = cast(RFnExpr)callee)
		return call(fn.fn, pos, params, ctx);
	if (RAValCTExpr val = cast(RAValCTExpr)callee){
		final switch (val.res.type){
			case AValCT.Type.Symbol:
				return call(val.res.symS, pos, params, ctx);
			case AValCT.Type.Literal:
				return SmErrsVal!RExpr([errNotCallable(pos, val.res.val.toString)]);
			case AValCT.Type.Type:
				if (params.length != 1)
					return SmErrsVal!RExpr([errParamCount(pos,
								val.res.typeT.toString, 1, params.length)]);
				OptVal!RExpr res = params[0].toRExpr.to(val.res.typeT, ctx);
				if (res.isVal)
					return SmErrsVal!RExpr(res.val);
				return SmErrsVal!RExpr([
						errIncompatType(pos, val.res.typeT.toString, params[0].toString)]);
			case AValCT.Type.Expr:
				return call(callee, pos, params, ctx);
			case AValCT.Type.Seq:
				return SmErrsVal!RExpr([errNotCallable(pos, val.res.toString)]);
		}
	}

	return SmErrsVal!RExpr([errUnsup(callee.pos,
				typeid(callee).to!string.format!"calling %s")]);
}

package SmErrsVal!RExpr call(ASymbol* sym, Location pos, AValCT[] params,
		IdentU[] ctx = [IdentU.init]){
	if (sym.type == ASymbol.Type.Fn)
		return call(&sym.fnS, pos, params, ctx);
	if (params.length != 1 || !params[0].isVal)
		return SmErrsVal!RExpr([errCallableIncompat(pos,
					sym.toString, params.map!(p => p.toString))]);
	ADataType type;
	switch (sym.type){
		case ASymbol.Type.Struct:
			type = ADataType.of(&sym.structS);
			break;
		case ASymbol.Type.Union:
			type = ADataType.of(&sym.unionS);
			break;
		case ASymbol.Type.Enum:
			type = ADataType.of(&sym.enumS);
			break;
		default:
	}
	if (type == ADataType.ofNoInit)
		return SmErrsVal!RExpr([errNotCallable(pos, ADataType.ofNoInit.toString)]);
	if (params.length != 1)
		return SmErrsVal!RExpr([errParamCount(pos,
					type.toString, 1, params.length)]);
	OptVal!RExpr res = params[0].toRExpr.to(type, ctx);
	if (res.isVal)
		return SmErrsVal!RExpr(res.val);
	return SmErrsVal!RExpr([
			errIncompatType(pos, type.toString, params[0].toString)]);
}

/// ditto
pragma(inline, true)
private SmErrsVal!RExpr call(AFn* fnSym, Location pos, AValCT[] params,
		IdentU[] ctx = [IdentU.init]){
	SmErr[] errs;
	RExpr[] casted = new RExpr[fnSym.paramsT.length];
	assert (params.length <= fnSym.paramsT.length);
	foreach (size_t i, AValCT paramVal; params){
		OptVal!RExpr res = paramVal.toRExpr.to(fnSym.paramsT[i], ctx);
		if (!res.isVal){
			errs ~= errIncompatType(pos, fnSym.paramsT[i].toString,
					params[i].toString);
			continue;
		}
		casted[i] = res.val;
	}
	if (errs.length)
		return SmErrsVal!RExpr(errs);
	foreach (size_t i; params.length .. fnSym.paramsT.length){
		if (!fnSym.paramsV[i].isVal){
			return SmErrsVal!RExpr([
					errCallableIncompat(pos, fnSym.toString,
						params.map!(p => p.toString))]);
		}
		RLiteralExpr val =
			new RLiteralExpr(AVal(fnSym.paramsT[i], fnSym.paramsV[i].val));
		val.pos = pos;
		casted[i] = val;
	}
	RFnCallExpr call = new RFnCallExpr(new RFnExpr(fnSym), casted);
	call.pos = pos;
	return SmErrsVal!RExpr(call);
}

/// ditto
private SmErrsVal!RExpr fnCall(RExpr expr, Location pos, AValCT[] params,
		IdentU[] ctx = [IdentU.init]){
	assert (cast(RFnExpr)expr is null);
	assert (cast(RAValCTExpr)expr is null);
	if (expr.type.type != ADataType.Type.Fn)
		return SmErrsVal!RExpr([errNotCallable(expr.pos, expr.type.toString)]);
	SmErr[] errs;
	RExpr[] casted = new RExpr[expr.type.paramT.length];
	assert (params.length == expr.type.paramT.length);
	foreach (size_t i, AValCT paramVal; params){
		OptVal!RExpr res = paramVal.toRExpr.to(expr.type.paramT[i], ctx);
		if (!res.isVal){
			errs ~= errIncompatType(expr.pos, expr.type.paramT[i].toString,
					params[i].toString);
			continue;
		}
		casted[i] = res.val;
	}
	if (errs.length)
		return SmErrsVal!RExpr(errs);
	RFnCallExpr call = new RFnCallExpr(expr, casted);
	call.pos = pos;
	return SmErrsVal!RExpr(call);
}
