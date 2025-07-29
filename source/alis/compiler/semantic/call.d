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

/// calculates callability score.
/// `size_t.max` -> not callable
/// `0` -> highest callability
package size_t callabilityOf(ASymbol* sym, AValCT[] params){
	return size_t.max; // TODO: kinda sad tbh, nothing is callable :(
}

/// ditto
package size_t callabilityOf(ADataType type, AValCT[] params){
	return size_t.max; // TODO: kinda sad tbh, nothing is callable :(
}

/// ditto
package size_t callabilityOf(AValCT val, AValCT[] params){
	return size_t.max; // TODO: kinda sad tbh, nothing is callable :(
}

/// ditto
package size_t callabilityOf(RExpr callee, AValCT[] params){
	return size_t.max; // TODO: kinda sad tbh, nothing is callable :(
}

/// generates RST for calling something.
/// works for RAValCTExpr as callee as well
/// Returns: SmErr[] or RExpr
package SmErrsVal!RExpr call(RExpr callee, AValCT[] params){
	RFnExpr fn = cast(RFnExpr)callee;
	if (fn is null){
		if (RAValCTExpr val = cast(RAValCTExpr)callee){
			final switch (val.res.type){
				case AValCT.Type.Symbol:
					ASymbol* sym = val.res.symS;
					if (sym.type == ASymbol.Type.Fn)
						return fnCall(&sym.fnS, params);

					// if symbol is type, do type conversion
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
							assert(false, "w o t");
					}
					if (type == ADataType.ofNoInit)
						break;
					if (params.length != 1)
						return SmErrsVal!RExpr([errParamCount(callee.pos,
									val.res.typeT.toString, 1, params.length)]);
					OptVal!RExpr res = params[0].toRExpr.to(type);
					if (res.isVal)
						return SmErrsVal!RExpr(res.val);
					return SmErrsVal!RExpr([
							errIncompatType(callee.pos, type.toString, params[0].toString)]);
					break;

				case AValCT.Type.Literal:
					break;
				case AValCT.Type.Type:
					// type conversion
					if (params.length != 1)
						return SmErrsVal!RExpr([errParamCount(callee.pos,
									val.res.typeT.toString, 1, params.length)]);
					OptVal!RExpr res = params[0].toRExpr.to(val.res.typeT);
					if (res.isVal)
						return SmErrsVal!RExpr(res.val);
					return SmErrsVal!RExpr([
							errIncompatType(callee.pos, val.res.typeT.toString,
								params[0].toString)]);
				case AValCT.Type.Expr:
					return SmErrsVal!RExpr([errUnsup(callee.pos,
								"AValCT.Type.Expr as callee")]);
				case AValCT.Type.Seq:
					return SmErrsVal!RExpr([errUnsup(callee.pos,
								"AValCT.Type.Seq as callee")]);
			}
		}
	}

	if (fn)
		return fnCall(fn, params);

	return SmErrsVal!RExpr([errUnsup(callee.pos,
				typeid(callee).to!string.format!"calling %s")]);
}

/// generates RST for calling a function
/// works for RAValCTExpr as callee as well
/// Returns: SmErr[] or RExpr
pragma(inline, true)
package SmErrsVal!RExpr fnCall(AFn* fnSym, AValCT[] params){
	return fnCall(new RFnExpr(fnSym), params);
}

/// ditto
package SmErrsVal!RExpr fnCall(RFnExpr callee, AValCT[] params){
	AFn* symC = callee.fn;
	SmErr[] errs;
	RExpr[] casted = new RExpr[symC.paramsT.length];
	assert (params.length <= symC.paramsT.length);
	foreach (size_t i, AValCT paramVal; params){
		OptVal!RExpr res = paramVal.toRExpr.to(symC.paramsT[i]);
		if (!res.isVal){
			errs ~= errIncompatType(callee.pos, symC.paramsT[i].toString,
					params[i].toString);
			continue;
		}
		casted[i] = res.val;
	}
	if (errs.length)
		return SmErrsVal!RExpr(errs);
	foreach (size_t i; params.length .. symC.paramsT.length){
		RLiteralExpr val =
			new RLiteralExpr(AVal(symC.paramsT[i], symC.paramsV[i]));
		val.pos = callee.pos;
		casted[i] = val;
	}
	RFnCallExpr call = new RFnCallExpr(callee, casted);
	call.pos = callee.pos;
	return SmErrsVal!RExpr(call);
}

/// ditto
package SmErrsVal!RExpr fnCall(RExpr expr, AValCT[] params){
	assert (cast(RFnExpr)expr is null);
	if (expr.type.type != ADataType.Type.Fn)
		return SmErrsVal!RExpr([errNotCallable(expr.pos, expr.type.toString)]);
	SmErr[] errs;
	RExpr[] casted = new RExpr[expr.type.paramT.length];
	assert (params.length == expr.type.paramT.length);
	foreach (size_t i, AValCT paramVal; params){
		OptVal!RExpr res = paramVal.toRExpr.to(expr.type.paramT[i]);
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
	call.pos = expr.pos;
	return SmErrsVal!RExpr(call);
}
