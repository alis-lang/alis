/+
semantic.intrinsics package
+/
module alis.compiler.semantic.intrinsics;

import alis.compiler.semantic.intrinsics.intrns;
static import cmn = alis.compiler.semantic.intrinsics.common;

public alias callabilityOf = cmn.callabilityOf!CallabilityCheckers;
public alias resolve = cmn.resolve!ExprTranslators;
