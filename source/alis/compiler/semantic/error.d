/++
Alis Semantic Errors
+/
module alis.compiler.semantic.error;

import alis.compiler.common;

/// Value or SmErr
alias SmErrVal(T) = ErrVal!(T, SmErr);
/// Value or SmErrs
alias SmErrsVal(T) = ErrVal!(T, SmErr[]);

/// Alis Semantic Error
struct SmErr{
	/// Possible Error Types
	enum Type{
		IdentReuse, /// Same identifier used across definitions
	}
}
