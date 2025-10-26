module alis.compiler.parser.preced;

import alis.compiler.lexer;

import std.string,
			 std.array,
			 std.algorithm;

/// operators in precedence
private static immutable OpsP = [
	[`A . B`, `A -> B`],
	[`A [ B`], // ]
	[`@ A`],
	[`A ( B`], // )
	["( A"], // )
	[`# A`, `const A`],
	[`A ?`, `A ?? B`, `A ++`, `A --`],
	[`A @`, `A ...`],
	[`~ A`, `is A`, `!is A`, `! A`],
	[`A * B`, `A / B`, `A % B`],
	[`A + B`, `A - B`],
	[`A << B`, `A >> B`],
	[`A & B`, `A | B`, `A ^ B`],
	[`A : B`, `A == B`, `A != B`, `A >= B`, `A <= B`, `A > B`, `A < B`, `A is B`,
		`A !is B`],
	[`A && B`, `A || B`],
	null, // marker for DEF_P
	["A { B"], // }
	[`A = B`, `A += B`, `A -= B`, `A *= B`, `A /= B`, `A %= B`, `A &= B`,
		`A |= B`, `A ^= B`],
	["A , B"],
];

/// Binary operators
public static immutable BinOps = BinOpsP.joiner.array;
/// Prefix operators
public static immutable PreOps = PreOpsP.joiner.array;
/// Postfix operators
public static immutable PostOps = PostOpsP.joiner.array;

/// Default Precedence
public enum DEF_P = OpsP.length - OpsP.countUntil(null);
/// Max precedence
public enum MAX_P = 0;

/// Precedence of a Binary operator
public template PrecedOfBin(string Op){
	enum PrecedOfBin = findPreced();
	byte findPreced(){
		foreach (size_t P, immutable string[] Ops; OpsP){
			if (Ops.filter!(o => o.isBinOp).map!(o => o.opStrip).canFind(Op))
				return cast(byte)(OpsP.length - P);
		}
		return 0;
	}
}

/// Precedence of a Prefix operator
public template PrecedOfPre(string Op){
	enum PrecedOfPre = findPreced();
	ubyte findPreced(){
		foreach (size_t P, immutable string[] Ops; OpsP){
			if (Ops.filter!(o => o.isPreOp).map!(o => o.opStrip).canFind(Op))
				return cast(ubyte)(OpsP.length - P);
		}
		return 0;
	}
}

/// Precedence of a Postfix operator
public template PrecedOfPost(string Op){
	enum PrecedOfPost = findPreced();
	ubyte findPreced(){
		foreach (size_t P, immutable string[] Ops; OpsP){
			if (Ops.filter!(o => o.isPostOp).map!(o => o.opStrip).canFind(Op))
				return cast(ubyte)(OpsP.length - P);
		}
		return 0;
	}
}

/// binary operators in precedence
private static immutable BinOpsP = OpsP
	.map!(ops => ops.filter!(op => op.isBinOp).map!(op => op.opStrip).array)
	.array;

/// prefix operators in precedence
private static immutable PreOpsP = OpsP
	.map!(ops => ops.filter!(op => op.isPreOp).map!(op => op.opStrip).array)
	.array;

/// postfix operators in precedence
private static immutable PostOpsP = OpsP
	.map!(ops => ops.filter!(op => op.isPostOp).map!(op => op.opStrip).array)
	.array;

/// Gets stripped operator
private string opStrip(string s){
	return s.replace("A", "").replace("B", "").strip;
}
///
unittest{
	assert("A + B".opStrip == "+");
}

/// Whether a string is a binary operator
private bool isBinOp(string s){
	return s.canFind('A') && s.canFind('B');
}
///
unittest{
	assert(isBinOp("A + B") == true);
	assert(isBinOp("+ B") == false);
	assert(isBinOp("A +") == false);
}

/// Whether a string is a prefix operator
private bool isPreOp(string s){
	return s.canFind('A') && !s.canFind('B') && s[$-1] == 'A';
}
///
unittest{
	assert(isPreOp("A + B") == false);
	assert(isPreOp("+ A") == true);
	assert(isPreOp("A +") == false);
}

/// Whether a string is a prefix operator
private bool isPostOp(string s){
	return s.canFind('A') && !s.canFind('B') && s[0] == 'A';
}
///
unittest{
	assert(isPostOp("A + B") == false);
	assert(isPostOp("+ A") == false);
	assert(isPostOp("A +") == true);
}
