module alis.compiler.codegen.main;
import alis.compiler.common;
import alis.common;
import alis.compiler.rst;
import std.stdio;
import std.file;

import std.array;
import std.conv;
import std.format;
import std.algorithm; 

const string testFolder = "tests/codegen/";

class BytecodeGenerator {
	private string[][] bytecodeInstructions;
	private int labelCounter = 0;
	private int stackOffset = 0;
	private int[string] variableOffsets; // Map of variable names to their stack offsets


	/// Generates bytecode of a statement
	/// 
	/// Params:
	///   statement = A Resolved statement like `RIf`, `RFn`
	/// Returns: Bytecode
	string[][] generateBytecode(RStatement statement) {
		bytecodeInstructions.length = 0;
		labelCounter = 0;
		stackOffset = 0;
		variableOffsets = null; // Clear variable offsets map

		generateStatementBytecode(statement);
		return bytecodeInstructions;
	}

	/// Utility function that generates bytecode for a given sequence of statements.
	///
	/// Params:
	/// 	statements = An array of statements to process.
	private void generateStatementsBytecode(RStatement[] statements){
		foreach (statement ; statements){
			generateStatementBytecode(statement);				
		}
	}

	/// Pushes an instruction.
	/// 
	/// Params:
	/// instruction = The instruction to be added.
	/// params = Additional parameters for the instruction (optional).
	private void addInstruction(string instruction, string[] params = []) {
		bytecodeInstructions ~= [instruction] ~ params;
	}

	/// Updates the variable offsets map with the variables in the given block
	/// 
	/// Params:
	///   block = Block containing local variables
	///   startOffset = The starting offset for the variables
	/// Returns: The new offset after all variables
	private int updateBlockVariableOffsets(RBlock block, int startOffset) {
		int offset = startOffset;
		
		// Map local variables to their offsets
		foreach (i, name; block.localsN) {
			variableOffsets[name] = offset;
			offset += cast(int)block.localsT[i].sizeOf;
		}
		
		return offset;
	}

	

	/// Gets the offset for a variable name
	/// 
	/// Params:
	///   name = Variable name to look up
	/// Returns: The offset or -1 if not found
	private int getVariableOffset(string name) {
		if (name in variableOffsets) {
			return variableOffsets[name];
		}
		return -1; // Variable not found
	}

	/// Determines the type of the statement and dispatches it 
	/// to the appropriate bytecode generation function.
	///
	/// Params:
	///    statement = The statement to generate bytecode for.
	private void generateStatementBytecode(RStatement statement) {
		if (auto block = cast(RBlock)statement) {
			generateBlockBytecode(block);
		} else if (auto returnStmt = cast(RReturn)statement) {
			generateReturnBytecode(returnStmt);
		} else if (auto ifStmt = cast(RIf)statement) {
			generateIfBytecode(ifStmt);
		} else if (auto forStmt = cast(RFor)statement) {
			generateForBytecode(forStmt);
		} else if (auto whileStmt = cast(RWhile)statement) {
			generateWhileBytecode(whileStmt);
		} else if (auto doWhileStmt = cast(RDoWhile)statement) {
			generateDoWhileBytecode(doWhileStmt);
		} else if (auto switchStmt = cast(RSwitch)statement) {
			generateSwitchBytecode(switchStmt);
		} else if (auto expr = cast(RExpr)statement) {
			generateExpressionBytecode(expr);
		} else {
			throw new Exception("Unsupported statement type: " ~ typeid(statement).toString);
		}
	}

	/// Determines the type of the expression and dispatches it 
	/// to the appropriate bytecode generation function.
	///
	/// Params:
	///    expr = The expression to generate bytecode for.
	private void generateExpressionBytecode(RExpr expr) {
		if (auto identExpr = cast(RIdentExpr)expr) {
			generateIdentExprBytecode(identExpr);
		} else if (auto blockExpr = cast(RBlockExpr)expr) {
			generateBlockExprBytecode(blockExpr);
		} else if (auto intrinsicExpr = cast(RIntrinsicExpr)expr) {
			generateIntrinsicExprBytecode(intrinsicExpr);
		} else if (auto intrinsicCallExpr = cast(RIntrinsicCallExpr)expr) {
			generateIntrinsicCallExprBytecode(intrinsicCallExpr);
		} else if (auto dTypeExpr = cast(RDTypeExpr)expr) {
			generateDTypeExprBytecode(dTypeExpr);
		} else if (auto assignExpr = cast(RAssignExpr)expr) {
			generateAssignExprBytecode(assignExpr);
		} else if (auto refAssignExpr = cast(RRefAssignExpr)expr) {
			generateRefAssignExprBytecode(refAssignExpr);
		} else if (auto derefExpr = cast(RDerefExpr)expr) {
			generateDerefExprBytecode(derefExpr);
		} else if (auto commaExpr = cast(RCommaExpr)expr) {
			generateCommaExprBytecode(commaExpr);
		} else if (auto fnCallExpr = cast(RFnCallExpr)expr) {
			generateFnCallExprBytecode(fnCallExpr);
		} else if (auto preIsExpr = cast(RPreIsExpr)expr) {
			generatePreIsExprBytecode(preIsExpr);
		} else if (auto preNotIsExpr = cast(RPreNotIsExpr)expr) {
			generatePreNotIsExprBytecode(preNotIsExpr);
		} else if (auto refExpr = cast(RRefExpr)expr) {
			generateRefExprBytecode(refExpr);
		} else if (auto vtGetExpr = cast(RVTGetExpr)expr) {
			generateVTGetExprBytecode(vtGetExpr);
		} else if (auto memberGetExpr = cast(RMemberGetExpr)expr) {
			generateMemberGetExprBytecode(memberGetExpr);
		} else if (auto fnExpr = cast(RFnExpr)expr) {
			generateFnExprBytecode(fnExpr);
		} else if (auto structLiteralExpr = cast(RStructLiteralExpr)expr) {
			generateStructLiteralExprBytecode(structLiteralExpr);
		} else if (auto arrayLiteralExpr = cast(RArrayLiteralExpr)expr) {
			generateArrayLiteralExprBytecode(arrayLiteralExpr);
		} else if (auto literalExpr = cast(RLiteralExpr)expr) {
			generateLiteralExprBytecode(literalExpr);
		} else {
			throw new Exception("Unsupported expression type: " ~ typeid(expr).toString);
		}
	}

	/// Generates bytecode for a block statement
	private void generateBlockBytecode(RBlock block) {
		// TODO: Implement block statement bytecode generation
	}

	/// Generates bytecode for a return statement
	private void generateReturnBytecode(RReturn returnStmt) {
		// TODO: Implement return statement bytecode generation
	}

	/// Generates bytecode for an if statement
	private void generateIfBytecode(RIf ifStmt) {
		// TODO: Implement if statement bytecode generation
	}

	/// Generates bytecode for a for statement
	private void generateForBytecode(RFor forStmt) {
		// TODO: Implement for statement bytecode generation
	}

	/// Generates bytecode for a while statement
	private void generateWhileBytecode(RWhile whileStmt) {
		// TODO: Implement while statement bytecode generation
	}

	/// Generates bytecode for a do-while statement
	private void generateDoWhileBytecode(RDoWhile doWhileStmt) {
		// TODO: Implement do-while statement bytecode generation
	}

	/// Generates bytecode for a switch statement
	private void generateSwitchBytecode(RSwitch switchStmt) {
		// TODO: Implement switch statement bytecode generation
	}

	/// Generates bytecode for an identifier expression
	private void generateIdentExprBytecode(RIdentExpr expr) {
		// TODO: Implement identifier expression bytecode generation
	}

	/// Generates bytecode for a block expression
	private void generateBlockExprBytecode(RBlockExpr expr) {
		// TODO: Implement block expression bytecode generation
	}

	/// Generates bytecode for an intrinsic expression
	private void generateIntrinsicExprBytecode(RIntrinsicExpr expr) {
		// TODO: Implement intrinsic expression bytecode generation
	}

	/// Generates bytecode for an intrinsic call expression
	private void generateIntrinsicCallExprBytecode(RIntrinsicCallExpr expr) {
		// TODO: Implement intrinsic call expression bytecode generation
	}

	/// Generates bytecode for a data type expression
	private void generateDTypeExprBytecode(RDTypeExpr expr) {
		// TODO: Implement data type expression bytecode generation
	}

	/// Generates bytecode for an assignment expression
	private void generateAssignExprBytecode(RAssignExpr expr) {
		// TODO: Implement assignment expression bytecode generation
	}

	/// Generates bytecode for a reference assignment expression
	private void generateRefAssignExprBytecode(RRefAssignExpr expr) {
		// TODO: Implement reference assignment expression bytecode generation
	}

	/// Generates bytecode for a dereference expression
	private void generateDerefExprBytecode(RDerefExpr expr) {
		// TODO: Implement dereference expression bytecode generation
	}

	/// Generates bytecode for a comma expression
	private void generateCommaExprBytecode(RCommaExpr expr) {
		// TODO: Implement comma expression bytecode generation
	}

	/// Generates bytecode for a function call expression
	private void generateFnCallExprBytecode(RFnCallExpr expr) {
		// TODO: Implement function call expression bytecode generation
	}

	/// Generates bytecode for a pre-is expression
	private void generatePreIsExprBytecode(RPreIsExpr expr) {
		// TODO: Implement pre-is expression bytecode generation
	}

	/// Generates bytecode for a pre-not-is expression
	private void generatePreNotIsExprBytecode(RPreNotIsExpr expr) {
		// TODO: Implement pre-not-is expression bytecode generation
	}

	/// Generates bytecode for a reference expression
	private void generateRefExprBytecode(RRefExpr expr) {
		// TODO: Implement reference expression bytecode generation
	}

	/// Generates bytecode for a VT get expression
	private void generateVTGetExprBytecode(RVTGetExpr expr) {
		// TODO: Implement VT get expression bytecode generation
	}

	/// Generates bytecode for a member get expression
	private void generateMemberGetExprBytecode(RMemberGetExpr expr) {
		// TODO: Implement member get expression bytecode generation
	}

	/// Generates bytecode for a function expression
	private void generateFnExprBytecode(RFnExpr expr) {
		// TODO: Implement function expression bytecode generation
	}

	/// Generates bytecode for a struct literal expression
	private void generateStructLiteralExprBytecode(RStructLiteralExpr expr) {
		// TODO: Implement struct literal expression bytecode generation
	}

	/// Generates bytecode for an array literal expression
	private void generateArrayLiteralExprBytecode(RArrayLiteralExpr expr) {
		// TODO: Implement array literal expression bytecode generation
	}

	/// Generates bytecode for a literal expression
	private void generateLiteralExprBytecode(RLiteralExpr expr) {
		// TODO: Implement literal expression bytecode generation
	}


}


void printBytecode(string[][] bytecodeInstructions) {
	writefln("Generated %d Bytecode Instructions:", bytecodeInstructions.length);
	writeln("------------------");
	foreach (size_t lineNum, instruction; bytecodeInstructions) {
		if (instruction.length == 0) continue;

		string inst = instruction[0];
		string[] params = instruction[1..$];

		if (params.length > 0) {
			writefln("%04d: %s %s", lineNum, inst, params.join(" "));
		} else {
			writefln("%04d: %s", lineNum, inst);
		}
	}
	writeln("------------------");
	
}

void printBytecodeToFile(string filename, string[][] bytecodeInstructions) {
	string output;
	foreach (instruction; bytecodeInstructions) {
		if (instruction.length == 0) continue;

		string inst = instruction[0];
		string[] params = instruction[1..$];

		if (params.length > 0) {
			output ~= format!"%s %s\n"(inst, params.join(" "));
		} else {
			output ~= format!"%s\n"(inst);
		}
	}
	output ~= "\n";
	std.file.write(filename, output);

}


// Tesing conversion of literals to bytes and then back to values 
unittest{
	
	auto generator = new BytecodeGenerator;

	// Integer literal
	auto intLiteral = new RLiteralExpr;
	intLiteral.type = ADataType.ofInt(32);
	intLiteral.value = asBytes!int(1);

	// Float literal
	auto floatLiteral = new RLiteralExpr;
	floatLiteral.type = ADataType.ofFloat(32);
	floatLiteral.value = asBytes!float(1.0f);

	// Double literal
	auto doubleLiteral = new RLiteralExpr;
	doubleLiteral.type = ADataType.ofInt(64);
	doubleLiteral.value = asBytes!double(3.14159);

	// Short literal
	auto shortLiteral = new RLiteralExpr;
	shortLiteral.type = ADataType.ofInt(16);
	shortLiteral.value = asBytes!short(42);

	// Long literal
	auto longLiteral = new RLiteralExpr;
	longLiteral.type = ADataType.ofInt(64);
	longLiteral.value = asBytes!long(123456789);

	// Print converted values
	assert(as!int(intLiteral.value) ==  1);
	assert(as!float(floatLiteral.value) ==  1.0f);
	assert(as!double(doubleLiteral.value) ==  3.14159);
	assert(as!short(shortLiteral.value) ==  42);
	assert(as!long(longLiteral.value) ==  123456789);

	bool exceptionThrown = false;
	try {
		auto invalidLiteral = new RLiteralExpr;
		invalidLiteral.type = ADataType.ofString();
	} catch (Exception e) {
		exceptionThrown = true;
	}

	RReturn intReturnStmt = new RReturn;
	intReturnStmt.val = intLiteral;

	RReturn floatReturnStmt = new RReturn;
	floatReturnStmt.val = floatLiteral;
	
	auto intBytecode = generator.generateBytecode(intReturnStmt);
	printBytecodeToFile(testFolder ~ "generated_int_code.txt", intBytecode);

	auto floatBytecode = generator.generateBytecode(floatReturnStmt);
	printBytecodeToFile(testFolder ~ "generated_float_code.txt", floatBytecode);

	RBlock block = new RBlock;
	block.statements ~= intReturnStmt;
	block.statements ~= floatReturnStmt;

	RIf ifStmt = new RIf;
	ifStmt.condition = intLiteral;
	ifStmt.onTrue = block;

	RFor forStmt = new RFor;
	forStmt.countIdent = "i"; // TODO check why these are strings
	forStmt.valIdent = "j"; // TODO check why these are strings
	forStmt.body = block;

	auto complexBytecode = generator.generateBytecode(forStmt);
	printBytecodeToFile(testFolder ~  "complex_code.txt", complexBytecode);

}

// Testing literal expressions
unittest {

	auto generator = new BytecodeGenerator;

	// Test for 4-byte int literal
	auto intLiteral = new RLiteralExpr;
	intLiteral.type = ADataType.ofInt(32);
	intLiteral.value = asBytes!int(42);
	generator.generateLiteralBytecode(intLiteral);

	// Test for 8-byte long literal
	auto longLiteral = new RLiteralExpr;
	longLiteral.type = ADataType.ofInt(64);
	longLiteral.value = asBytes!long(123456789L);	
	generator.generateLiteralBytecode(longLiteral);

	// Test for 32-bit float literal
	auto floatLiteral = new RLiteralExpr;
	floatLiteral.type = ADataType.ofFloat(32);
	floatLiteral.value = asBytes!float(3.14f);	
	generator.generateLiteralBytecode(floatLiteral);

	// Test for 64-bit double literal
	auto doubleLiteral = new RLiteralExpr;
	doubleLiteral.type = ADataType.ofInt(64);
	doubleLiteral.value = asBytes!double(2.717);	
	generator.generateLiteralBytecode(doubleLiteral);

	printBytecodeToFile(testFolder ~  "literal_code.txt", generator.bytecodeInstructions);
	//printBytecode(generator.bytecodeInstructions);

} 


// Testing Function Bytecode Generator
unittest{
	RFn fn = new RFn;
	fn.ident = "testFunction";

	// Create function body as a block expression
	RBlockExpr blockExpr = new RBlockExpr;
	blockExpr.type.type = ADataType.Type.Struct; // Assume void return type
	blockExpr.block = new RBlock;
	fn.body = blockExpr;

	// Initialize function parameters
	fn.paramsN = ["param1", "param2"];  // Parameter names
	fn.paramsT = [
		ADataType.ofInt,  // param1 type
		ADataType.ofInt   // param2 type
	];
	fn.paramCount = fn.paramsN.length;  // Set number of parameters

	// Initialize block's local variables
	blockExpr.block.localsN = ["local1", "local2"];
	blockExpr.block.localsT = [
		ADataType.ofFloat, // local1 type
		ADataType.ofInt    // local2 type
	];

	auto generator = new BytecodeGenerator;
	generator.generateFunctionBytecode(fn);
	fn.ident = "testFunction2";
	generator.generateFunctionBytecode(fn);
			
	printBytecodeToFile(testFolder ~  "fnbytecode_code.txt", generator.bytecodeInstructions);
}

// Testing Intrinsic Call Expr Bytecode Generator
unittest{
	auto generator = new BytecodeGenerator;

	RIdentExpr iExpr = new RIdentExpr;
	iExpr.ident = "iExpr";

	RLiteralExpr litNegOne = new RLiteralExpr;
	litNegOne.type = ADataType.ofInt;
	litNegOne.value = (-1L).asBytes;

	RLiteralExpr litTen = new RLiteralExpr;
	litTen.type = ADataType.ofInt;
	litTen.value = (10L).asBytes;

	// cmpI64 = cmpI64(iExpr, 10)
	RIntrinsicCallExpr cmpI64 = new RIntrinsicCallExpr;
	cmpI64.name = "cmpI64";
	cmpI64.params = [iExpr, litTen];

	// isI8 = isI8(cmpI64, -1)
	RIntrinsicCallExpr isI8 = new RIntrinsicCallExpr;
	isI8.name = "isI8";
	isI8.params = [cmpI64, litNegOne];

	// incI32 = incI32(iExpr)
	RIntrinsicCallExpr incI32 = new RIntrinsicCallExpr;
	incI32.name = "incI32";
	incI32.params = [iExpr];

	// Run test cases
	//writeln("Testing isI8:");
	generator.generateIntrinsicCallExprBytecode(isI8);

	//writeln("Testing incI32:");
	//generator.generateIntrinsicCallExprBytecode(incI32);

	printBytecodeToFile(testFolder ~  "intrinsic_code.txt", generator.bytecodeInstructions);
}

// Testing if conditions
unittest {
	/*
	var int x = 0;

	if $isI8($cmpI64(x, 10), -1) {
		x = 1;
	} else {
		x = 2;
	}
	*/

	auto generator = new BytecodeGenerator();

	// Define variable x
	RIdentExpr xIdent = new RIdentExpr;
	xIdent.ident = "x";

	RLiteralExpr litZero = new RLiteralExpr;
	litZero.type = ADataType.ofInt;
	litZero.value = (0L).asBytes;

	RAssignExpr initX = new RAssignExpr;
	initX.lhs = xIdent;
	initX.rhs = litZero;

	// Define condition: isI8(cmpI64(x, 10), -1)
	RLiteralExpr litTen = new RLiteralExpr;
	litTen.type = ADataType.ofInt;
	litTen.value = (10L).asBytes;

	RLiteralExpr litNegOne = new RLiteralExpr;
	litNegOne.type = ADataType.ofInt;
	litNegOne.value = (-1L).asBytes;

	RIntrinsicCallExpr cmpI64 = new RIntrinsicCallExpr;
	cmpI64.name = "cmpI64";
	cmpI64.params = [xIdent, litTen];

	RIntrinsicCallExpr isI8 = new RIntrinsicCallExpr;
	isI8.name = "isI8";
	isI8.params = [cmpI64, litNegOne];

	// Define if-true block: x = 1
	RLiteralExpr litOne = new RLiteralExpr;
	litOne.type = ADataType.ofInt;
	litOne.value = (1L).asBytes;

	RAssignExpr assignTrue = new RAssignExpr;
	assignTrue.lhs = xIdent;
	assignTrue.rhs = litOne;

	// Define if-false block: x = 2
	RLiteralExpr litTwo = new RLiteralExpr;
	litTwo.type = ADataType.ofInt;
	litTwo.value = (2L).asBytes;

	RAssignExpr assignFalse = new RAssignExpr;
	assignFalse.lhs = xIdent;
	assignFalse.rhs = litTwo;

	// Define if statement
	RIf ifStmt = new RIf;
	ifStmt.condition = isI8;
	ifStmt.onTrue = assignTrue;
	ifStmt.onFalse = assignFalse;

	// Generate bytecode
	generator.generateStatementBytecode(initX);
	generator.generateStatementBytecode(ifStmt);

	printBytecodeToFile(testFolder ~ "rif_intrinsic_code.txt", generator.bytecodeInstructions);
}


version (codegen) {
	void main() {
		writeln("Testing Bytecode Generator...");
	}
}


