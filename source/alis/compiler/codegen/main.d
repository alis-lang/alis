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

	private string generateLabel(string prefix = "label") {
		return format!"%s_%d"(prefix, labelCounter++);
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
	/// 	expr = The expression to generate bytecode for.
	private void generateExpressionBytecode(RExpr expr) {
		// Save current state
		int savedStackOffset = stackOffset;

		if (auto literal = cast(RLiteralExpr) expr) {
			generateLiteralBytecode(literal);
		} else if (auto identExpr = cast(RIdentExpr) expr) {
			generateIdentExprBytecode(identExpr);
		} else if (auto blockExpr = cast(RBlockExpr) expr) {
			generateBlockExprBytecode(blockExpr);
		} else if (auto intrinsicCallExpr = cast(RIntrinsicCallExpr) expr) {
			generateIntrinsicCallExprBytecode(intrinsicCallExpr);
		} else if (auto assignExpr = cast(RAssignExpr) expr) {
			generateAssignExprBytecode(assignExpr);
		} else {
			throw new Exception("Unsupported expression type: " ~ typeid(expr).toString);
		}

		// Restore stack offset only
		stackOffset = savedStackOffset;
	}

	/// Generates bytecode for a function definition.
	///
	/// Params:
	///    fn = The function to generate bytecode for.
	private void generateFunctionBytecode(RFn fn) {
		// Save previous variable offsets and create a new map for this function scope
		int[string] oldVariableOffsets = variableOffsets.dup;
		int oldStackOffset = stackOffset;
		variableOffsets = null;

		// Add function label
		addInstruction(fn.ident ~ ":");
		
		// Allocate space for return address
		size_t returnAddressSize = (cast(RBlockExpr)fn.body).type.sizeOf;
		addInstruction("\tpshN", [returnAddressSize.to!string]);
		variableOffsets[fn.ident] = stackOffset;
		stackOffset += cast(int)returnAddressSize;
		
		// Register parameters in the offset map, starting after return address
		int paramOffset = stackOffset;
		foreach (i, name; fn.paramsN) {	
			variableOffsets[name] = paramOffset;
			paramOffset += cast(int)fn.paramsT[i].sizeOf;
		}
		
		// Set stack offset to after parameters
		stackOffset = paramOffset;
		writeln("Parameter offsets: ", variableOffsets);

		// Generate bytecode for the function's block expression
		RBlockExpr fnBlockExpr = cast(RBlockExpr)fn.body;
		generateBlockExprBytecode(fnBlockExpr);
		
		// Restore previous variable offsets
		variableOffsets = oldVariableOffsets;
		stackOffset = oldStackOffset;
	}


	private void generateBlockBytecode(RBlock block) {
		// Save current offsets before entering the block
		int[string] previousOffsets = variableOffsets.dup;
		int previousStackOffset = stackOffset;
		
		// Update offsets with block's local variables
		stackOffset = updateBlockVariableOffsets(block, stackOffset);
		
		// Allocate space for local variables
		foreach (localType; block.localsT) {
			addInstruction("\tpshN", [localType.sizeOf.to!string]);
		}
		
		// Generate code for statements in the block
		generateStatementsBytecode(block.statements);
		
		// Restore previous offsets when exiting the block
		variableOffsets = previousOffsets;
		stackOffset = previousStackOffset;
	}

	private void generateReturnBytecode(RReturn returnStmt) {
		writeln("Mock: generateReturnBytecode called");
	}


	private void generateIfBytecode(RIf ifStmt) {
		writeln("Calling GenerateIfBytecode");

		string endLabel = generateLabel("if_end");
		string elseLabel = generateLabel("if_else");
		
		// Generate condition expression bytecode
		generateExpressionBytecode(ifStmt.condition);
		
		// Jump to else branch if condition is false
		if (ifStmt.onFalse) {
			addInstruction("\tjmpC", ["@" ~ elseLabel]);
			
			// Generate true branch code
			generateStatementBytecode(ifStmt.onTrue);
			
			// Skip over else branch
			addInstruction("\tjmp", ["@" ~ endLabel]);
			
			// Else branch
			addInstruction(elseLabel ~ ":");
			generateStatementBytecode(ifStmt.onFalse);
		} else {
			// If there's no else branch and condition is false, just skip the true branch
			addInstruction("\tjmpC", ["@" ~ endLabel]);
			
			// Generate true branch code
			generateStatementBytecode(ifStmt.onTrue);
		}
		
		// End of if statement
		addInstruction(endLabel ~ ":");
	}

	private void generateForBytecode(RFor forStmt) {
		writeln("Mock: generateForBytecode called: ");
	}

	private void generateWhileBytecode(RWhile whileStmt) {
		writeln("Mock: generateWhileBytecode called: ");
	}

	private void generateDoWhileBytecode(RDoWhile doWhileStmt) {
		writeln("Mock: generateDoWhileBytecode called: ");
	}

	private void generateSwitchBytecode(RSwitch switchStmt) {
		writeln("Mock: generateSwitchBytecode called: ");
	}

	/// Generates bytecode for a literal expression.
	///
	/// Params:
	///    literalExpr = The literal expression to generate bytecode for.
	private void generateLiteralBytecode(RLiteralExpr literalExpr) {
		size_t size = literalExpr.type.sizeOf;
		addInstruction("\tpshN", [size.to!string]);
		
		// Update stack offset
		stackOffset += cast(int)size;
	}

	
	private void generateIdentExprBytecode(RIdentExpr identExpr) {
		string varName = identExpr.ident;
		int offset = getVariableOffset(varName);
		
		if (offset >= 0) {
			// Variable found, load its value
			addInstruction("\tgetR", [offset.to!string]);
		} else {
			writeln("Variable offsets map: ", variableOffsets);
			throw new Exception("Variable not found: " ~ varName);
		}
	}

	/// Generates bytecode for a block expression.
	///
	/// This function allocates space for the return address and local variables,
	/// then generates bytecode for the statements inside the block.
	///
	/// Params:
	///    blockExpr = The block expression to generate bytecode for.
	private void generateBlockExprBytecode(RBlockExpr blockExpr) {
		// Save current variable offsets
		int[string] oldVariableOffsets = variableOffsets.dup;
		int oldStackOffset = stackOffset;
		
		// Only allocate return address if we're not in a function context
		// (indicated by stackOffset being 0)
		if (stackOffset == 0) {
			size_t returnAddressSize = blockExpr.type.sizeOf;
			addInstruction("\tpshN", [returnAddressSize.to!string]);
			stackOffset += cast(int)returnAddressSize;
		}
		
		RBlock block = blockExpr.block;
		
		// Update variable offsets for this block's local variables
		// Start local variables after parameters and return address
		stackOffset = updateBlockVariableOffsets(block, stackOffset);
		writeln("Block variable offsets: ", variableOffsets);
		
		// Allocate space for block's local variables
		foreach (localT; block.localsT) {
			addInstruction("\tpshN", [localT.sizeOf.to!string]);
		}
		
		generateStatementsBytecode(block.statements);
		
		// Restore previous offsets
		variableOffsets = oldVariableOffsets;
		stackOffset = oldStackOffset;
	}

	/// Generates bytecode for an intrinsic function call.
	///
	/// Supported Intrinsics:
	///    cmpIX`  : Compares two integer values of size X (e.g., `cmpI8`, `cmpI16`).
	///    isIX`   : Checks if a comparison result matches an expected value.
	///
	/// Params:
	///    intrinsicCallExpr = The intrinsic call expression to generate bytecode for.
	private void generateIntrinsicCallExprBytecode(RIntrinsicCallExpr intrinsicCallExpr) {
		const string name = intrinsicCallExpr.name;
		const string intrinsicLabel = "intrinsic_" ~ name;
		if (name.startsWith("cmpI")) {

			addInstruction("intrinsic_" ~ name ~ ":", []);

			// Extract the number X from cmpIX
			string numStr = name[4 .. $];
			int X;
			try {
				X = numStr.to!int;
			} catch (ConvException) {
				throw new FormatException("Invalid intrinsic name: " ~ name);
			}

			assert(intrinsicCallExpr.params.length == 2, name ~ " expects 2 parameters.");
			addInstruction("\tpshN", [(X).to!string]);

			RExpr leftOperand = intrinsicCallExpr.params[0]; 
			RExpr rightOperand = intrinsicCallExpr.params[1];
			size_t offsetLeft, offsetRight;

			// Save current stack offset and generate left operand
			int savedStackOffset = stackOffset;
			generateExpressionBytecode(leftOperand);
			
			// Calculate left operand offset based on stack change
			if (auto identExpr = cast(RIdentExpr)leftOperand) {
				offsetLeft = getVariableOffset(identExpr.ident);
			} else {
				offsetLeft = savedStackOffset;
				savedStackOffset = stackOffset;
			}
			
			// Generate right operand
			generateExpressionBytecode(rightOperand);
			
			// Calculate right operand offset
			if (auto identExpr = cast(RIdentExpr)rightOperand) {
				offsetRight = getVariableOffset(identExpr.ident);
			} else {
				offsetRight = savedStackOffset;
			}

			addInstruction("\tgetR", [offsetLeft.to!string]);
			addInstruction("\tgetR", [offsetRight.to!string]);
			addInstruction("\tcmpI" ~ X.to!string);
			addInstruction("\tret");
		} 
		else if (name.startsWith("isI")) {
			string numStr = name[3 .. $];
			int X;
			try {
				X = numStr.to!int;
			} catch (ConvException) {
				throw new FormatException("Invalid intrinsic name: " ~ name);
			}
			
			assert(intrinsicCallExpr.params.length == 2, name ~ " expects 2 parameters.");
			addInstruction("\tjmp", ["@intrinsic_isI" ~ (X.to!string)]);

			// Save current state
			int[string] savedVariableOffsets = variableOffsets.dup;
			int savedStackOffset = stackOffset;

			RIntrinsicCallExpr cmpIntrinsic = cast(RIntrinsicCallExpr)intrinsicCallExpr.params[0];
			generateIntrinsicCallExprBytecode(cmpIntrinsic);
				
			addInstruction(intrinsicLabel ~ ":");
			addInstruction("\tpshN", [(X).to!string]);
			addInstruction("\tjmp", ["@intrinsic_cmp" ~ (X.to!string)]);

			auto expectedValue = cast(RLiteralExpr)intrinsicCallExpr.params[1];
			
			// Restore state after first param
			int[string] midVariableOffsets = variableOffsets.dup;
			int midStackOffset = stackOffset;
			
			// Process second param
			generateLiteralBytecode(expectedValue);
			
			RExpr cmpLeftOperand = cmpIntrinsic.params[0];
			size_t offsetCmp, offsetVal;
			
			if (auto identExpr = cast(RIdentExpr)cmpLeftOperand) {
				offsetCmp = getVariableOffset(identExpr.ident);
			} else {
				offsetCmp = savedStackOffset;
			}

			offsetVal = midStackOffset;

			addInstruction("\tgetR", [offsetCmp.to!string]);
			addInstruction("\tgetR", [offsetVal.to!string]);
			addInstruction("\tcmpI" ~ X.to!string);
			addInstruction("\tret");
			
			// Restore state
			variableOffsets = savedVariableOffsets;
		} 
		else if (name.startsWith("incI")) {
			string numStr = name[4 .. $];
			int X;
			try {
				X = numStr.to!int;
			} catch (ConvException) {
				throw new FormatException("Invalid intrinsic name: " ~ name);
			}

			assert(intrinsicCallExpr.params.length == 1, name ~ " expects 1 parameter.");

			addInstruction(intrinsicLabel ~ ":", []);

			RExpr operand = intrinsicCallExpr.params[0];
			int offset;
			
			if (auto identExpr = cast(RIdentExpr)operand) {
				offset = getVariableOffset(identExpr.ident);
				if (offset < 0) {
					throw new Exception("Variable not found: " ~ identExpr.ident);
				}
			} else {
				// Process the operand to get its offset
				int savedStackOffset = stackOffset;
				generateExpressionBytecode(operand);
				offset = savedStackOffset;
			}

			addInstruction("\tgetR", [offset.to!string]);
			addInstruction("\tpshN", [(X).to!string]);
			addInstruction("\taddI" ~ X.to!string);
			addInstruction("\tput", [offset.to!string]);
		} 
		else if (name == "writeln") {
			assert(intrinsicCallExpr.params.length == 1, "writeln expects 1 parameter.");

			RExpr argument = intrinsicCallExpr.params[0];
			int offset;
			
			if (auto identExpr = cast(RIdentExpr)argument) {
				offset = getVariableOffset(identExpr.ident);
				if (offset < 0) {
					throw new Exception("Variable not found: " ~ identExpr.ident);
				}
			} else {
				// Process the argument to get its offset
				int savedStackOffset = stackOffset;
				generateExpressionBytecode(argument);
				offset = savedStackOffset;
			}

			addInstruction("\tgetR", [offset.to!string]);
			addInstruction("writeS");
		} 
		else {
			throw new Exception("Unsupported intrinsic: " ~ name);
		}
	}

	private void generateAssignExprBytecode(RAssignExpr assignExpr) {
		// Save the current state
		int[string] savedVariableOffsets = variableOffsets.dup;
		int savedStackOffset = stackOffset;

		// Generate the right-hand side expression first
		// This will push the value onto the stack
		generateExpressionBytecode(assignExpr.rhs);	

		// Handle the left-hand side
		if (auto identExpr = cast(RIdentExpr)assignExpr.lhs) {
			// Find the variable in the current scope
			string varName = identExpr.ident;
			int offset = getVariableOffset(varName);
			
			if (offset < 0) {
				// Variable not found, create a new one
				offset = stackOffset;
				variableOffsets[varName] = offset;
				// Note: Space for the variable should already be allocated
			}

			// Store the value from stack to the variable location
			addInstruction("\tput", [offset.to!string]);
		}
		else if (auto memberGetExpr = cast(RMemberGetExpr)assignExpr.lhs) {
			// TODO: Handle member assignment
			writeln("TODO: Handle member assignment");
		} else if (auto derefExpr = cast(RDerefExpr)assignExpr.lhs) {
			// TODO: Handle dereference assignment
			writeln("TODO: Handle dereference assignment");
		} else {
			throw new Exception("Unsupported left-hand side in assignment: " ~ typeid(assignExpr.lhs).toString);
		}

		// Restore previous state
		variableOffsets = savedVariableOffsets;
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

/*
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
*/

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


