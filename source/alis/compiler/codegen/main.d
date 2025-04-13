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


	/// Generates bytecode of a statement
	/// 
	/// Params:
	///   statement = A Resolved statement like `RIf`, `RFn`
	/// Returns: Bytecode
	string[][] generateBytecode(RStatement statement) {
		bytecodeInstructions.length = 0;
		labelCounter = 0;
		stackOffset = 0;

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
	}

	/// Generates bytecode for a function definition.
	///
	/// Params:
	///    fn = The function to generate bytecode for.
	private void generateFunctionBytecode(RFn fn) {
		// Add function label
		addInstruction(fn.ident ~ ":");

		// Generate bytecode for the function's block expression
		RBlockExpr fnBlockExpr = cast(RBlockExpr)fn.body;
		generateBlockExprBytecode(fnBlockExpr);
	}


	private void generateBlockBytecode(RBlock block) {
		writeln("Mock: generateBlockBytecode called") ;
		
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
			addInstruction("\tjmpC", "@" ~ [elseLabel]);
			
			// Generate true branch code
			generateStatementBytecode(ifStmt.onTrue);
			
			// Skip over else branch
			addInstruction("\tjmp", "@" ~[endLabel]);
			
			// Else branch
			addInstruction(elseLabel ~ ":");
			generateStatementBytecode(ifStmt.onFalse);
		} else {
			// If there's no else branch and condition is false, just skip the true branch
			addInstruction("\tjmpC", "@" ~ [endLabel]);
			
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

		// Commenting out the part ahead which was pushing values. 
		// TODO: Probably delete this becuase we need to push space, not values

		// ADataType.Type type = (literalExpr.type).type;

		// if (type == ADataType.Type.IntX) {
		// 	// Convert the byte array to the appropriate IntX type
			
		// 	instruction = "\tpshI" ~ to!string(size);			
			
		// 	if (size == 1) {
		// 		byte value = as!byte(literalExpr.value);
		// 		params = [to!string(value)];
		// 	} else if (size == 2) {
		// 		short value = as!short(literalExpr.value);
		// 		params = [to!string(value)];
		// 	} else if (size == 4) {
		// 		int value = as!int(literalExpr.value);
		// 		params = [to!string(value)];
		// 	} else if (size == 8) {
		// 		long value = as!long(literalExpr.value);
		// 		params = [to!string(value)];
		// 	}
		// } else if (type == ADataType.Type.FloatX) {
		// 	// Convert the byte array to the appropriate FloatX type

		// 	size_t size = literalExpr.type.x;
		// 	instruction = "\tpshF" ~ to!string(size);
			
		// 	if (size == 4) {
		// 		float value = as!float(literalExpr.value);
		// 		params = [to!string(value)];
		// 	} else if (size == 8) {
		// 		double value = as!double(literalExpr.value);
		// 		params = [to!string(value)];
		// 	}
		// }
		
		// addInstruction(instruction, params);
	}

	
	private void generateIdentExprBytecode(RIdentExpr identExpr){
		
	}

	/// Generates bytecode for a block expression.
	///
	/// This function allocates space for the return address and local variables,
	/// then generates bytecode for the statements inside the block.
	///
	/// Params:
	///    blockExpr = The block expression to generate bytecode for.
	private void generateBlockExprBytecode(RBlockExpr blockExpr){
		size_t returnAddressSize = blockExpr.type.sizeOf;
		string instruction = returnAddressSize.format!"\tpshN %d";
		addInstruction(instruction); // Instruction for Return Address 
		RBlock block = blockExpr.block;

		foreach (localT ; block.localsT){
			instruction = (localT.sizeOf).format!"\tpshN %d";
			addInstruction(instruction); //Instruction for Parameters and Local Vars
		}

		generateStatementsBytecode(block.statements);
		
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

			if (auto lit = cast(RLiteralExpr) leftOperand) {
				generateLiteralBytecode(lit);
				offsetLeft = lit.type.sizeOf * size_t(1);
			} else {
				offsetLeft = 0;
			}

			if (auto lit = cast(RLiteralExpr) rightOperand) {
				generateLiteralBytecode(lit);
				offsetRight = lit.type.sizeOf * size_t(2);
			} else {
				offsetRight = 0;
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

			RIntrinsicCallExpr cmpIntrinsic = cast(RIntrinsicCallExpr)intrinsicCallExpr.params[0];
			generateIntrinsicCallExprBytecode(cmpIntrinsic);
				
			addInstruction(intrinsicLabel ~ ":");
			addInstruction("\tpshN", [(X).to!string]);
			addInstruction("\tjmp", ["@intrinsic_cmp" ~ (X.to!string)]);

			auto expectedValue = cast(RLiteralExpr)intrinsicCallExpr.params[1];
			generateLiteralBytecode(expectedValue);

			RExpr cmpLeftOperand = cmpIntrinsic.params[0];
			size_t offsetCmp, offsetVal;
			if (auto lit = cast(RLiteralExpr) cmpLeftOperand) {
				offsetCmp = lit.type.sizeOf * size_t(1); 
			} else {
				offsetCmp = 0; 
			}

			offsetVal = expectedValue.type.sizeOf * size_t(2);

			addInstruction("\tgetR", [offsetCmp.to!string]);
			addInstruction("\tgetR", [offsetVal.to!string]);
			addInstruction("\tcmpI" ~ X.to!string);
			addInstruction("\tret");
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

			auto operand = cast(RLiteralExpr)intrinsicCallExpr.params[0];
			size_t offset = operand.type.sizeOf * size_t(1);

			addInstruction("\tgetR", [offset.to!string]);
			addInstruction("\tpshN", [(X).to!string]);
			addInstruction("\taddI" ~ X.to!string);
			addInstruction("\tput", [offset.to!string]);
		} 
		else if (name == "writeln") {
			assert(intrinsicCallExpr.params.length == 1, "writeln expects 1 parameter.");

			auto argument = cast(RLiteralExpr)intrinsicCallExpr.params[0];
			size_t offset = argument.type.sizeOf * size_t(1);

			addInstruction("\tgetR", [offset.to!string]);
			addInstruction("writeS");
		} 
		else {
			throw new Exception("Unsupported intrinsic: " ~ name);
		}
	}

	private void generateAssignExprBytecode(RAssignExpr assignExpr){

		// Generate the right-hand side expression first
		// This will push the value onto the stack
		generateExpressionBytecode(assignExpr.rhs);	

		// Calculate the offset for storing the value
		size_t offset = 0;

		// Handle the left-hand side
		if (auto identExpr = cast(RIdentExpr)assignExpr.lhs) {

			// Find the variable in the current scope
			string varName = identExpr.ident;
			bool found = false;

			// TODO: Find variable offset

			// Store the value from stack to the variable location
			addInstruction("\tput", [offset.to!string]);
		}

		

		else if (auto memberGetExpr = cast(RMemberGetExpr)assignExpr.lhs) {
			// TODO

		} else if (auto derefExpr = cast(RDerefExpr)assignExpr.lhs) {
			// TODO
		} else {
			// TODO
		}

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

	// Define function parameters and local variables
	blockExpr.block.localsN = ["param1", "param2", "local1", "local2"];
	blockExpr.block.localsT = [
		ADataType.ofInt,  // param1
		ADataType.ofInt,  // param2
		ADataType.ofFloat, // local1
		ADataType.ofInt    // local2
	];

	auto generator = new BytecodeGenerator;
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


