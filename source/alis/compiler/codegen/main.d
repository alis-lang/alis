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

	string[][] generateBytecode(RStatement statement) {
		bytecodeInstructions.length = 0;
		labelCounter = 0;
		stackOffset = 0;

		generateStatementBytecode(statement);
		return bytecodeInstructions;
	}

	private void addInstruction(string instruction, string[] params = []) {
		bytecodeInstructions ~= [instruction] ~ params;
	}



	private string generateLabel(string prefix = "label") {
		return format!"%s_%d"(prefix, labelCounter++);
	}

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

	private void generateFunctionBytecode(RFn fn){

		// Add Fucntion Identifier
		addInstruction(fn.ident ~ ":");

		// Push Return address, parameters and local variables to stack
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
			addInstruction("jmpC", [elseLabel]);
			
			// Generate true branch code
			generateStatementBytecode(ifStmt.onTrue);
			
			// Skip over else branch
			addInstruction("jmp", [endLabel]);
			
			// Else branch
			addInstruction(elseLabel ~ ":");
			generateStatementBytecode(ifStmt.onFalse);
		} else {
			// If there's no else branch and condition is false, just skip the true branch
			addInstruction("jmpC", [endLabel]);
			
			// Generate true branch code
			generateStatementBytecode(ifStmt.onTrue);
		}
		
		// End of if statement
		addInstruction(endLabel ~ ":");
	}

	private void generateForBytecode(RFor forStmt) {
		writeln("Mock: generateForBytecode called: ");
		RStatement[] statements = (cast(RBlock)forStmt.body).statements;
		generateStatementsBytecode(statements);
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



	// Utility methods to convert literals
	// TODO: Not being used anymore so probably delete this
	private string convertLiteralToType(RLiteralExpr literalExpr) {
		string type = literalExpr.type.toString;
		switch (type) {
			case "int8":
				return "I1";
			case "int16":
				return "I2";
			case "int32":
				return "I4";
			case "int64":
				return "I8";
			case "float32":
				return "F4";
			case "float64":
				return "F8";
			default: throw new Exception("Unsupported literal type: " ~ type);
		}
	}

	private void generateExpressionBytecode(RExpr expr){
		if (auto literal = cast(RLiteralExpr)expr){
			generateLiteralBytecode(literal);
		} else if (auto identExpr = cast(RIdentExpr) expr){
			generateIdentExprBytecode(identExpr);
		} else if (auto blockExpr = cast(RBlockExpr) expr){
			generateBlockExprBytecode(blockExpr);
		} else if (auto intrinsicCallExpr = cast(RIntrinsicCallExpr) expr){
			generateIntrinsicCallExprBytecode(intrinsicCallExpr);
		}
		else {
			throw new Exception("Unsupported expression type: " ~ typeid(expr).toString);
		}
	}

	private void generateIntrinsicCallExprBytecode(RIntrinsicCallExpr intrinsicCallExpr) {
		const string name = intrinsicCallExpr.name;
		const string intrinsicLabel = "intrinsic_" ~ name;
		

		if (name.startsWith("cmpI")) {

			addInstruction("intrinsic_" ~ name ~ ":", []);

			// Extract the number X from cmpIX
			string numStr = name[4 .. $]; // Get the number after "cmpI"
			int X;
			try {
				X = numStr.to!int/8; 
			} catch (ConvException) {
				throw new FormatException("Invalid intrinsic name: " ~ name);
			}

			assert(intrinsicCallExpr.params.length == 2, name ~ " expects 2 parameters.");
			// Push space return address
			addInstruction("\tpshN", [X.to!string]);

			RExpr leftOperand = intrinsicCallExpr.params[0]; 
			RExpr rightOperand = intrinsicCallExpr.params[1];
			size_t offsetLeft, offsetRight;

			if (auto lit = cast(RLiteralExpr) leftOperand) {
				generateLiteralBytecode(lit);
				offsetLeft = lit.type.sizeOf/8 * size_t(1);
			} else {
				// TODO: generate identifier bytecode
				offsetLeft = 0; // TODO: Calculate Offset
			}

			if (auto lit = cast(RLiteralExpr) rightOperand) {
				generateLiteralBytecode(lit);
				offsetRight = lit.type.sizeOf/8 * size_t(2);
			} else {
				// TODO: generate identifier bytecode
				offsetRight = 0; // TODO: Calculate Offset
			}

			addInstruction("\tgetR", [offsetLeft.to!string]);
			addInstruction("\tgetR", [offsetRight.to!string]);
			addInstruction("\tcmpI" ~ X.to!string);
			addInstruction("\tret");
		} 
		else if (name.startsWith("isI")) {

			// Extract the number X from isIX
			string numStr = name[3 .. $]; // Get the number after "isI"
			int X;
			try {
				X = numStr.to!int/8; 
			} catch (ConvException) {
				throw new FormatException("Invalid intrinsic name: " ~ name);
			}
			
			assert(intrinsicCallExpr.params.length == 2, name ~ " expects 2 parameters.");

			// Left Param will be $cmp. Its output will be on top of the stack
			RIntrinsicCallExpr cmpIntrinsic = cast(RIntrinsicCallExpr)intrinsicCallExpr.params[0];
			generateIntrinsicCallExprBytecode(cmpIntrinsic);
				
			// Create return address space. Then Jump to $cmp 
			addInstruction(intrinsicLabel ~ ":");  //  Generate $cmp bytecode first. Then $isI.
			addInstruction("\tpshN", [X.to!string]); // Push space return address
			addInstruction("\tjmp", ["@intrinsic_isI" ~ ((X*8).to!string)]);

			// Right param will be a literal expression
			auto expectedValue = cast(RLiteralExpr)intrinsicCallExpr.params[1];
			generateLiteralBytecode(expectedValue);

			RExpr cmpLeftOperand = cmpIntrinsic.params[0];
			size_t offsetCmp, offsetVal;
			// cmpLeftOperand can be an identifier or a LiteralExpr
			if (auto lit = cast(RLiteralExpr) cmpLeftOperand) {
				offsetCmp = lit.type.sizeOf/8 * size_t(1); 
			} else {
				// TODO: generate identifier bytecode
				offsetCmp = 0; 
			}

			offsetVal = expectedValue.type.sizeOf/8 * size_t(2);

			addInstruction("\tgetR", [offsetCmp.to!string]);
			addInstruction("\tgetR", [offsetVal.to!string]);
			addInstruction("\tcmpI" ~ X.to!string);
			addInstruction("\tret");
		} 
		else if (name.startsWith("incI")) {
			// Extract the number X from incIX
			string numStr = name[4 .. $]; // Get the number after "incI"
			int X;
			try {
				X = numStr.to!int/8;
			} catch (ConvException) {
				throw new FormatException("Invalid intrinsic name: " ~ name);
			}

			assert(intrinsicCallExpr.params.length == 1, name ~ " expects 1 parameter.");

			addInstruction(intrinsicLabel ~ ":", []);

			auto operand = cast(RLiteralExpr)intrinsicCallExpr.params[0];
			size_t offset = operand.type.sizeOf/8 * size_t(1);

			addInstruction("\tgetR", [offset.to!string]);
			addInstruction("\tpshI" ~ X.to!string, ["1"]);
			addInstruction("\taddI" ~ X.to!string);
			addInstruction("\tput", [offset.to!string]);
		} 
		else if (name == "writeln") {
			assert(intrinsicCallExpr.params.length == 1, "writeln expects 1 parameter.");

			auto argument = cast(RLiteralExpr)intrinsicCallExpr.params[0];
			size_t offset = argument.type.sizeOf/8 * size_t(1);

			addInstruction("\tgetR", [offset.to!string]);
			addInstruction("writeln");
		} 
		else {
			throw new Exception("Unsupported intrinsic: " ~ name);
		}
	}


	// Generates Bytecode of Literals
	private void generateLiteralBytecode(RLiteralExpr literalExpr) {
		ADataType.Type type = (literalExpr.type).type;
		string instruction;
		string[] params;

		if (type == ADataType.Type.IntX) {
			// Convert the byte array to the appropriate IntX type
			size_t size = literalExpr.type.sizeOf/8;
			instruction = "\tpshI" ~ to!string(size);			
			
			if (size == 1) {
				byte value = as!byte(literalExpr.value);
				params = [to!string(value)];
			} else if (size == 2) {
				short value = as!short(literalExpr.value);
				params = [to!string(value)];
			} else if (size == 4) {
				int value = as!int(literalExpr.value);
				params = [to!string(value)];
			} else if (size == 8) {
				long value = as!long(literalExpr.value);
				params = [to!string(value)];
			}
		} else if (type == ADataType.Type.FloatX) {
			// Convert the byte array to the appropriate FloatX type

			size_t size = literalExpr.type.x;
			instruction = "\tpshF" ~ to!string(size/8);
			
			if (size == 4) {
				float value = as!float(literalExpr.value);
				params = [to!string(value)];
			} else if (size == 8) {
				double value = as!double(literalExpr.value);
				params = [to!string(value)];
			}
		}
		
		addInstruction(instruction, params);
	}

	private void generateIdentExprBytecode(RIdentExpr identExpr){
		
	}

	private void generateBlockExprBytecode(RBlockExpr blockExpr){
		size_t returnAddressSize = blockExpr.type.sizeOf/8;
		string instruction = returnAddressSize.format!"\tpshN %d";
		addInstruction(instruction); // Instruction for Return Address 
		RBlock block = blockExpr.block;

		foreach (localT ; block.localsT){
			instruction = (localT.sizeOf/8).format!"\tpshN %d";
			addInstruction(instruction); //Instruction for Parameters and Local Vars
		}

		generateStatementsBytecode(block.statements);
		
	}

	private void generateStatementsBytecode(RStatement[] statements){
		foreach (statement ; statements){
			generateStatementBytecode(statement);				
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
// unittest{
	
// 	auto generator = new BytecodeGenerator;

// 	// Integer literal
// 	auto intLiteral = new RLiteralExpr;
// 	intLiteral.type = ADataType.ofInt(4);
// 	intLiteral.value = asBytes!int(1);

// 	// Float literal
// 	auto floatLiteral = new RLiteralExpr;
// 	floatLiteral.type = ADataType.ofFloat(4);
// 	floatLiteral.value = asBytes!float(1.0f);

// 	// Double literal
// 	auto doubleLiteral = new RLiteralExpr;
// 	doubleLiteral.type = ADataType.ofInt(8);
// 	doubleLiteral.value = asBytes!double(3.14159);

// 	// Short literal
// 	auto shortLiteral = new RLiteralExpr;
// 	shortLiteral.type = ADataType.ofInt(2);
// 	shortLiteral.value = asBytes!short(42);

// 	// Long literal
// 	auto longLiteral = new RLiteralExpr;
// 	longLiteral.type = ADataType.ofInt(8);
// 	longLiteral.value = asBytes!long(123456789);

// 	// Print converted values
// 	assert(as!int(intLiteral.value) ==  1);
// 	assert(as!float(floatLiteral.value) ==  1.0f);
// 	assert(as!double(doubleLiteral.value) ==  3.14159);
// 	assert(as!short(shortLiteral.value) ==  42);
// 	assert(as!long(longLiteral.value) ==  123456789);

// 	bool exceptionThrown = false;
// 	try {
// 		auto invalidLiteral = new RLiteralExpr;
// 		invalidLiteral.type = ADataType.ofString();
// 	} catch (Exception e) {
// 		exceptionThrown = true;
// 	}


// 	RReturn intReturnStmt = new RReturn;
// 	intReturnStmt.val = intLiteral;

// 	RReturn floatReturnStmt = new RReturn;
// 	floatReturnStmt.val = floatLiteral;

	
// 	auto intBytecode = generator.generateBytecode(intReturnStmt);
// 	printBytecodeToFile(testFolder ~ "generated_int_code.txt", intBytecode);

// 	auto floatBytecode = generator.generateBytecode(floatReturnStmt);
// 	printBytecodeToFile(testFolder ~ "generated_float_code.txt", floatBytecode);

// 	RBlock block = new RBlock;
// 	block.statements ~= intReturnStmt;
// 	block.statements ~= floatReturnStmt;

// 	RIf ifStmt = new RIf;
// 	ifStmt.condition = intLiteral;
// 	ifStmt.onTrue = block;

// 	RFor forStmt = new RFor;
// 	forStmt.countIdent = "i"; // TODO check why these are strings
// 	forStmt.valIdent = "j"; // TODO check why these are strings
// 	forStmt.body = block;



// 	auto complexBytecode = generator.generateBytecode(forStmt);
// 	printBytecodeToFile(testFolder ~  "complex_code.txt", complexBytecode);

// }

// Testing literal expressions
// unittest {

// 	auto generator = new BytecodeGenerator;

// 	// Test for 4-byte int literal
// 	auto intLiteral = new RLiteralExpr;
// 	intLiteral.type = ADataType.ofInt(4);
// 	intLiteral.value = asBytes!int(42);
// 	generator.generateLiteralBytecode(intLiteral);

// 	// Test for 8-byte long literal
// 	auto longLiteral = new RLiteralExpr;
// 	longLiteral.type = ADataType.ofInt(8);
// 	longLiteral.value = asBytes!long(123456789L);	
// 	generator.generateLiteralBytecode(longLiteral);

// 	// Test for 32-bit float literal
// 	auto floatLiteral = new RLiteralExpr;
// 	floatLiteral.type = ADataType.ofFloat(4);
// 	floatLiteral.value = asBytes!float(3.14f);	
// 	generator.generateLiteralBytecode(floatLiteral);

// 	// // Test for 64-bit double literal
// 	auto doubleLiteral = new RLiteralExpr;
// 	doubleLiteral.type = ADataType.ofInt(8);
// 	doubleLiteral.value = asBytes!double(2.717);	
// 	generator.generateLiteralBytecode(doubleLiteral);

// 	printBytecodeToFile(testFolder ~  "literal_code.txt", generator.bytecodeInstructions);
// 	//printBytecode(generator.bytecodeInstructions);

// } 

// Testing Function Bytecode Generator
// unittest{
// 	RFn fn = new RFn;
// 	fn.ident = "testFunction";

// 	// Create function body as a block expression
// 	RBlockExpr blockExpr = new RBlockExpr;
// 	blockExpr.type.type = ADataType.Type.Struct; // Assume void return type
// 	blockExpr.block = new RBlock;
// 	fn.body = blockExpr;

// 	// Define function parameters and local variables
// 	blockExpr.block.localsN = ["param1", "param2", "local1", "local2"];
// 	blockExpr.block.localsT = [
// 		ADataType.ofInt,  // param1
// 		ADataType.ofInt,  // param2
// 		ADataType.ofFloat, // local1
// 		ADataType.ofInt    // local2
// 	];

// 	auto generator = new BytecodeGenerator;
// 	generator.generateFunctionBytecode(fn);
// 	printBytecodeToFile(testFolder ~  "fnbytecode_code.txt", generator.bytecodeInstructions);

// }

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
    writeln("Testing cmpI64:");
    generator.generateIntrinsicCallExprBytecode(cmpI64);

    writeln("Testing isI8:");
    generator.generateIntrinsicCallExprBytecode(isI8);

    //writeln("Testing incI32:");
    //generator.generateIntrinsicCallExprBytecode(incI32);

	printBytecodeToFile(testFolder ~  "intrinsic_code.txt", generator.bytecodeInstructions);



}


version (codegen) {
	void main() {
		writeln("Testing Bytecode Generator...");
	}
}


