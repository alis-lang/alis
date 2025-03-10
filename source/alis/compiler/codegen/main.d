

module alis.compiler.codegen.main;
import alis.compiler.common;
import alis.common;
import alis.compiler.rst;
import std.stdio;
import std.file;

import std.array;
import std.conv;
import std.format;

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
			//generateExpressionBytecode(expr);
		} else {
			throw new Exception("Unsupported statement type: " ~ typeid(statement).toString);
		}
	}

	private void generateBlockBytecode(RBlock block) {
		writeln("Mock: generateBlockBytecode called") ;
		foreach (statement ; block.statements){
			generateStatementBytecode(statement);
			
		}
	}

	private void generateReturnBytecode(RReturn returnStmt) {
		writeln("Mock: generateReturnBytecode called");
	}


	private void generateIfBytecode(RIf ifStmt) {
		writeln("Calling GenerateIfBytecode");

		string endLabel = generateLabel("if_end");
		string elseLabel = generateLabel("if_else");
		
		// Generate condition expression bytecode
		//generateExpressionBytecode(ifStmt.condition);
		
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
			// If there's no else branch, just skip the true branch if condition is false
			addInstruction("jmpC", [endLabel]);
			
			// Generate true branch code
			generateStatementBytecode(ifStmt.onTrue);
		}
		
		// End of if statement
		addInstruction(endLabel ~ ":");
	}

	private void generateForBytecode(RFor forStmt) {
		writeln("Mock: generateForBytecode called: ");
		RStatement[] body = (cast(RBlock)forStmt.body).statements;
		foreach (statement ; body){
			generateStatementBytecode(statement);
			
		}
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

	// 
	// private void generateExpressionBytecode(RExpr expr) {
	// 	if (auto literal = cast(RLiteralExpr)expr) {
	// 		generateLiteralBytecode(literal);


	// 	} else if (auto identExpr = cast(RIdentExpr)expr) {
	// 		// Handle identifier
	// 		int offset = 0; // Would need to determine actual offset from symbol table
	// 		string typeStr = identExpr.ident.toString;
			
	// 		if (typeStr.startsWith("int")) {
	// 			int size = to!int(typeStr[3..$]);
	// 			addInstruction("get", [to!string(size/8), to!string(offset)]);
	// 		} else if (typeStr.startsWith("float")) {
	// 			int size = to!int(typeStr[5..$]);
	// 			addInstruction("get", [to!string(size/8), to!string(offset)]);
	// 		}
	// 	} else if (auto assignExpr = cast(RAssignExpr)expr) {
	// 		// For assignment, generate the right side first (which puts value on stack),
	// 		// then store it to the location of the left side
	// 		generateExpressionBytecode(assignExpr.rhs);
			
	// 		// Determine the left side location
	// 		if (auto identExpr = cast(RIdentExpr)assignExpr.lhs) {
	// 			int offset = 0; // Would need actual offset
	// 			string typeStr = identExpr.ident.toString;
				
	// 			if (typeStr.startsWith("int")) {
	// 				int size = to!int(typeStr[3..$]);
	// 				addInstruction("putI" ~ to!string(size/8), [to!string(offset)]);
	// 			} else if (typeStr.startsWith("float")) {
	// 				int size = to!int(typeStr[5..$]);
	// 				addInstruction("putF" ~ to!string(size/8), [to!string(offset)]);
	// 			}
	// 		}
	// 	} else if (auto fnCallExpr = cast(RFnCallExpr)expr) {
	// 		// Push parameters in reverse order
	// 		foreach_reverse (param; fnCallExpr.params) {
	// 			generateExpressionBytecode(param);
	// 		}
			
	// 		// Generate callee expression
	// 		if (auto identExpr = cast(RIdentExpr)fnCallExpr.callee) {
	// 			string funcName = identExpr.ident.toString;
	// 			addInstruction("call", ["@" ~ funcName]);
	// 		} else {
	// 			// More complex function call (function pointer, etc.)
	// 			generateExpressionBytecode(fnCallExpr.callee);
	// 			addInstruction("call", ["R"]);  // Call through register
	// 		}
	// 	} else if (auto commaExpr = cast(RCommaExpr)expr) {
	// 		// Evaluate all expressions, but only the last one's value is kept
	// 		foreach (subExpr; commaExpr.exprs) {
	// 			generateExpressionBytecode(subExpr);
				
	// 			// Pop intermediate results except the last one
	// 			if (subExpr != commaExpr.exprs[$-1]) {
	// 				addInstruction("popN", ["1"]);
	// 			}
	// 		}
	// 	} else if (auto refExpr = cast(RRefExpr)expr) {
	// 		// Get reference to value
	// 		if (auto identExpr = cast(RIdentExpr)refExpr.val) {
	// 			int offset = 0; // Would need actual offset
	// 			addInstruction("pshO");
	// 			addInstruction("pshI4", [to!string(offset)]);
	// 			addInstruction("addI4");
	// 		} else {
	// 			// More complex reference
	// 			generateExpressionBytecode(refExpr.val);
	// 			addInstruction("getR");
	// 		}
	// 	} else if (auto derefExpr = cast(RDerefExpr)expr) {
	// 		// Dereference a pointer
	// 		generateExpressionBytecode(derefExpr.val);
	// 		addInstruction("get", ["4", "0"]); // Assuming 4-byte pointer dereferencing
	// 	}
	// 	// Add more expression types as needed
	// }

	// Generates Bytecode of Literals
	private void generateLiteralBytecode(RLiteralExpr literalExpr) {
		ADataType.Type type = (*literalExpr.type).type;
		string instruction;
		string[] params;
		
		if (type == ADataType.Type.IntX) {
			// Convert the byte array to the appropriate IntX type
			int size = literalExpr.type.x;
			instruction = "pshI" ~ to!string(size);
			
			
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

			int size = literalExpr.type.x;
			instruction = "pshF" ~ to!string(size);
			
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

private ADataType* getTypeFromString(string typeStr) {
	alias Type = ADataType.Type;
	switch (typeStr) {
		case "int8":
			return new ADataType(false, Type.IntX, 8);
		case "int16":
			return new ADataType(false, Type.IntX, 16);
		case "int32":
			return new ADataType(false, Type.IntX, 32);
		case "int64":
			return new ADataType(false, Type.IntX, 64);
		case "float32":
			return new ADataType(false, Type.FloatX, 32);
		case "float64":
			return new ADataType(false, Type.FloatX, 64);
		default:
			throw new Exception("Unknown type: " ~ typeStr);
	}
}

// Tesing conversion of literals to bytes and then back to values 
unittest{
	
	auto generator = new BytecodeGenerator;

	// Integer literal
	auto intLiteral = new RLiteralExpr;
	intLiteral.type = getTypeFromString("int32");
	intLiteral.value = asBytes!int(1);

	// Float literal
	auto floatLiteral = new RLiteralExpr;
	floatLiteral.type = getTypeFromString("float32");
	floatLiteral.value = asBytes!float(1.0f);

	// Double literal
	auto doubleLiteral = new RLiteralExpr;
	doubleLiteral.type = getTypeFromString("float64");
	doubleLiteral.value = asBytes!double(3.14159);

	// Short literal
	auto shortLiteral = new RLiteralExpr;
	shortLiteral.type = getTypeFromString("int16");
	shortLiteral.value = asBytes!short(42);

	// Long literal
	auto longLiteral = new RLiteralExpr;
	longLiteral.type = getTypeFromString("int64");
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
		invalidLiteral.type = getTypeFromString("string");
	} catch (Exception e) {
		exceptionThrown = true;
	}
	assert(exceptionThrown);

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
	intLiteral.type = new ADataType(false, ADataType.Type.IntX, 4);
	intLiteral.value = asBytes!int(42);
	generator.generateLiteralBytecode(intLiteral);

	// Test for 8-byte long literal
	auto longLiteral = new RLiteralExpr;
	longLiteral.type = new ADataType(false, ADataType.Type.IntX, 8);
	longLiteral.value = asBytes!long(123456789L);	
	generator.generateLiteralBytecode(longLiteral);

	// Test for 32-bit float literal
	auto floatLiteral = new RLiteralExpr;
	floatLiteral.type = new ADataType(false, ADataType.Type.FloatX, 4);
	floatLiteral.value = asBytes!float(3.14f);	
	generator.generateLiteralBytecode(floatLiteral);

	// // Test for 64-bit double literal
	auto doubleLiteral = new RLiteralExpr;
	doubleLiteral.type = new ADataType(false, ADataType.Type.FloatX, 8);
	doubleLiteral.value = asBytes!double(2.717);	
	generator.generateLiteralBytecode(doubleLiteral);

	printBytecodeToFile(testFolder ~  "literal_code.txt", generator.bytecodeInstructions);
	printBytecode(generator.bytecodeInstructions);

} 


version (codegen) {
	void main() {
		writeln("Testing Bytecode Generator...");
	}
}


