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
			int localSize = cast(int)block.localsT[i].sizeOf;
			offset += localSize;
			addInstruction("\tpshN", [localSize.to!string]);
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

	/// Generates bytecode for a block expression
	private void generateBlockExprBytecode(RBlockExpr expr, string functionName = "returnAddress") {
		
		// Allocate space for the return address
		size_t returnAddressSize = ADataType.ofInt.sizeOf; 
		variableOffsets[functionName] = stackOffset;
		stackOffset += cast(int)returnAddressSize;
		addInstruction("\tpshN", [returnAddressSize.to!string]); 

		// Generate bytecode for the block
		generateBlockBytecode(expr.block); 
	}

	/// Generates bytecode for a block statement
	private void generateBlockBytecode(RBlock block) {
		// Update variable offsets for local variables in this block
		int startOffset = stackOffset; 
		updateBlockVariableOffsets(block, startOffset);

		// Update the stack offset for the next block
		stackOffset = startOffset;

		// Generate bytecode for each statement in the block
		generateStatementsBytecode(block.statements); 
	}

	/// Generates bytecode for a return statement
	private void generateReturnBytecode(RReturn returnStmt) {
		// TODO: Implement return statement bytecode generation
	}

	/// Generates bytecode for an if statement
	private void generateIfBytecode(RIf ifStmt) {
		// Generate bytecode for the condition
		generateExpressionBytecode(ifStmt.condition);

		// Create labels for the true and false branches
		string trueLabel = "true_" ~ labelCounter.to!string;
		string falseLabel = "false_" ~ labelCounter.to!string;
		string endLabel = "end_if_" ~ labelCounter.to!string;
		labelCounter++;

		// Add a conditional jump based on the result of the condition
		addInstruction("\tjmpC", ["@" ~ falseLabel]); // Jump to false label if condition is false

		// Generate bytecode for the true branch
		generateStatementBytecode(ifStmt.onTrue);

		// Add a jump to the end label after the true branch
		addInstruction("\tjmpC", ["@" ~ endLabel]); // Jump to end label after true branch

		// Generate bytecode for the false branch if it exists
		if (ifStmt.onFalse) {
			addInstruction(falseLabel, []); // Mark the false branch
			generateStatementBytecode(ifStmt.onFalse);
		}

		// Add the end label
		addInstruction(endLabel, []); // Mark the end of the if statement
	}

	/// Generates bytecode for a for statement
	private void generateForBytecode(RFor forStmt) {
		// TODO: Implement for statement bytecode generation
	}

	/// Generates bytecode for a while statement
	private void generateWhileBytecode(RWhile whileStmt) {
		// Create labels for the start and end of the while loop
		string startLabel = "while_start_" ~ labelCounter.to!string;
		string endLabel = "while_end_" ~ labelCounter.to!string;
		labelCounter++;

		addInstruction(startLabel, []);

		// Generate bytecode for the condition
		generateExpressionBytecode(whileStmt.condition);

		// Jump to end label if condition is false
		addInstruction("\tjmpC", ["@" ~ endLabel]); 

		// Generate bytecode for the body of the while loop
		generateStatementBytecode(whileStmt.body);

		// Jump back to the start of the while loop for 
		addInstruction("\tjmp", ["@" ~ startLabel]);

		addInstruction(endLabel, []);
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

	/// Generates bytecode for an intrinsic expression
	private void generateIntrinsicExprBytecode(RIntrinsicExpr expr) {
		// TODO: Implement intrinsic expression bytecode generation
	}

	/// Generates bytecode for an intrinsic call expression
	private void generateIntrinsicCallExprBytecode(RIntrinsicCallExpr expr) {
		// Handle each parameter expression
		foreach (param; expr.params) {
			generateExpressionBytecode(param);
		}
		// TODO: Implement intrinsic call expression bytecode generation
	}

	/// Generates bytecode for a data type expression
	private void generateDTypeExprBytecode(RDTypeExpr expr) {
		// TODO: Implement data type expression bytecode generation
	}

	/// Generates bytecode for an assignment expression
	private void generateAssignExprBytecode(RAssignExpr expr) {
		// Handle rhs expression first
		generateExpressionBytecode(expr.rhs); 

		// Handle lhs expression
		if (auto identExpr = cast(RIdentExpr)expr.lhs) {
			int lhsOffset = getVariableOffset(identExpr.ident);
			if (lhsOffset == -1) {
				throw new Exception("Left-hand side variable not found in variableOffsets");
			}
			// Generate the bytecode for the assignment
			addInstruction("\tput", [lhsOffset.to!string]); 
		} else if (auto memberGetExpr = cast(RMemberGetExpr)expr.lhs) {
			// Handle member assignment
			generateExpressionBytecode(memberGetExpr.val); 
			addInstruction("put", [memberGetExpr.member]); //TODO: Fix offset
		} else if (auto vtGetExpr = cast(RVTGetExpr)expr.lhs) {
			// Handle VT get assignment
			generateExpressionBytecode(vtGetExpr.val);
			addInstruction("put", [vtGetExpr.member]); //TODO: Fix offset
		} else {
			throw new Exception("Unsupported left-hand side expression type: " ~ typeid(expr.lhs).toString);
		}
	}

	/// Generates bytecode for a reference assignment expression
	private void generateRefAssignExprBytecode(RRefAssignExpr expr) {
		// Handle lhs expression
		generateExpressionBytecode(expr.lhs);
		// Handle rhs expression
		generateExpressionBytecode(expr.rhs);
		// TODO: Implement reference assignment expression bytecode generation
	}

	/// Generates bytecode for a dereference expression
	private void generateDerefExprBytecode(RDerefExpr expr) {
		// Handle target expression
		generateExpressionBytecode(expr.val); 
		// TODO: Implement dereference expression bytecode generation
	}

	/// Generates bytecode for a comma expression
	private void generateCommaExprBytecode(RCommaExpr expr) {
		// Handle each part expression
		foreach (part; expr.exprs) { 
			generateExpressionBytecode(part); // Pass the current map
		}
	}

	/// Generates bytecode for a function call expression
	private void generateFnCallExprBytecode(RFnCallExpr expr) {
		// Handle callee expression
		generateExpressionBytecode(expr.callee); 
		// Handle each argument expression
		foreach (arg; expr.params) {
			generateExpressionBytecode(arg); // Pass the current map
		}
	}

	/// Generates bytecode for a pre-is expression
	private void generatePreIsExprBytecode(RPreIsExpr expr) {
		// Handle inner expression
		generateExpressionBytecode(expr.val); 
	}

	/// Generates bytecode for a pre-not-is expression
	private void generatePreNotIsExprBytecode(RPreNotIsExpr expr) {
		// Handle inner expression
		generateExpressionBytecode(expr.val); 
	}

	/// Generates bytecode for a reference expression
	private void generateRefExprBytecode(RRefExpr expr) {
		// Handle target expression
		generateExpressionBytecode(expr.val); 
	}

	/// Generates bytecode for a VT get expression
	private void generateVTGetExprBytecode(RVTGetExpr expr) {
		// Handle target expression
		generateExpressionBytecode(expr.val); 
	}

	/// Generates bytecode for a member get expression
	private void generateMemberGetExprBytecode(RMemberGetExpr expr) {
		// Handle target expression
		generateExpressionBytecode(expr.val); 
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
		// Handle each element expression
		foreach (element; expr.elements) {
			generateExpressionBytecode(element);
		}
		// TODO: Implement array literal expression bytecode generation
	}

	/// Generates bytecode for a literal expression.
	///
	/// Params:
	///    literalExpr = The literal expression to generate bytecode for.
	private void generateLiteralExprBytecode(RLiteralExpr literalExpr) {
		size_t size = literalExpr.type.sizeOf;
		//addInstruction("\tpshN", [size.to!string]);
		
		// Update stack offset
		stackOffset += cast(int)size;
	}


}


void printBytecode(string[][] bytecodeInstructions, string instructionIsFor = "") {
	writefln("Generated %d Bytecode Instructions for %s:", bytecodeInstructions.length, instructionIsFor);
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



version (codegen) {
	void main() {
		writeln("Testing Bytecode Generator...");
	}
}



unittest {
	/*
	if (outerCondition) {
		outerResult = 100; 
	} else {
		if (innerCondition) {
			innerResult = 42; 
		}
	}
	*/
    auto generator = new BytecodeGenerator;

    // Create the inner if statement
	RIf innerIfStmt = new RIf;
	RIdentExpr innerCondExpr = new RIdentExpr;
	innerCondExpr.ident = "innerCondition";
	innerIfStmt.condition = innerCondExpr;
	RBlock innerTrueBlock = new RBlock;
	innerIfStmt.onTrue = innerTrueBlock;
	innerIfStmt.onFalse = null;


    // Create a statement for the inner if's true branch
    RAssignExpr innerAssign = new RAssignExpr;
	// Create and assign lhs
	RIdentExpr lhsExpr = new RIdentExpr;
	lhsExpr.ident = "innerResult";
	innerAssign.lhs = lhsExpr;
	// Create and assign rhs
	RLiteralExpr rhsExpr = new RLiteralExpr;
	rhsExpr.type = ADataType.ofInt;
	rhsExpr.value = asBytes!int(42);
	innerAssign.rhs = rhsExpr;

    // Add the inner assignment to the inner if's true block
    innerTrueBlock.statements ~= innerAssign;

	// Create the outer if statement
	RIf outerIfStmt = new RIf;

	// Create and assign the condition
	RIdentExpr outerCondExpr = new RIdentExpr;
	outerCondExpr.ident = "outerCondition";
	outerIfStmt.condition = outerCondExpr;

	// Create and assign the true and false blocks
	RBlock outerTrueBlock = new RBlock;
	RBlock outerFalseBlock = new RBlock;
	outerIfStmt.onTrue = outerTrueBlock;
	outerIfStmt.onFalse = outerFalseBlock;

	// Create a statement for the outer if's true branch
	RAssignExpr outerAssign = new RAssignExpr;

	// Create and assign lhs
	RIdentExpr outerLhsExpr = new RIdentExpr;
	outerLhsExpr.ident = "outerResult";
	outerAssign.lhs = outerLhsExpr;

	// Create and assign rhs
	RLiteralExpr outerRhsExpr = new RLiteralExpr;
	outerRhsExpr.type = ADataType.ofInt;
	outerRhsExpr.value = asBytes!int(100);
	outerAssign.rhs = outerRhsExpr;

	// Add the outer assignment to the outer if's true block
	outerTrueBlock.statements ~= outerAssign;
	outerTrueBlock.localsN = [outerLhsExpr.ident];
	outerTrueBlock.localsT = [ADataType.ofInt];

	// Add the inner if statement to the outer if's false block
	outerFalseBlock.statements ~= innerIfStmt;
	outerFalseBlock.localsN = [innerCondExpr.ident, lhsExpr.ident ];
	outerFalseBlock.localsT = [ADataType.ofBool, ADataType.ofInt];


    // Generate bytecode for the outer if statement
    generator.generateIfBytecode(outerIfStmt);

    // Print the generated bytecode
    printBytecode(generator.bytecodeInstructions, "if statements");
}

unittest {
	auto generator = new BytecodeGenerator;

	// Create a block expression with local variables and statements
	RBlockExpr blockExpr = new RBlockExpr;
	blockExpr.block = new RBlock;

	// Define local variables
	blockExpr.block.localsN = ["local1", "local2"];
	blockExpr.block.localsT = [ADataType.ofInt, ADataType.ofFloat]; 

	// Create statements for the block
	RIdentExpr local1Ident = new RIdentExpr;
	local1Ident.ident = "local1";

	RIdentExpr local2Ident = new RIdentExpr;
	local2Ident.ident = "local2";

	RLiteralExpr literal1 = new RLiteralExpr;
	literal1.type = ADataType.ofInt;
	literal1.value = asBytes!int(10);

	RLiteralExpr literal2 = new RLiteralExpr;
	literal2.type = ADataType.ofFloat;
	literal2.value = asBytes!float(20.0f);

	// Create assignment statements
	RAssignExpr assignLocal1 = new RAssignExpr;
	assignLocal1.lhs = local1Ident;
	assignLocal1.rhs = literal1;

	RAssignExpr assignLocal2 = new RAssignExpr;
	assignLocal2.lhs = local2Ident;
	assignLocal2.rhs = literal2;

	// Add statements to the block
	blockExpr.block.statements ~= assignLocal2;
	blockExpr.block.statements ~= assignLocal1;

	// Generate bytecode for the block expression
	generator.generateBlockExprBytecode(blockExpr);


	printBytecode(generator.bytecodeInstructions, "block expression and assignment statements");
}

unittest {
    /*
    while (condition) {
        result = 42;
    }
    */
    auto generator = new BytecodeGenerator;

    // Create a while statementz
    RWhile whileStmt = new RWhile;
    RIdentExpr conditionExpr = new RIdentExpr;
    conditionExpr.ident = "condition"; // Example condition variable
    whileStmt.condition = conditionExpr;

    // Create a block for the body of the while loop
    RBlock bodyBlock = new RBlock;
    whileStmt.body = bodyBlock;

    // Create a statement for the body of the while loop
    RAssignExpr assignStmt = new RAssignExpr;
    RIdentExpr lhsExpr = new RIdentExpr;
    lhsExpr.ident = "result"; // Example variable to assign to
    assignStmt.lhs = lhsExpr;
    RLiteralExpr rhsExpr = new RLiteralExpr;
    rhsExpr.type = ADataType.ofInt;
    rhsExpr.value = asBytes!int(42); // Example value
    assignStmt.rhs = rhsExpr;

    // Add the assignment statement to the body block
    bodyBlock.statements ~= assignStmt;
	bodyBlock.localsN = [lhsExpr.ident];
	bodyBlock.localsT = [ADataType.ofInt];

    // Generate bytecode for the while statement
    generator.generateWhileBytecode(whileStmt);

    // Print the generated bytecode
    printBytecode(generator.bytecodeInstructions, "while statement");
}

// unittest {
//     /*
//     result = 10;
//     while (result < 20) {
//         result = result + 1;
//     }
//     */
//     auto generator = new BytecodeGenerator;

//     // Create initial assignment
//     RAssignExpr initAssign = new RAssignExpr;
//     RIdentExpr initLhs = new RIdentExpr;
//     initLhs.ident = "result";
//     initAssign.lhs = initLhs;
//     RLiteralExpr initRhs = new RLiteralExpr;
//     initRhs.type = ADataType.ofInt;
//     initRhs.value = asBytes!int(10);
//     initAssign.rhs = initRhs;

//     // Create the while statement
//     RWhile whileStmt = new RWhile;
    
//     // Create the condition (result < 20)
//     RIdentExpr conditionExpr = new RIdentExpr;
//     conditionExpr.ident = "result";
//     RLiteralExpr compareValue = new RLiteralExpr;
//     compareValue.type = ADataType.ofInt;
//     compareValue.value = asBytes!int(20);
//     // TODO: Create comparison expression (result < 20)
//     whileStmt.condition = conditionExpr; // This will need to be updated with proper comparison

//     // Create the body block
//     RBlock bodyBlock = new RBlock;
//     whileStmt.body = bodyBlock;

//     // Create the increment statement (result = result + 1)
//     RAssignExpr incrementStmt = new RAssignExpr;
//     RIdentExpr incrementLhs = new RIdentExpr;
//     incrementLhs.ident = "result";
//     incrementStmt.lhs = incrementLhs;
    
//     // Create the addition expression (result + 1)
//     RIdentExpr addLhs = new RIdentExpr;
//     addLhs.ident = "result";
//     RLiteralExpr addRhs = new RLiteralExpr;
//     addRhs.type = ADataType.ofInt;
//     addRhs.value = asBytes!int(1);
//     // TODO: Create addition expression (result + 1)
//     incrementStmt.rhs = addRhs; // This will need to be updated with proper addition

//     // Add the increment statement to the body block
//     bodyBlock.statements ~= incrementStmt;
// 	bodyBlock.localsN = [incrementLhs.ident];
// 	bodyBlock.localsT = [ADataType.ofInt];

//     // Generate bytecode for the initial assignment
//     generator.generateAssignExprBytecode(initAssign);
    
//     // Generate bytecode for the while statement
//     generator.generateWhileBytecode(whileStmt);

//     // Print the generated bytecode
//     printBytecode(generator.bytecodeInstructions, "while statement with increment");
// }


