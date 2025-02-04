# Test Suite

## Parser

- **agg-member-list-01.alis**: List of 1 AggMember made
- **agg-member-list-02.alis**: List of 4 AggMembers made
- **alias-01.alis**: 2 Alias made
- **alias-02.alis**: Alias made with templates
- **anon-function-01.alis**: Anonymous Functions Made
- **anon-struct.alis**: 1 Anonymous Union made
- **anon-union-01.alis**: 1 Anonym./ous Union made
- **anon-union-02.alis**: 1 Anonymous Union made
- **attributes-01.alis**: 2 Variables made with attributes
- **attributes-02.alis**: <span style="color:red"> (Error) </span> Attributes must be placed in beginning while declaring Global Variable
- **attributes-03.alis**: <span style="color:red"> (Error) </span> Attributes must not be placed in beginning while declaring Local Variable
- **attributes-04.alis**: 1 Function, 3 Aggregations made with Attributes
- **block-01.alis**: 1 Empty Block and 1 Non-Empty Block made
- **block-02.alis**: 1 Empty Block-Expression and 1 Non-Empty Block-Expression made
- **do-while-loop-01.alis**: 3 blocks of do-while loops with 1 of them empty
- **enum-01.alis**: 1 Enum made with 3 members
- **enum-02.alis**: 1 Enum made with 3 members
- **enum-03.alis**: 2 Enums made with templates
- **for-01.alis**: 4 For-Loops made
- **function-01.alis**: Function Declarations Made
- **function-02.alis**: Function Declarations and Definitions Made
- **function-03.alis**: Functions with Templates Made
- **global-var-01.alis**: 2 Global Variables made with 2 attributes
- **global-var-02.alis**: <span style="color:red"> (Error) </span> Global var cannot be static
- **if-01.alis**: 3 blocks of if conditions with 2 of them empty
- **if-02.alis**: 2 blocks of if conditions
- **import-01.alis**: 6 modules imported
- **intrinsics-01.alis**: Function Declarations Made
- **literals-01.alis**: 2 local static variables
- **local-var-01.alis**: 2 local static variables
- **local-var-02.alis**: <span style="color:red"> (Error) </span> Variable should be declared with 'var'
- **local-var-03.alis**: 1 local variable made without 'var'
- **local-var-04.alis**: 1 Local variable made and initialized
- **mixin-01.alis**: Template Mixin'd
- **mixin-02.alis**: Aggregations Mixin'd
- **module-01.alis**: dummy test
- **static-for-01.alis**: 4 Static For-Loops made in a union
- **static-if-01.alis**: 3 blocks of static if conditions in a union
- **static-if-02.alis**: 2 blocks of static if conditions in a union
- **static-switch-01.alis**: 3 Switches
- **struct-01.alis**: Struct Made
- **struct-02.alis**: Struct and StructLiteralExpression made with cComp
- **struct-03.alis**: 2 Structs made with templates
- **switch-01.alis**: 3 Switches
- **switch-02.alis**: <span style="color:red"> (Error) </span> No default case present
- **templates-01.alis**: Templates made
- **union-01.alis**: Named Union Made
- **union-02.alis**: Unnamed Union Made
- **union-03.alis**: <span style="color:red"> (Error) </span> Union cannot have mixed named and unnamed members
- **union-04.alis**: Union made with Templates
- **utest-01.alis**: 1 empty utest, 2 non-empty utest made
- **while-loop-01.alis**: 3 blocks of while loops with 1 of them empty

## Semantic Analysis
**function-01.alis**<span style="color:red"> (Error) </span> Function with Return-Type but no return statement
**function-02.alis**<span style="color:red"> (Error) </span> Function with Return-Type but void returned
**function-03.alis**<span style="color:red"> (Error) </span> Function with incorrect type returned
**while-loop-01.alis**: 4 blocks of while loops with different conditon data-types
