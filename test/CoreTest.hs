--
-- EPITECH PROJECT, 2022
-- glados
-- File description:
-- CoreTest
--

module CoreTest (
    testsCore
) where

import Test.HUnit

import JigotASM
import Core

import Control.Exception
import System.IO.Unsafe
import GHC.IO.Handle
import System.IO
import System.Exit

-- Test for compile

readHandler :: IOError -> IO a
readHandler _ = putStrLn "Error" >> exitWith (ExitFailure 84)

getContentFile :: String -> IO (Maybe String)
getContentFile filename =  do
    content <- catch (readFile filename) readHandler
    return $ Just content

-- Id Path Expected_Output Expected_InEnv Test
testFile :: String -> StackV -> String -> Assertion
testFile filename o outputmsg = case unsafePerformIO $ getContentFile filename of
    Just content -> case compile [] content of
            Left x -> assertEqual [] outputmsg x
            Right (Nothing, _) -> assertFailure "Error: No return value"
            Right (Just x, _) -> do
                hs <- openFile "./test/files/output_entry" WriteMode
                hDuplicateTo hs stdout
                hClose hs
                content <- readFile "./test/files/output_entry"
                assertEqual [] (o, outputmsg) (x, content)
    Nothing -> assertFailure "Error"

-- Basic test

compileParsingErrorTest :: Test
compileParsingErrorTest = TestCase (assertEqual [] (Left "Error: Compiler was unable to parse the code (Parser), reason: unexpected start of input") (compile [] "/*"))

compileCPTASTErrorTest :: Test
compileCPTASTErrorTest = TestCase (assertEqual [] (Left "Error: Compiler found invalid expression (CPT->AST)") (compile [] "x void = true;"))

-- Test with  Files

compileBuiltins1 :: Test
compileBuiltins1 = TestLabel "Compile Builtins1 File" $ TestCase (testFile "./test/files/builtins1.jg" (StackNumber 1) "a\n")

compileBuiltins2 :: Test
compileBuiltins2 = TestLabel "Compile Builtins2 File" $ TestCase (testFile "./test/files/builtins2.jg" (StackNumber 0) "True\n")

compileBuiltins3 :: Test
compileBuiltins3 = TestLabel "Compile Builtins3 File" $ TestCase (testFile "./test/files/builtins3.jg" (StackNumber 0) "False\n")

compileCall :: Test
compileCall = TestLabel "Compile Call File" $ TestCase (testFile "./test/files/call.jg" (StackNumber 5) "")

compileEmptyFile :: Test
compileEmptyFile = TestLabel "Compile Empty File" $ TestCase (testFile "./test/files/emptyFile.jg" (StackNumber 0) "Error: Compiler was unable to parse the code (Parser), reason: unexpected start of input")

compileEmptyMain :: Test
compileEmptyMain = TestLabel "Compile Empty Main File" $ TestCase (testFile "./test/files/emptyMain.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Type error: not a void function")

compileEqualityFile :: Test
compileEqualityFile = TestLabel "Compile Equality File" $ TestCase (testFile "./test/files/equality.jg" (StackNumber 0) "True\nTrue\nFalse\nTrue\nTrue\nFalse\nFalse\nTrue\nTrue\nFalse\nTrue\n")

compileFactorialFile :: Test
compileFactorialFile = TestLabel "Compile Factorial File" $ TestCase (testFile "./test/files/factorial.jg" (StackNumber 0) "3628800\n")

compileCommentFile :: Test
compileCommentFile = TestLabel "Compile Comment File" $ TestCase (testFile "./test/files/commentfile.jg" (StackNumber 0) "")

compileExitFile :: Test
compileExitFile = TestLabel "Compile Exit File" $ TestCase (testFile "./test/files/exitTest.jg" (StackNumber 33) "hello world\n")

compileForEachFile :: Test
compileForEachFile = TestLabel "Compile ForEach File" $ TestCase (testFile "./test/files/foreach.jg" (StackNumber 0) "hello\nworld\n")

compileForFile :: Test
compileForFile = TestLabel "Compile ForLoop File" $ TestCase (testFile "./test/files/forloop.jg" (StackNumber 0) "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n")

compileForLoopAllFile :: Test
compileForLoopAllFile = TestLabel "Compile ForLoopAll File" $ TestCase (testFile "./test/files/forloopAll.jg" (StackNumber 0) "10\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n20\n30\n")

compileIfFile :: Test
compileIfFile = TestLabel "Compile If File" $ TestCase (testFile "./test/files/ifcheck.jg" (StackNumber 0) "this is a string\nhello\n")

compileIndexFile :: Test
compileIndexFile = TestLabel "Compile Index File" $ TestCase (testFile "./test/files/indextest.jg" (StackNumber 0) "hello\n5\nl\nhellol\nhellol\n")

compileIntinFloatAndInv :: Test
compileIntinFloatAndInv = TestLabel "Compile IntinFloatAndInv File" $ TestCase (testFile "./test/files/intInFloat.jg" (StackNumber 0) "56\n")

compileLinkedFile :: Test
compileLinkedFile = TestLabel "Compile Linked File" $ TestCase (testFile "./test/files/linked.jg" (StackNumber 0) "hello\nmaybe\ntwo\n[world,maybe,one]\n")

compileFindSubStringFile :: Test
compileFindSubStringFile = TestLabel "Compile Find SubString File" $ TestCase (testFile "./examples/find_substr.jg" (StackNumber 0) "3\n-1\n")

compileScopedFile :: Test
compileScopedFile = TestLabel "Compile Scoped File" $ TestCase (testFile "./test/files/scoped.jg" (StackNumber 40) "42\n43\n85\n42\n")

compileStructEmptyFile :: Test
compileStructEmptyFile = TestLabel "Compile Struct Empty File" $ TestCase (testFile "./test/files/struct_empty.jg" (StackNumber 0) "")

compileStructFile :: Test
compileStructFile = TestLabel "Compile Struct File" $ TestCase (testFile "./test/files/structuretest.jg" (StackNumber 0) "5\n10\n20\n")

compileTailOperationFile :: Test
compileTailOperationFile = TestLabel "Compile Tail Operator File" $ TestCase (testFile "./test/files/tailOperation.jg" (StackNumber 0) "[0,2,3]\nuif\n")

compileArrayFile :: Test
compileArrayFile = TestLabel "Compile Array File" $ TestCase (testFile "./test/files/test_array.jg" (StackNumber 0) "4\n5\n")

compileWhileFile :: Test
compileWhileFile = TestLabel "Compile While File" $ TestCase (testFile "./test/files/whileloop.jg" (StackNumber 0) "⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿\n⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿\n⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⣿⣿⣿⣿\n⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⠟⢉⣉⠙⠻⣿⣿⡿⠋⠀⠈⢻⣿⣿\n⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠟⠋⠀⠀⢸⢩⣽⣦⣨⠟⠁⠀⠀⠀⠚⠿⣿\n⣿⣿⣿⣿⣿⣿⣿⡿⠿⠟⠛⠉⠀⠀⠀⠀⠀⠸⡜⣿⠟⠁⠀⠀⠀⠀⠀⠀⠀⣸\n⣿⣿⣿⣿⣿⣿⡏⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢳⡘⣧⣀⣠⣤⠶⣶⣿⣿⣿⣿\n⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠳⣌⠻⣿⣿⠀⢿⣿⣿⣿⣿\n⣿⣿⣿⣿⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠳⠶⠾⢀⣿⣿⣿⣿⣿\n⣿⡿⠛⠛⠛⠛⢿⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣠⣤⣶⣿⣿⣿⣿⣿⣿\n⣿⡄⠀⠀⠀⠀⠈⢿⣆⠀⠀⠀⠀⠀⠀⠀⠀⣠⣴⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿\n⣿⣿⣶⡖⠀⠀⠀⣴⣿⣷⣄⡀⠀⠀⠀⣴⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿\n⣿⣿⣿⣇⠀⢀⣾⣿⣿⣿⣿⣿⣷⣶⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿\n⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿\n⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿\n1\n0\n")

compileArrayOperandFile :: Test
compileArrayOperandFile = TestLabel "Compile Array Operand File" $ TestCase (testFile "./test/files/arrayOperand.jg" (StackNumber 0) "world\nJEJEJEJJEJE\nlol\nLOL\n[]\nL\nOL\nL\nO\n[World]\n(empty)\n")

compileDeclareVarFile :: Test
compileDeclareVarFile = TestLabel "Compile Declare Var File" $ TestCase (testFile "./test/files/declareVar.jg" (StackNumber 0) "Error: Compiler found invalid expression (CPT->AST)")

compileEmptyStructDefFile :: Test
compileEmptyStructDefFile = TestLabel "Compile Empty Struct Def File" $ TestCase (testFile "./test/files/declStruct.jg" (StackNumber 0) "0\nYay\n")

compileStructureWithinFile :: Test
compileStructureWithinFile = TestLabel "Compile Structure Within File" $ TestCase (testFile "./test/files/errors/struct_within.jg" (StackNumber 0) "Error: Compiler was unable to parse the code (Parser), reason: unexpected end of input")

compileFooFile :: Test
compileFooFile = TestLabel "Compile Foo File" $ TestCase (testFile "./test/files/foo.jg" (StackNumber 42) "")

compileAddinFunctionFile :: Test
compileAddinFunctionFile = TestLabel "Compile Add in Function File" $ TestCase (testFile "./test/files/addInFunction.jg" (StackNumber 0) "4\n")

compileIfBasic1File :: Test
compileIfBasic1File = TestLabel "Compile If Basic 1 File" $ TestCase (testFile "./test/files/if1.jg" (StackNumber 1) "")

compileIfBasic2File :: Test
compileIfBasic2File = TestLabel "Compile If Basic 2 File" $ TestCase (testFile "./test/files/if2.jg" (StackNumber 2) "")

compileIfBasic3File :: Test
compileIfBasic3File = TestLabel "Compile If Basic 3 File" $ TestCase (testFile "./test/files/if3.jg" (StackNumber 21) "")

compileListFile1 :: Test
compileListFile1 = TestLabel "Compile List File 1" $ TestCase (testFile "./test/files/list1.jg" (StackNumber 0) "1\n")

compileListFile2 :: Test
compileListFile2 = TestLabel "Compile List File 2" $ TestCase (testFile "./test/files/list2.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Index out of bounds: x[0]")

compileMultipleFnWithSameVariableName :: Test
compileMultipleFnWithSameVariableName = TestLabel "Compile Multiple Fn With Same Variable Name" $ TestCase (testFile "./test/files/multipleFnWithSameVariableName.jg" (StackNumber 0) "")

compileMultipleGlobals :: Test
compileMultipleGlobals = TestLabel "Compile Multiple Globals" $ TestCase (testFile "./test/files/multipleGlobals.jg" (StackNumber 0) "10\n1.3\n")

compileMultiplyFile :: Test
compileMultiplyFile = TestLabel "Compile Multiply File" $ TestCase (testFile "./test/files/multiply.jg" (StackNumber 0) "4\n")

compileOnlyFunctionFile :: Test
compileOnlyFunctionFile = TestLabel "Compile Only Function File" $ TestCase (testFile "./test/files/onlyFunction.jg" (StackNumber 0) "Error: reference indefinie vers main")

compileOnlyGlobalFile :: Test
compileOnlyGlobalFile = TestLabel "Compile Only Global File" $ TestCase (testFile "./test/files/onlyGlobals.jg" (StackNumber 0) "Error: reference indefinie vers main")

compileOperandIfFile :: Test
compileOperandIfFile = TestLabel "Compile Operand If File" $ TestCase (testFile "./test/files/operandInIf.jg" (StackNumber 0) "False\nTrue\nTrue\nFalse\nTrue\nTrue\nFalse\nTrue\n")

compileReadFile :: Test
compileReadFile = TestLabel "Compile Read File" $ TestCase (testFile "./test/files/readFile.jg" (StackNumber 0) "# Glados (Part 1 - Jigot)\\n\\nAll features that implements our language (Jigot)\\n\\n# Installation\\n\\n1 - Download from repository the project:\\n\\nhttps://github.com/EpitechPromo2025/B-FUN-500-STG-5-1-glados-paul.gazeau-rousseau\\n\\n2 - Launch Makefile\\n\\n```bash\\nmake\\n```\\n\\n3 - If you want to launch unit test used for this project\\n\\n```bash\\nmake test `OR` stack test\\n```\\n\\n# Usage\\n\\nYou can run the program after compile it by two differents ways:\\n\\n1- Passing File as arguments (it will stop if it fails on a file)\\n\\n```bash\\n./glados file1 file2 filen\\n\\n```\\n2 - Run the interpreter mode\\n```bash\\n./glados -i\\n```\\n\\n# Interpreter\\n\\n## Useful commands\\n\\n- (\\\":c\\\") -> clear the buffer that been filled if an equation isn't comple\\n- (\\\":s\\\") -> show the buffer\\n- (\\\":e\\\") -> show the environment\\n- (\\\":ce\\\") -> clear environment\\n- (\\\":q\\\") -> exit interpreter mode\\n- (\\\":h\\\") -> command menu\\n- (\\\":m\\\") -> multiline mode\\n- (\\\":h\\\") -> print help\\n- (\\\":r\\\") -> Run the code in the current buffer\\n- (\\\":l\\\") -> Load a file in the interpreter\\n- (\\\":jigot\\\") -> Print information about the Jigot language\\n\\n## Misc\\n\\n- What's your favorite food ? - Print until sigint the language LOGO\\n\\n## Useful Stuff\\n\\nIn the interpreter in the line when you can write it display the last exit code that it have handle so:\\n\\n```bash\\njigot 0> ... -> Last return was 0\\n```\\n\\n```bash\\njigot 84> ... -> Last return was 84\\n```\\n\\nTo see all differents syntax go to our documentation: https://glados-jigot.gitbook.io/jigot\n")

compileWriteFile :: Test
compileWriteFile = TestLabel "Compile Write File" $ TestCase (testFile "./test/files/writeFile.jg" (StackNumber 0) "")

compileVoidFunctionFile :: Test
compileVoidFunctionFile = TestLabel "Compile Void Function File" $ TestCase (testFile "./test/files/voidFunction.jg" (StackNumber 0) "lol\n")

compileVoidFunctionErrorFile :: Test
compileVoidFunctionErrorFile = TestLabel "Compile Void Function Error File" $ TestCase (testFile "./test/files/voidFunctionError.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Type error: got VInt, but expected: VFunc (VGlobal VVoid)")

compileTernaryFile :: Test
compileTernaryFile = TestLabel "Compile Ternary File" $ TestCase (testFile "./test/files/ternary.jg" (StackNumber 0) "1\n1\n")

compileVariableReturnFile :: Test
compileVariableReturnFile = TestLabel "Compile Variable Return File" $ TestCase (testFile "./test/files/variableReturn.jg" (StackNumber 0) "kirikoo\n")

compileAssignFile :: Test
compileAssignFile = TestLabel "Compile Assignement File" $ TestCase (testFile "./test/files/assignement.jg" (StackNumber 0) "100\nlolb\n")

compileTestMultipleDeclSameName :: Test
compileTestMultipleDeclSameName = TestLabel "Compile Test Multiple Decl Same Name" $ TestCase (testFile "./test/files/testMultDeclSameName.jg" (StackNumber 0) "2\n")

compileTestWithVariable :: Test
compileTestWithVariable = TestLabel "Compile Test With Variable" $ TestCase (testFile "./test/files/callFnWithVariables.jg" (StackNumber 5) "")

compileExprAfterReturn :: Test
compileExprAfterReturn = TestLabel "Compile Expr After Return" $ TestCase (testFile "./test/files/exprAfterRet.jg" (StackNumber 0) "")

compileGlobalUsage :: Test
compileGlobalUsage = TestLabel "Compile Global Usage" $ TestCase (testFile "./test/files/globalUsage.jg" (StackNumber 0) "10\n0\n-10\n")

compileCallWithVarAndFuncSameName :: Test
compileCallWithVarAndFuncSameName = TestLabel "Compile Call With Var And Func Same Name" $ TestCase (testFile "./test/files/varAndFuncSName.jg" (StackNumber 4) "")

compileModifyArray :: Test
compileModifyArray = TestLabel "Compile Modify Array" $ TestCase (testFile "./test/files/modifyArray.jg" (StackNumber 0) "hello\nworld\n[world]\na\nbmdulila\n")

compileAllOperationTest :: Test
compileAllOperationTest = TestLabel "Compile All Operation Test" $ TestCase (testFile "./test/files/allOperation.jg" (StackNumber 0) "3\n2.0\n3\n2.0\n3\n2.0\n0\n-3\n0.0\nHello world\nFalse\nTrue\nTrue\nTrue\nTrue\n(empty)\nwow\n[wow,lol]\nTrue\nFalse\nFalse\nFalse\nFalse\nFalse\nFalse\nFalse\nTrue\nTrue\nTrue\nFalse\nFalse\nTrue\nTrue\nTrue\nTrue\nTrue\n25\n25.0\n-3\n-3.0\n[wow,lol,4]\n")

compileCastingTest :: Test
compileCastingTest = TestLabel "Compile Casting Test" $ TestCase (testFile "./test/files/casting.jg" (StackNumber 0) "0\n2\n0\n1\n2.0\n1.0\n0.0\n1.0\nTrue\nTrue\nTrue\nTrue\nTrue\nHello world\n1\n1.0\n1.0\n[0,1,2,3,4,5]\n2\nTrue\nFalse\n1\n0\n1.0\n0.0\n")

compileBitwiseTest :: Test
compileBitwiseTest = TestLabel "Compile Bitwise Test" $ TestCase (testFile "./test/files/bitwise.jg" (StackNumber 0) "3\n1\n1\n2\n0\n")

compileNotExprTest :: Test
compileNotExprTest = TestLabel "Compile Not Expr Test" $ TestCase (testFile "./test/files/ifnot.jg" (StackNumber 0) "")

compileNonDeclOperation :: Test
compileNonDeclOperation = TestLabel "Compile Non Decl Operation Error" $ TestCase (testFile "./test/files/nondeclOp.jg" (StackNumber 0) "")

compileNotBool :: Test
compileNotBool = TestLabel "Compile Not Bool" $ TestCase (testFile "./test/files/notBool.jg" (StackNumber 0) "")

compileDotInstantFile :: Test
compileDotInstantFile = TestLabel "Compile Dot Instant File" $ TestCase (testFile "./test/files/dotInstant.jg" (StackNumber 0) "hellol\n")

compileDefaultStruct :: Test
compileDefaultStruct = TestLabel "Compile Default Struct File" $ TestCase (testFile "./test/files/defStruct.jg" (StackNumber 0) "(empty)\n0\n0.0\n[]\nFalse\n")

-- Advanced tests

compileAdvancedStructureTest :: Test
compileAdvancedStructureTest = TestLabel "Compile Advanced Structure Test" $ TestCase (testFile "./test/files/advanced/structure_advanced.jg" (StackNumber 0) "Yay im only a human after all\nRagnBogen\n37\nTrue\nDavid Guetta\n55\nTrue\nMickael Jackson\n50\nFalse\n")

compileAdvancedLotOfGlobalTest :: Test
compileAdvancedLotOfGlobalTest = TestLabel "Compile Advanced Lot Of Global Test" $ TestCase (testFile "./test/files/advanced/lotofglobals.jg" (StackNumber 0) "0\n42\nhello\n19\n")

-- Error Handling

compileErrorFileOne :: Test
compileErrorFileOne = TestLabel "Compile Error File One" $ TestCase (testFile "./test/files/errors/errorFile1.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Not valid parameters for operation")

compileFunctionNotDefined :: Test
compileFunctionNotDefined = TestLabel "Compile Function Not Defined" $ TestCase (testFile "./test/files/errors/functionNotDefined.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Variable not found: lolita")

compileWrongTypeFileOne :: Test
compileWrongTypeFileOne = TestLabel "Compile Wrong Type File One" $ TestCase (testFile "./test/files/errors/wrongType1.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Type mismatch (3): expected VBool but got hello")

compileWrongTypeFileTwo :: Test
compileWrongTypeFileTwo = TestLabel "Compile Wrong Type File Two" $ TestCase (testFile "./test/files/errors/wrongType2.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Type mismatch (3): expected VString but got 12")

compileWrongTypeFileThree :: Test
compileWrongTypeFileThree = TestLabel "Compile Wrong Type File Three" $ TestCase (testFile "./test/files/errors/wrongType3.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Type mismatch (3): expected VInt but got False")

compileInvalidExpression :: Test
compileInvalidExpression = TestLabel "Compile Invalid Append Operation" $ TestCase (testFile "./test/files/errors/invalidExpr.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Invalid expression: Expression \"append\" with stack: Stack [[1.0,2.0,3.0]]")

compileWrongArgumentPassedIn :: Test
compileWrongArgumentPassedIn = TestLabel "Compile Wrong Argument Passed In" $ TestCase (testFile "./test/files/errors/wrongArgument.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Missing or wrong arguments for function: lol")

compileInvalidNumberOfArguments :: Test
compileInvalidNumberOfArguments = TestLabel "Compile Invalid Number Of Arguments" $ TestCase (testFile "./test/files/errors/invalidNbArgs.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Missing or wrong arguments for function: lol")

compileReservedKeyword :: Test
compileReservedKeyword = TestLabel "Compile Reserved Keyword" $ TestCase (testFile "./test/files/errors/reservedKeyword.jg" (StackNumber 0) "Error: Compiler was unable to compile the code (AST->IST), reason: if is a reserved keyword")

compileReservedKeyword2 :: Test
compileReservedKeyword2 = TestLabel "Compile Reserved Keyword 2" $ TestCase (testFile "./test/files/errors/reservedKeyword2.jg" (StackNumber 0) "Error: Compiler was unable to compile the code (AST->IST), reason: string is a type cannot be used for name")

compileReservedKeyword3 :: Test
compileReservedKeyword3 = TestLabel "Compile Reserved Keyword 3" $ TestCase (testFile "./test/files/errors/reservedKeyword3.jg" (StackNumber 0) "Error: Compiler was unable to compile the code (AST->IST), reason: if is a reserved keyword")

compileMultipleVarSameName :: Test
compileMultipleVarSameName = TestLabel "Compile Multiple Var Same Name" $ TestCase (testFile "./test/files/errors/multipleVarSameName.jg" (StackNumber 0) "Error: Compiler was unable to compile the code (AST->IST), reason: Variable already defined: x")

compileMultipleVarSameName2 :: Test
compileMultipleVarSameName2 = TestLabel "Compile Multiple Var Same Name 2" $ TestCase (testFile "./test/files/errors/multipleVarSameName2.jg" (StackNumber 0) "Error: Compiler was unable to compile the code (AST->IST), reason: Duplicate variable or function")

compileCallWithVarAndFuncSameNameError :: Test
compileCallWithVarAndFuncSameNameError = TestLabel "Compile Call With Var And Func Same Name" $ TestCase (testFile "./test/files/errors/varAndFuncSNameError.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Your code contains two environments with the same name, please fix it")

compileMultipleFnWithSameName :: Test
compileMultipleFnWithSameName = TestLabel "Compile Multiple Fn With Same Name" $ TestCase (testFile "./test/files/errors/multipleFnSameName.jg" (StackNumber 0) "Error: Compiler was unable to compile the code (AST->IST), reason: Function already defined: lol")

compileVariableWithSameName :: Test
compileVariableWithSameName = TestLabel "Compile Variable With Same Name" $ TestCase (testFile "./test/files/errors/multipleVarSameName.jg" (StackNumber 0) "Error: Compiler was unable to compile the code (AST->IST), reason: Variable already defined: x")

compileFuncAndVarSameName :: Test
compileFuncAndVarSameName = TestLabel "Compile Func And Var Same Name" $ TestCase (testFile "./test/files/errors/FuncAndvarSName.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Your code contains two environments with the same name, please fix it")

compileDivisionByZero :: Test
compileDivisionByZero = TestLabel "Compile Division By Zero" $ TestCase (testFile "./test/files/errors/div0.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Division by zero")

compileDivisionByZeroFloat :: Test
compileDivisionByZeroFloat = TestLabel "Compile Division By Zero Float" $ TestCase (testFile "./test/files/errors/divfloat0.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Division by zero")

compileModuloByZero :: Test
compileModuloByZero = TestLabel "Compile Modulo By Zero" $ TestCase (testFile "./test/files/errors/mod0.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Modulo by zero")

compileModuloWithFloat :: Test
compileModuloWithFloat = TestLabel "Compile Modulo With Float" $ TestCase (testFile "./test/files/errors/modfloat.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Not valid parameters for operation")

compileStructAlreadyDefinedError :: Test
compileStructAlreadyDefinedError = TestLabel "Compile Struct Already Defined Error" $ TestCase (testFile "./test/files/errors/struct_alreadyDef.jg" (StackNumber 0) "Error: Compiler was unable to compile the code (AST->IST), reason: Structure already defined: lol")

compileStructNotFoundError :: Test
compileStructNotFoundError = TestLabel "Compile Struct Not Found Error" $ TestCase (testFile "./test/files/errors/structure_notFound.jg" (StackNumber 0) "Error: Compiler was unable to compile the code (AST->IST), reason: Structure not found: test")

compileSubFunction :: Test
compileSubFunction = TestLabel "Compile Sub Function" $ TestCase (testFile "./test/files/errors/subFunction.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Variable not found: x")

compileSyntaxError :: Test
compileSyntaxError = TestLabel "Compile Syntax Error" $ TestCase (testFile "./test/files/errors/syntaxError.jg" (StackNumber 0) "Error: Compiler was unable to parse the code (Parser), reason: unexpected start of input")

compileSyntaxError2 :: Test
compileSyntaxError2 = TestLabel "Compile Syntax Error 2" $ TestCase (testFile "./test/files/errors/syntaxError2.jg" (StackNumber 0) "Error: Compiler was unable to parse the code (Parser), reason: unexpected end of input")

compileCastingError :: Test
compileCastingError = TestLabel "Compile Casting Error" $ TestCase (testFile "./test/files/errors/casting_error.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Not valid parameters for operation")

compileInvalidReturnVal :: Test
compileInvalidReturnVal = TestLabel "Compile Invalid Return Val" $ TestCase (testFile "./test/files/errors/invalidRetValue.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Type error: got VString, but expected: VFunc (VGlobal VInt)")

compileReadFileDoesNotExist :: Test
compileReadFileDoesNotExist = TestLabel "Compile Read File Does Not Exist" $ TestCase (testFile "./test/files/errors/readFileIF.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: File not found")

compileReadFileInvalidValue :: Test
compileReadFileInvalidValue = TestLabel "Compile Read File Invalid Value" $ TestCase (testFile "./test/files/errors/readFileIV.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Not a string")

compileInvalidEqstruct :: Test
compileInvalidEqstruct = TestLabel "Compile Invalid Eqstruct" $ TestCase (testFile "./test/files/errors/invalidEqStruct.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Type mismatch (1): expected VInt but got hello")

compileWrongDeclarationInStruct :: Test
compileWrongDeclarationInStruct = TestLabel "Compile Wrong Declaration In Struct" $ TestCase (testFile "./test/files/errors/wrongDeclarationInStruct.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Attribute not found: x, in uuuu")

compileWrongStructDeclaration :: Test
compileWrongStructDeclaration = TestLabel "Compile Wrong Struct Declaration" $ TestCase (testFile "./test/files/errors/wrongStructDeclaration.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Variable not found: uuuu")

compileCastErr :: Test
compileCastErr = TestLabel "Compile Cast Err" $ TestCase (testFile "./test/files/errors/casterr.jg" (StackNumber 0) "Error: Compiler found invalid expression (CPT->AST)")

compileNotStr :: Test
compileNotStr = TestLabel "Compile Not Str" $ TestCase (testFile "./test/files/errors/notStr.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Not a valid value")

compileDotInstantErrorFile :: Test
compileDotInstantErrorFile = TestLabel "Compile Dot Instant Error File" $ TestCase (testFile "./test/files/errors/dotInstantError.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Invalid expression: Expression \"len\" with stack: Stack [42,Cast >> VInt]")

compileIndexOutOfBoundsError :: Test
compileIndexOutOfBoundsError = TestLabel "Compile Index Out Of Bounds Error" $ TestCase (testFile "./test/files/errors/indexOutOfBounds.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Index out of bounds: content[10]")

compileIndexNotANumberError :: Test
compileIndexNotANumberError = TestLabel "Compile Index Not A Number Error" $ TestCase (testFile "./test/files/errors/indexNotNumber.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Not a number")

compileInvalidExitData :: Test
compileInvalidExitData = TestLabel "Compile Invalid Exit Data" $ TestCase (testFile "./test/files/errors/invalidExit.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Exit code must be a number")

compileMissingReturn :: Test
compileMissingReturn = TestLabel "Compile Missing Return" $ TestCase (testFile "./test/files/errors/missingRet.jg" (StackNumber 0) "Error: Evaluator was unable to execute the code (IST->Stack), reason: Function (\"lol\") did not return a value")

compileStructVariableAlreadyDefined :: Test
compileStructVariableAlreadyDefined = TestLabel "Compile Struct Variable Already Defined" $ TestCase (testFile "./test/files/errors/struct_alreadyDef2.jg" (StackNumber 0) "Error: Compiler was unable to compile the code (AST->IST), reason: Variable already defined: lol")

testsCore :: Test.HUnit.Test
testsCore = TestList [  

    compileParsingErrorTest,
    compileCPTASTErrorTest,

    compileCommentFile,

    compileBuiltins1,
    compileBuiltins2,
    compileBuiltins3,
    compileCall,
    compileFactorialFile,
    compileEmptyFile,
    compileEmptyMain,
    compileExitFile,
    compileEqualityFile,

    compileForEachFile,
    compileForFile,
    compileForLoopAllFile,
    compileIfFile,
    compileIndexFile,
    compileIntinFloatAndInv,
    compileLinkedFile,
    compileFindSubStringFile,
    compileScopedFile,
    compileStructEmptyFile,
    compileStructFile,
    compileTailOperationFile,
    compileArrayFile,
    compileArrayOperandFile,
    compileWhileFile,
    compileDeclareVarFile,
    compileEmptyStructDefFile,
    compileStructureWithinFile,

    compileFooFile,
    compileAddinFunctionFile,
    compileReadFile,
    compileWriteFile,
    compileIfBasic1File,
    compileIfBasic2File,
    compileIfBasic3File,
    compileListFile1,
    compileListFile2,
    compileMultipleFnWithSameVariableName,
    compileMultipleGlobals,
    compileMultiplyFile,
    compileOnlyFunctionFile,
    compileOnlyGlobalFile,
    compileOperandIfFile,
    compileTernaryFile,
    compileVariableReturnFile,
    compileAssignFile,
    compileTestMultipleDeclSameName,
    compileTestWithVariable,
    compileExprAfterReturn,
    compileGlobalUsage,
    compileCallWithVarAndFuncSameName,

    compileAdvancedStructureTest,
    compileAdvancedLotOfGlobalTest,

    compileErrorFileOne,
    compileFunctionNotDefined,
    compileWrongTypeFileOne,
    compileWrongTypeFileTwo,
    compileWrongTypeFileThree,
    compileInvalidExpression,
    compileWrongArgumentPassedIn,
    compileInvalidNumberOfArguments,
    compileReservedKeyword,
    compileReservedKeyword2,
    compileReservedKeyword3,
    compileInvalidEqstruct,
    compileWrongDeclarationInStruct,
    compileWrongStructDeclaration,
    compileCastErr,
    compileNotStr,
    compileNotBool,
    compileDotInstantFile,
    compileDefaultStruct,
    compileDotInstantErrorFile,
    compileIndexOutOfBoundsError,
    compileIndexNotANumberError,
    compileInvalidExitData,
    compileMissingReturn,
    compileStructVariableAlreadyDefined,
    compileMultipleVarSameName,
    compileMultipleVarSameName2,
    compileCallWithVarAndFuncSameNameError,
    compileMultipleFnWithSameName,
    compileVariableWithSameName,
    compileFuncAndVarSameName,
    compileNonDeclOperation,
    compileAllOperationTest,
    compileCastingTest,
    compileCastingError,
    compileInvalidReturnVal,
    compileReadFileDoesNotExist,
    compileReadFileInvalidValue,
    compileBitwiseTest,
    compileModifyArray,
    compileNotExprTest,
    compileVoidFunctionFile,
    compileDivisionByZero,
    compileDivisionByZeroFloat,
    compileModuloByZero,
    compileModuloWithFloat,
    compileStructAlreadyDefinedError,
    compileStructNotFoundError,
    compileSubFunction,
    compileSyntaxError,
    compileSyntaxError2,

    compileVoidFunctionErrorFile


    -- Without output (because the other are instable)
    ]