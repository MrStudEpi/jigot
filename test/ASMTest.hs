--
-- EPITECH PROJECT, 2022
-- glados
-- File description:
-- ASMTest
--

module ASMTest (
    testsASM
) where

import Test.HUnit

import JigotAST
import JigotASM

--- Test for astToStackIST

-- Test with ASymbol "a"
astToStackISTSymbol :: Test
astToStackISTSymbol = TestCase (assertEqual [] (Right (Code [(Get "a", 0)], [])) (runCompiler (astToStackIST (ASymbol "a") 0) []))

-- Test with AToken "+"
astToStackISTToken :: Test
astToStackISTToken = TestCase (assertEqual [] (Right (Code [(Get "+", 0)], [])) (runCompiler (astToStackIST (AToken "+") 0) []))

-- Test with ANumber 1
astToStackISTNumber :: Test
astToStackISTNumber = TestCase (assertEqual [] (Right (Code [(Push (StackNumber 1), 0)], [])) (runCompiler (astToStackIST (ANumber 1) 0) []))

-- Test with AFloat 1.0
astToStackISTFloat :: Test
astToStackISTFloat = TestCase (assertEqual [] (Right (Code [(Push (StackFloat 1.0), 0)], [])) (runCompiler (astToStackIST (AFloat 1.0) 0) []))

-- Test with ABool True
astToStackISTBoolTrue :: Test
astToStackISTBoolTrue = TestCase (assertEqual [] (Right (Code [(Push (StackBool True), 0)], [])) (runCompiler (astToStackIST (ABool True) 0) []))

-- Test with ABool False
astToStackISTBoolFalse :: Test
astToStackISTBoolFalse = TestCase (assertEqual [] (Right (Code [(Push (StackBool False), 0)], [])) (runCompiler (astToStackIST (ABool False) 0) []))

-- Test with ALiteral "a"
astToStackISTLiteral :: Test
astToStackISTLiteral = TestCase (assertEqual [] (Right (Code [(Push (StackString "a"), 0)], [])) (runCompiler (astToStackIST (ALiteral "a") 0) []))

-- Test with GetLastIndex

getLastIndexTestOne :: Test
getLastIndexTestOne = TestCase (assertEqual [] 2 (getLastIndex [(Get "a", 0), (Get "b", 1), (Get "c", 2)]))

getLastIndexTestTwo :: Test
getLastIndexTestTwo = TestCase (assertEqual [] (-1) (getLastIndex []))

--- Test for show StackV (for debug)

-- Test with StackNumber 1
showStackVNumber :: Test
showStackVNumber = TestCase (assertEqual [] "1" (show (StackNumber 1)))

-- Test with StackFloat 1.0
showStackVFloat :: Test
showStackVFloat = TestCase (assertEqual [] "1.0" (show (StackFloat 1.0)))

-- Test with StackBool True
showStackVBoolTrue :: Test
showStackVBoolTrue = TestCase (assertEqual [] "True" (show (StackBool True)))

-- Test with StackBool False
showStackVBoolFalse :: Test
showStackVBoolFalse = TestCase (assertEqual [] "False" (show (StackBool False)))

-- Test with StackString "a"
showStackVString :: Test
showStackVString = TestCase (assertEqual [] "a" (show (StackString "a")))

-- Test with StackList [StackNumber 1]
showStackVList :: Test
showStackVList = TestCase (assertEqual [] "[1]" (show (StackArray [StackNumber 1])))

-- Test with Procedure
showStackVProcedure :: Test
showStackVProcedure = TestCase (assertEqual [] "Procedure" (show (StackProcedure (Get "l"))))

-- Test with EProcedure
showStackVEProcedure :: Test
showStackVEProcedure = TestCase (assertEqual [] "EProcedure >> Environment {ename = \"l\", retype = VInt, nextenv = [], value = Nothing}" (show (StackEProcedure (Environment "l" VInt [] Nothing))))

-- Test with StackStruct
showStackVStruct :: Test
showStackVStruct = TestCase (assertEqual [] "Struct >> (Environment {ename = \"l\", retype = VInt, nextenv = [], value = Nothing},\"lol\")" (show (StackStruct (Environment "l" VInt [] Nothing, "lol"))))

-- Test with StackArrayIndex
showStackVArrayIndex :: Test
showStackVArrayIndex = TestCase (assertEqual [] "ArrayIndex >> Environment {ename = \"l\", retype = VInt, nextenv = [], value = Nothing} 1" (show (StackArrayIndex (Environment "l" VInt [] Nothing) 1)))

testsASM :: Test
testsASM = TestList [
    astToStackISTSymbol,
    astToStackISTToken,
    astToStackISTNumber,
    astToStackISTFloat,
    astToStackISTBoolTrue,
    astToStackISTBoolFalse,
    astToStackISTLiteral,

    getLastIndexTestOne,
    getLastIndexTestTwo,

    showStackVNumber,
    showStackVFloat,
    showStackVBoolTrue,
    showStackVBoolFalse,
    showStackVString,
    showStackVList,
    showStackVProcedure,
    showStackVEProcedure,
    showStackVStruct,
    showStackVArrayIndex

    ]