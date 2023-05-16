--
-- EPITECH PROJECT, 2022
-- glados
-- File description:
-- ASTest
--

module ASTest (
    testsASTS
) where

import Test.HUnit

import JigotAST

-- Test for cptToAST

-- Test with Symbol "true"
cptToASTTrue :: Test
cptToASTTrue = TestCase (assertEqual [] (Just (ABool True)) (cptToAST (Symbol "true")))

-- Test with Symbol "false"
cptToASTFalse :: Test
cptToASTFalse = TestCase (assertEqual [] (Just (ABool False)) (cptToAST (Symbol "false")))

-- Test with Symbol "a"
cptToASTSymbol :: Test
cptToASTSymbol = TestCase (assertEqual [] (Just (ASymbol "a")) (cptToAST (Symbol "a")))

-- Test with Symbol "int"
cptToASTSymbolType :: Test
cptToASTSymbolType = TestCase (assertEqual [] (Just (AType VInt)) (cptToAST (Symbol "int")))

-- Test with StringLiteral "a"
cptToASTStringLiteral :: Test
cptToASTStringLiteral = TestCase (assertEqual [] (Just (ALiteral "a")) (cptToAST (StringLiteral "a")))

-- Test with Number 1
cptToASTNumber :: Test
cptToASTNumber = TestCase (assertEqual [] (Just (ANumber 1)) (cptToAST (Number 1)))

-- Test with CFloat 1.0
cptToASTFloat :: Test
cptToASTFloat = TestCase (assertEqual [] (Just (AFloat 1.0)) (cptToAST (CFloat 1.0)))

-- Test with Array [Symbol "a", Symbol "b"]
cptToASTArray :: Test
cptToASTArray = TestCase (assertEqual [] (Just (AArray [ABool True, ABool False])) (cptToAST (Array [Symbol "true", Symbol "false"])))

-- Test with Operation Basic
cptToASTOperationBasic :: Test
cptToASTOperationBasic = TestCase (assertEqual [] (Just (Call (AToken "+") [ANumber 1, ANumber 2])) (cptToAST (List [Number 1, Token "+", Number 2])))

-- Test with Operation Complex
cptToASTOperationComplex :: Test
cptToASTOperationComplex = TestCase (assertEqual [] (Just (Call (AToken "+") [ANumber 1, Call (AToken "*") [ANumber 2, ANumber 3]])) (cptToAST (List [Number 1, Token "+", List [Number 2, Token "*", Number 3]])))

-- Test with Variable -> List "a" "int" ";"
cptToASTCreateVariableWithoutDef :: Test
cptToASTCreateVariableWithoutDef = TestCase (assertEqual [] (Just (Variable "a" VInt (ANumber 0))) (cptToAST (List [List [Symbol "a", Symbol "int"], Token ";"])))

-- Test with Variable -> List "a" "int" "=" "1" ";"
cptToASTCreateVariableWithDef :: Test
cptToASTCreateVariableWithDef = TestCase (assertEqual [] (Just (Variable {vname = "a", vtype = VInt, vvalue = Call {function = AToken "::", args = [ANumber 1,AType VInt]}})) (cptToAST (List [List [Symbol "a", Symbol "int", Token "=", Number 1], Token ";"])))

cptToASTReDefineVariable :: Test
cptToASTReDefineVariable = TestCase (assertEqual [] (Just (Call (AToken "=") [ASymbol "a", ANumber 1])) (cptToAST (List [List [Symbol "a", Token "=", Number 1], Token ";"])))

cptToASTCallFunctionOne :: Test
cptToASTCallFunctionOne = TestCase (assertEqual [] (Just (Call (ASymbol "print") [ASymbol "a"])) (cptToAST (List [List [Symbol "a", Symbol "print"], Token ";"])))

cptToASTCallFunctionTwo :: Test
cptToASTCallFunctionTwo = TestCase (assertEqual [] (Just (Call (ASymbol "onClick") [ANumber 0, ANumber 42])) (cptToAST (List [List [List [Number 0, Number 42], Symbol "onClick"], Token ";"])))

-- Test with Function -> List "a" "int" [List "b" "int"] { Number 0, Symbol "return", Token ";" }
cptToASTCreateFunction :: Test
cptToASTCreateFunction = TestCase (assertEqual [] (Just (Lambda "a" VInt (ASeq [Variable "b" VInt (ANumber 0)]) [Call (ASymbol "return") [ANumber 0]])) (cptToAST (List [Symbol "a", Symbol "int", Parameters [List [Symbol "b", Symbol "int"]], Body [List [List [Number 0, Symbol "return"], Token ";"]]])))

-- Test with show on CPT and AST
cptToShowSymbol :: Test
cptToShowSymbol = TestCase (assertEqual [] "Symbol \"a\"" (show (Symbol "a")))

cptToShowASymbol :: Test
cptToShowASymbol = TestCase (assertEqual [] "ASymbol \"a\"" (show (ASymbol "a")))

cptToNothing :: Test
cptToNothing = TestCase (assertEqual [] Nothing (getDefaultValue VVoid))

cptToASTCast :: Test
cptToASTCast = TestCase (assertEqual [] Nothing (cptToAST (List [Number 3, Token "::", (Symbol "nothing")])))

cptToASTVarNothing :: Test
cptToASTVarNothing = TestCase (assertEqual [] Nothing (cptToAST (List [Symbol "Foo", Symbol "Bar", Token "=", Body [Number 3]])))

cptToASTVarNothing2 :: Test
cptToASTVarNothing2 = TestCase (assertEqual [] Nothing (cptToAST (List [Symbol "Foo", Symbol "Bar", Token "=", Number 3])))

cptToASTGlobalVar :: Test
cptToASTGlobalVar = TestCase (assertEqual [] (Just (AGlobal "name" (VInt) (ANumber 0))) (cptToAST (Global [Symbol "name", Symbol "int", Token "=", Symbol "null"])))

cptToASTGlobalVar2 :: Test
cptToASTGlobalVar2 = TestCase (assertEqual [] (Just (AGlobal "x" (VInt) (Cond {icond = Call {function = AToken ">", args = [ANumber 3,ANumber 0]}, ibody = ANumber 1, repeatUntilFalse = False, ifFalse = Just (ANumber 0)}))) (cptToAST (List [Global [Symbol "x",Symbol "int",Token "=",List [List [Number 3,Token ">",Number 0],Token "?",List [Number 1,Token ":",Number 0]]],Token ";"])))

cptToASTGlobalVarNothing :: Test
cptToASTGlobalVarNothing = TestCase (assertEqual [] Nothing (cptToAST (Global [Symbol "name", Symbol "Bar", Token "=", Number 1])))

cptToASTParamListNothing :: Test
cptToASTParamListNothing = TestCase (assertEqual [] Nothing (cptToAST (List [List [Number 1, Token "::", Symbol "Foo"], Symbol "Foo"])))

-- Test for getVType

getVTypeInt :: Test
getVTypeInt = TestCase (assertEqual [] (Just VInt) (getVType "int"))

getVTypeFloat :: Test
getVTypeFloat = TestCase (assertEqual [] (Just VFloat) (getVType "float"))

getVTypeString :: Test
getVTypeString = TestCase (assertEqual [] (Just VString) (getVType "string"))

getVTypeBool :: Test
getVTypeBool = TestCase (assertEqual [] (Just VBool) (getVType "bool"))

getVTypeVoid :: Test
getVTypeVoid = TestCase (assertEqual [] (Just VVoid) (getVType "void"))

getVTypeList :: Test
getVTypeList = TestCase (assertEqual [] (Just (VList VInt)) (getVType "list_int"))

getVTypeOther :: Test
getVTypeOther = TestCase (assertEqual [] Nothing (getVType "other"))

getVTypeStruct :: Test
getVTypeStruct = TestCase (assertEqual [] (Just (VStruct "name")) (getVType "struct_name"))

testsASTS :: Test.HUnit.Test
testsASTS = TestList [
        -- cptToAST
        cptToASTSymbol,
        cptToASTSymbolType,
        cptToASTNumber,
        cptToASTFloat,
        cptToASTArray,
        cptToASTOperationBasic,
        cptToASTOperationComplex,
        cptToASTTrue,
        cptToASTFalse,
        cptToASTStringLiteral,
        cptToASTCreateVariableWithoutDef,
        cptToASTCreateVariableWithDef,
        cptToASTReDefineVariable,
        cptToASTCallFunctionOne,
        cptToASTCallFunctionTwo,
        cptToASTCreateFunction,
        cptToShowSymbol,
        cptToShowASymbol,
        cptToNothing,
        cptToASTCast,
        cptToASTVarNothing,
        cptToASTVarNothing2,
        cptToASTGlobalVarNothing,
        cptToASTGlobalVar,
        cptToASTGlobalVar2,
        cptToASTParamListNothing,

        -- getVType
        getVTypeInt,
        getVTypeFloat,
        getVTypeString,
        getVTypeVoid,
        getVTypeBool,
        getVTypeList,
        getVTypeOther,
        getVTypeStruct
    ]