--
-- EPITECH PROJECT, 2022
-- glados
-- File description:
-- ParserTest
--

module ParserTest (
    testsParser
) where


import Test.HUnit

import JigotParser
import JigotAST

getWhitespaceTest :: Test
getWhitespaceTest = TestCase (assertEqual [] (" \t\v\f\r\n") (getWhitespace))

getValidLettersTest :: Test
getValidLettersTest = TestCase (assertEqual [] ((['a'..'z']++['A'..'Z'])++['_']) (getValidLetters))

getTokensDListTest :: Test
getTokensDListTest = TestCase (assertEqual [] ["<", ">", "<=", ">=", "==", "!=", "&&", "||", "+", "-", "++", "*", "/", "%", "**"] (getTokensDList))

getTokensAssign :: Test
getTokensAssign = TestCase (assertEqual [] ["+=", "-=", "*=", "**=", "/=", "%=", "=", "++="] (getTokensWithAssign))

getTokensListTest :: Test
getTokensListTest = TestCase (assertEqual [] ['=', ':', '+', '-', '*', '/', '%', '!', '#', '?', '=', '>', '<', '&', '|'] (getTokensList))

-- Parse Pred
parsePredOne :: Test
parsePredOne = TestCase (assertEqual [] (Left "Error") (runParser (parsePred (=='a')) []))

parsePredTwo :: Test
parsePredTwo = TestCase (assertEqual [] (Left "Error") (runParser (parsePred (=='a')) "b"))

parsePredThree :: Test
parsePredThree = TestCase (assertEqual [] (Right ('a', "b")) (runParser (parsePred (=='a')) "ab"))

-- Parse Char
parseCharOne :: Test
parseCharOne = TestCase (assertEqual [] (Left "Error") (runParser (parseChar 'a') "b"))

parseCharTwo :: Test
parseCharTwo = TestCase (assertEqual [] (Left "Error") (runParser (parseChar 'a') []))

parseCharThree :: Test
parseCharThree = TestCase (assertEqual [] (Right ('a', "aa")) (runParser (parseChar 'a') "aaa"))
-- Parse Any Char
parseAnyCharOne :: Test
parseAnyCharOne = TestCase (assertEqual [] (Left "Error") (runParser (parseAnyChar "ac") "b"))

parseAnyCharTwo :: Test
parseAnyCharTwo = TestCase (assertEqual [] (Left "Error") (runParser (parseAnyChar "ac") []))

parseAnyCharThree :: Test
parseAnyCharThree = TestCase (assertEqual [] (Right ('a', "aa")) (runParser (parseAnyChar "ac") "aaa"))

parseAnyCharFour :: Test
parseAnyCharFour = TestCase (assertEqual [] (Right ('c', "aa")) (runParser (parseAnyChar "ca") "caa"))

-- Parse Any Char Except

parseAnyCharExceptOne :: Test
parseAnyCharExceptOne = TestCase (assertEqual [] (Right ('a', "aa")) (runParser (parseAnyCharExcept "bc") "aaa"))

parseAnyCharExceptTwo :: Test
parseAnyCharExceptTwo = TestCase (assertEqual [] (Left "Error") (runParser (parseAnyCharExcept "a") "aaa"))

-- Parse Many

parseManyOne :: Test
parseManyOne = TestCase (assertEqual [] (Right ("aa", "")) (runParser (parseMany (parseChar 'a')) "aa"))

parseManyTwo :: Test
parseManyTwo = TestCase (assertEqual [] (Right ([], [])) (runParser (parseMany (parseChar 'a')) []))

parseManyThree :: Test
parseManyThree = TestCase (assertEqual [] (Right ([], "ba")) (runParser (parseMany (parseChar 'a')) "ba"))

-- Parse Some

parseSomeOne :: Test
parseSomeOne = TestCase (assertEqual [] (Right ("aa", "")) (runParser (parseSome (parseChar 'a')) "aa"))

parseSomeTwo :: Test
parseSomeTwo = TestCase (assertEqual [] (Left "Error") (runParser (parseSome (parseChar 'a')) []))

parseSomeThree :: Test
parseSomeThree = TestCase (assertEqual [] (Left "Error") (runParser (parseSome (parseChar 'a')) "ba"))

-- Parse UInt

parseUIntOne :: Test
parseUIntOne = TestCase (assertEqual [] (Right (0, "")) (runParser parseUInt "0"))

parseUIntTwo :: Test
parseUIntTwo = TestCase (assertEqual [] (Right (0, "a")) (runParser parseUInt "0a"))

parseUIntThree :: Test
parseUIntThree = TestCase (assertEqual [] (Left "Error") (runParser parseUInt "av"))

parseUIntFour :: Test
parseUIntFour = TestCase (assertEqual [] (Left "Error") (runParser parseUInt "-22"))

-- Parse Int

parseIntOne :: Test
parseIntOne = TestCase (assertEqual [] (Right (0, "")) (runParser parseInt "0"))

parseIntTwo :: Test
parseIntTwo = TestCase (assertEqual [] (Right (0, "a")) (runParser parseInt "0a"))

parseIntThree :: Test
parseIntThree = TestCase (assertEqual [] (Left "Error") (runParser parseInt "av"))

parseIntFour :: Test
parseIntFour = TestCase (assertEqual [] (Right (-22, "")) (runParser parseInt "-22"))

-- Parse Float

parseFloatOne :: Test
parseFloatOne = TestCase (assertEqual [] (Right (0.0, "")) (runParser parseFloat "0.0"))

parseFloatTwo :: Test
parseFloatTwo = TestCase (assertEqual [] (Right (0.0, "a")) (runParser parseFloat "0.0a"))

parseFloatThree :: Test
parseFloatThree = TestCase (assertEqual [] (Left "Error") (runParser parseFloat "av"))

-- Parse Number

parseNumberOne :: Test
parseNumberOne = TestCase (assertEqual [] (Right (Number 0, "")) (runParser parseNumber "0"))

parseNumberTwo :: Test
parseNumberTwo = TestCase (assertEqual [] (Right (Number 0, "a")) (runParser parseNumber "0a"))

parseNumberThree :: Test
parseNumberThree = TestCase (assertEqual [] (Left "Error") (runParser parseNumber "av"))

parseNumberFour :: Test
parseNumberFour = TestCase (assertEqual [] (Right (Number (-22), "")) (runParser parseNumber "-22"))

parseNumberFive :: Test
parseNumberFive = TestCase (assertEqual [] (Right (CFloat 0.2, "")) (runParser parseNumber "0.2"))

-- Parse String

parseStringOne :: Test
parseStringOne = TestCase (assertEqual [] (Right ("", "")) (runParser (parseString "abc") "abc"))

parseStringTwo :: Test
parseStringTwo = TestCase (assertEqual [] (Left "Error") (runParser (parseString "4") "a"))

-- Parse Literal

parseStringLiteralOne :: Test
parseStringLiteralOne = TestCase (assertEqual [] (Right (StringLiteral "abc", "")) (runParser parseStringLiteral "\"abc\""))

parseStringLiteralTwo :: Test
parseStringLiteralTwo = TestCase (assertEqual [] (Left "Error") (runParser parseStringLiteral "abc"))

-- Parse Symbol

parseSymbolOne :: Test
parseSymbolOne = TestCase (assertEqual [] (Right (Symbol "a", "")) (runParser parseSymbol "a"))

parseSymbolTwo :: Test
parseSymbolTwo = TestCase (assertEqual [] (Right (Symbol "aa", "")) (runParser parseSymbol "aa"))

parseSymbolThree :: Test
parseSymbolThree = TestCase (assertEqual [] (Left "Error") (runParser parseSymbol "12"))

-- Parse Whitespace

parseWhitespaceOne :: Test
parseWhitespaceOne = TestCase (assertEqual [] (Right ('l', "ol")) (runParser (parseWhitespace (parseChar 'l'))  "   lol"))

parseWhitespaceTwo :: Test
parseWhitespaceTwo = TestCase (assertEqual [] (Left "Error") (runParser (parseWhitespace (parseChar 'l')) []))

parseWhitespaceThree :: Test
parseWhitespaceThree = TestCase (assertEqual [] (Left "Error") (runParser (parseWhitespace (parseChar 'l')) "m"))

-- Parse Separator (,)
parseSeparatorOne :: Test
parseSeparatorOne = TestCase (assertEqual [] (Right ('a', "")) (runParser (parseSeparator  (parseChar 'a')) ",a"))

parseSeparatorTwo :: Test
parseSeparatorTwo = TestCase (assertEqual [] (Left "Error") (runParser (parseSeparator  (parseChar 'a')) "a"))

-- Parse Many Function

parseManyFctOne :: Test
parseManyFctOne = TestCase (assertEqual [] (Right ("aa", "")) (runParser (parseManyFct parseWhitespace (parseChar 'a')) "         aa"))

parseManyFctTwo :: Test
parseManyFctTwo = TestCase (assertEqual [] (Right ([], [])) (runParser (parseManyFct parseWhitespace (parseChar 'a')) []))

parseManyFctThree :: Test
parseManyFctThree = TestCase (assertEqual [] (Right ([], "b")) (runParser (parseManyFct parseWhitespace (parseChar 'a')) "b"))

-- Parse List

parseListOne :: Test
parseListOne = TestCase (assertEqual [] (Right (List [Symbol "if", List [ Symbol "a" ], Symbol "x", Symbol "b"], "")) (runParser (parseList) "(if (a) x b)"))

parseListTwo :: Test
parseListTwo = TestCase (assertEqual [] (Left "Error") (runParser (parseList) "(if (a) x b"))

parseListThree :: Test
parseListThree = TestCase (assertEqual [] (Left "Error") (runParser (parseList) "a"))

-- Parse Array

parseArrayOne :: Test
parseArrayOne = TestCase (assertEqual [] (Right (Array [Number 1, Number 2, Number 3], "")) (runParser parseArray "[1,2,3]"))

parseArrayTwo :: Test
parseArrayTwo = TestCase (assertEqual [] (Left "Error") (runParser parseArray "[1,2,3"))

parseArrayThree :: Test
parseArrayThree = TestCase (assertEqual [] (Left "Error") (runParser parseArray "a"))

parseArrayFour :: Test
parseArrayFour = TestCase (assertEqual [] (Right (Array [], "")) (runParser parseArray "[]"))

-- Parse Key Variable for Parameters

parseKeyVariableOne :: Test
parseKeyVariableOne = TestCase (assertEqual [] (Right (List [Symbol "lol", Symbol "string"], "")) (runParser parseKeyVariable "lol string"))

parseKeyVariableTwo :: Test
parseKeyVariableTwo = TestCase (assertEqual [] (Left "Error") (runParser parseKeyVariable "lol"))

parseKeyVariableThree :: Test
parseKeyVariableThree = TestCase (assertEqual [] (Left "Error") (runParser parseKeyVariable "lol string string"))

-- Parse Parameters

parseParametersOne :: Test
parseParametersOne = TestCase (assertEqual [] (Right (Parameters [List [Symbol "lol", Symbol "string"]], "")) (runParser parseParameters "(lol string)"))

parseParametersTwo :: Test
parseParametersTwo = TestCase (assertEqual [] (Left "Error") (runParser parseParameters "(lol string"))

parseParametersThree :: Test
parseParametersThree = TestCase (assertEqual [] (Left "Error") (runParser parseParameters "lol string"))

parseParametersFour :: Test
parseParametersFour = TestCase (assertEqual [] (Right (Parameters [], "")) (runParser parseParameters "(void)"))

parseParametersFive :: Test
parseParametersFive = TestCase (assertEqual [] (Right (Parameters [], "")) (runParser parseParameters "()"))

parseParametersSix :: Test
parseParametersSix = TestCase (assertEqual [] (Right (Parameters [List [Symbol "lol", Symbol "string"], List [Symbol "lol", Symbol "string"]], "")) (runParser parseParameters "(lol string, lol string)"))

-- Parse Function

parseFunctionOne :: Test
parseFunctionOne = TestCase (assertEqual [] (Right (List [Symbol "_main", Symbol "int", Parameters [], Body [List [List [Number 0, Symbol "return"], Token ";"]]], "")) (runParser parseFunction "_main int () { 0 return; }"))

parseFunctionTwo :: Test
parseFunctionTwo = TestCase (assertEqual [] (Left "Error") (runParser parseFunction "_main int () { 0 return; "))

-- Parse Variable Definition

parseVariableDefOne :: Test
parseVariableDefOne = TestCase (assertEqual [] (Right (List [Symbol "a", Symbol "int", Token "=", Number 42], "")) (runParser parseVariableDef "a int = 42"))

parseVariableDefTwoT :: Test
parseVariableDefTwoT = TestCase (assertEqual [] (Left "Error") (runParser parseVariableDef "a int = ;"))

parseVariableDefThree :: Test
parseVariableDefThree = TestCase (assertEqual [] (Right (List [Symbol "a", Symbol "int", Token "=", List [Number 42, Symbol "lol"]], ";")) (runParser parseVariableDef "a int = 42 lol;"))

parseVariableDefFour :: Test
parseVariableDefFour = TestCase (assertEqual [] (Right (List [Symbol "a", Symbol "int", Token "=", List [Number 42, Symbol "lol"]], "")) (runParser parseVariableDef "a int = 42 lol"))

-- Parse Binary Operation

parseAssignementOne :: Test
parseAssignementOne = TestCase (assertEqual [] (Right (List [Symbol "a", Token "=", Number 0], ";")) (runParser parseAssignement "a = 0;"))

parseAssignementTwo :: Test
parseAssignementTwo = TestCase (assertEqual [] (Left "Error") (runParser parseAssignement "a = ;0"))

parseAssignementThree :: Test
parseAssignementThree = TestCase (assertEqual [] (Right (List [Symbol "a", Token "=", List [Number 0, Token "+", Number 1]], ";")) (runParser parseAssignement "a = 0 + 1;"))

parseAssignementFour :: Test
parseAssignementFour = TestCase (assertEqual [] (Right (List [Symbol "a", Token "=", List [Number 0, Token "+", List [Number 1, Token "*", Number 2]]], ";")) (runParser parseAssignement "a = 0 + 1 * 2;"))

parseAssignementFive :: Test
parseAssignementFive = TestCase (assertEqual [] (Right (List [Symbol "a", Token "=", List [List [Number 0, Token "+", Number 1], Symbol "lol"]], ";")) (runParser parseAssignement "a = (0 + 1) lol;"))

-- Structure body parsing (Need to add a specific case because it's like <symbol> <type>;)

parseBodyStructOne :: Test
parseBodyStructOne = TestCase (assertEqual [] (Right (Body [List [Symbol "a", Token "=", Number 0], List [Symbol "b", Token "=", Number 1]], "")) (runParser parseBodyStructAssign "{ a = 0, b = 1 }"))

parseBodyStructTwo :: Test
parseBodyStructTwo = TestCase (assertEqual [] (Left "Error") (runParser parseBodyStructAssign "{ a int; b int; }"))

parseBodyStructThree :: Test
parseBodyStructThree = TestCase (assertEqual [] (Left "Error") (runParser parseBodyStructDeclaration "{ a = 0 }"))

parseBodyStructFour :: Test
parseBodyStructFour = TestCase (assertEqual [] (Right (Body [List [List [Symbol "a",Symbol "int"],Token ";"],List [List [Symbol "b",Symbol "int"],Token ";"]],"")) (runParser parseBodyStructDeclaration "{ a int; b int; }"))

parseBodyStructFive :: Test
parseBodyStructFive = TestCase (assertEqual [] (Right (Body [], "")) (runParser parseBodyStructDeclaration "{}"))

-- Parse Structure declaration

parseStructDefOne :: Test
parseStructDefOne = TestCase (assertEqual [] (Right (List [Symbol "name", Symbol "struct", Body [List [List [Symbol "a", Symbol "int"], Token ";"], List [List [Symbol "b", Symbol "int"], Token ";"]]], "")) (runParser parseStructDef "name struct { a int; b int; }"))

parseStructDefTwo :: Test
parseStructDefTwo = TestCase (assertEqual [] (Left "Error") (runParser parseStructDef "name struct { a int; b int }"))

-- Parse Structure assignement

parseStructDefAssigOne :: Test
parseStructDefAssigOne = TestCase (assertEqual [] (Right (List [Symbol "ex", Symbol "struct_name", Token "=", Body [List [Symbol "a", Token "=", Number 42]]], ";")) (runParser parseStructDefAssign "ex struct_name = { a = 42 };"))

parseStructDefAssignTwo :: Test
parseStructDefAssignTwo = TestCase (assertEqual [] (Left "Error") (runParser parseStructDefAssign "ex struct_name = { a = 42"))

-- Parse Scope

parseScopeOne :: Test
parseScopeOne = TestCase (assertEqual [] (Right (ScopeElement [List [List [Symbol "x", Symbol "int", Token "=", Number 42], Token ";"]], "")) (runParser parseScope "{ x int = 42; }"))

parseScopeTwo :: Test
parseScopeTwo = TestCase (assertEqual [] (Left "Error") (runParser parseScope "{ x int = 42;"))

-- Parse Expression

parseLineExprOne :: Test
parseLineExprOne = TestCase (assertEqual [] (Right (List [List [Symbol "a", Token "=", Number 0], Token ";"], "")) (runParser parseLineExpr "a = 0;"))

parseLineExprTwo :: Test
parseLineExprTwo = TestCase (assertEqual [] (Right (List [List [Symbol "a", Token "=", List [Number 0, Token "+", Number 1]], Token ";"], "")) (runParser parseLineExpr "a = 0 + 1;"))

parseLineExprThree :: Test
parseLineExprThree = TestCase (assertEqual [] (Right (List [List [List [Symbol "lolita"], Symbol "lol"], Token ";"], "")) (runParser parseLineExpr "(lolita) lol;"))

-- Parse Multiple Expression

parseMultipleExpression :: Test
parseMultipleExpression = TestCase (assertEqual [] (Right (List [List [Number 1,Token "+",List [Number 2,Token "*",Number 4]],Token "*",List [List [Number 5,Token "+",List [Number 8,Token "*",Number 9]],Token "+",Number 43]],"")) (runParser parseExprMultiple "1 + 2 * 4 * 5 + 8 * 9 + 43"))

parseMultipleExpressionTwo :: Test
parseMultipleExpressionTwo = TestCase (assertEqual [] (Right (List [Number 43, Token "==", List [Number 8, Token "*", Number 9]],"")) (runParser parseExprMultiple "43 == 8 * 9"))

parseMultipleExpressionThree :: Test
parseMultipleExpressionThree = TestCase (assertEqual [] (Right (List [Number 56, Token "==", List [Number 8, Token ">", Number 9]], "")) (runParser parseExprMultiple "56 == 8 > 9"))

-- Parse Global Expression

parseGlobalExprOne :: Test
parseGlobalExprOne = TestCase (assertEqual [] (Right (List [Global [Symbol "a", Symbol "int", Token "=", Number 42], Token ";"], "")) (runParser parseLineExprGlobal "a int = 42;"))

parseGlobalExprTwo :: Test
parseGlobalExprTwo = TestCase (assertEqual [] (Left "Error") (runParser parseLineExprGlobal "\"jigot\" import;"))

parseGlobalExprThree :: Test
parseGlobalExprThree = TestCase (assertEqual [] (Left "Error") (runParser parseLineExprGlobal "lol struct { a int; b int; };"))

-- Parse Binary Operation ++ & --

parseBinaryOperationPlusPlusOne :: Test
parseBinaryOperationPlusPlusOne = TestCase (assertEqual [] (Right (List [Token "++", Symbol "a"], ";")) (runParser parseBinaryOperationPlusPlus "++a;"))

parseBinaryOperationPlusPlusTwo :: Test
parseBinaryOperationPlusPlusTwo = TestCase (assertEqual [] (Right (List [Token "++", Symbol "a"], "")) (runParser parseBinaryOperationPlusPlus "a++"))

parseBinaryOperationPlusPlusThree :: Test
parseBinaryOperationPlusPlusThree = TestCase (assertEqual [] (Left "Error") (runParser parseBinaryOperationPlusPlus "a+"))

parseBinaryOperationMinusMinusOne :: Test
parseBinaryOperationMinusMinusOne = TestCase (assertEqual [] (Right (List [Token "--", Symbol "a"], ";")) (runParser parseBinaryOperationMinusMinus "--a;"))

parseBinaryOperationMinusMinusTwo :: Test
parseBinaryOperationMinusMinusTwo = TestCase (assertEqual [] (Right (List [Token "--", Symbol "a"], "")) (runParser parseBinaryOperationMinusMinus "a--"))

parseBinaryOperationMinusMinusThree :: Test
parseBinaryOperationMinusMinusThree = TestCase (assertEqual [] (Left "Error") (runParser parseBinaryOperationPlusPlus "a-"))

-- Parse Conditional Statement

-- if
parseCondOneT :: Test
parseCondOneT = TestCase (assertEqual [] (Right (List [List [Symbol "a", Token "==", Number 0], Symbol "if", Body []], "")) (runParser parseCondOne "(a == 0) if {}"))

-- else
parseCondTwoT :: Test
parseCondTwoT = TestCase (assertEqual [] (Right (List [List [ List [Symbol "a", Token "==", Number 0], Symbol "if", Body []], Symbol "else", Body []], "")) (runParser parseCondTwo "(a == 0) if {} else {}"))

-- else if else if else
parseCondThreeT :: Test
parseCondThreeT = TestCase (assertEqual [] (Right (List [List [List [Symbol "a", Token "==", Number 0],Symbol "if",Body []],Symbol "else",List [List [List [Symbol "b", Token "==", Number 1],Symbol "if",Body []],Symbol "else",Body []]], "")) (runParser parseCondTwo "(a == 0) if {} else (b == 1) if {} else {}"))

--unexpected token
parseCondFourT :: Test
parseCondFourT = TestCase (assertEqual [] (Left "Error") (runParser parseCondTwo "(a == 0) unless {}"))

--while
parseCondFiveT :: Test
parseCondFiveT = TestCase (assertEqual [] (Right (List [List [Symbol "a", Token "==", Number 0], Symbol "while", Body []], "")) (runParser parseCondOne "(a == 0) while {}"))

-- Parse For loop

parseForLoopOne :: Test
parseForLoopOne = TestCase (assertEqual [] (Right (List [List [List [List [Token "++",Symbol "i"],Token ";",List [Symbol "i",Token "<",Number 10]],Symbol "for",List [Symbol "i",Symbol "int",Token "=",Number 0],Body []]], "")) (runParser parseFor "(++i; i < 10) for (i int = 0) {}"))

parseForLoopTwo :: Test
parseForLoopTwo = TestCase (assertEqual [] (Right (List [List [List [List [Token "--",Symbol "i"],Token ";",List [Symbol "i",Token "<",Number 10]],Symbol "for",List [Symbol "i",Symbol "int",Token "=",Number 0],Body []]], "")) (runParser parseFor "(--i; i < 10) for (i int = 0) {}"))

parseForLoopThree :: Test
parseForLoopThree = TestCase (assertEqual [] (Right (List [List [List [List [Token "++",Symbol "i"],Token ";",List [Symbol "i",Token "<",Number 10]],Symbol "for",List [Symbol "i",Symbol "int",Token "=",Number 0],Body []]],"")) (runParser parseFor "(i++; i < 10) for (i int = 0) {}"))

parseForLoopFour :: Test
parseForLoopFour = TestCase (assertEqual [] (Right (List [List [List [List [],Token ";",List [Symbol "i",Token "<",Number 10]],Symbol "for",List [Symbol "i",Symbol "int",Token "=",Number 0],Body []]], "")) (runParser parseFor "(; i < 10) for (i int = 0) {}"))

parseForLoopFive :: Test
parseForLoopFive = TestCase (assertEqual [] (Right (List [List [List [List [],Token ";",List [Symbol "i",Token "<",Number 10]],Symbol "for",List [],Body []]], "")) (runParser parseFor "(; i < 10) for () {}"))

-- Parse For Each

parseForEachOne :: Test
parseForEachOne = TestCase (assertEqual [] (Right (List [List [List [Symbol "item",Symbol "name",Token ":",Symbol "items"],Symbol "for",Body []]], "")) (runParser parseForEach "(item name: items) for {}"))

parseForEachTwo :: Test
parseForEachTwo = TestCase (assertEqual [] (Left "Error") (runParser parseForEach "(i++; i < 10) for (i int = 0) {}"))

-- Parse Special

parseSpecialOne :: Test
parseSpecialOne = TestCase (assertEqual [] (Right (List [Symbol "a",Token "@",Symbol "b"], "")) (runParser parseSpecial "a[b]"))

parseSpecialTwo :: Test
parseSpecialTwo = TestCase (assertEqual [] (Left "Error") (runParser parseSpecial "1[b]"))

parseSpecialThree :: Test
parseSpecialThree = TestCase (assertEqual [] (Right (List [Symbol "a",Token "@",Number 1], "")) (runParser parseSpecial "a[1]"))

-- Parse Return (Probably need to be more generalized ?)

parseReturnDefOne :: Test
parseReturnDefOne = TestCase (assertEqual [] (Right (List [List [Number 42, Symbol "lol"], Symbol "return"], "")) (runParser parseReturnDef "42 lol return"))

parseReturnDefTwo :: Test
parseReturnDefTwo = TestCase (assertEqual [] (Left "Error") (runParser parseReturnDef "42 return;"))

parseReturnDefThree :: Test
parseReturnDefThree = TestCase (assertEqual [] (Right (List [List [Number 4, Symbol "lol"], Symbol "print"], "")) (runParser parseReturnDef "4 lol print"))

-- Parse Import

parseImportOne :: Test
parseImportOne = TestCase (assertEqual [] (Right (Global [StringLiteral "jigot",Symbol "import"],"")) (runParser parseImport "\"jigot\" import;"))

parseImportTwo :: Test
parseImportTwo = TestCase (assertEqual [] (Left "Error") (runParser parseImport "\"jigot\" import"))

parseImportThree :: Test
parseImportThree = TestCase (assertEqual [] (Right (Global [Symbol "lolito",Symbol "int",Parameters [],Body [List [List [Number 1,Symbol "return"],Token ";"]]],"")) (runParser parseImport "\"./examples/lolito.jg\" import;"))

parseImportFour :: Test
parseImportFour = TestCase (assertEqual [] (Left "Invalid content") (runParser parseImport "\"./test/files/errors/syntaxError.jg\" import;"))

parseImportFive :: Test
parseImportFive = TestCase (assertEqual [] (Left "Invalid content") (runParser parseImport "\"./test/files/errors/syntaxError2.jg\" import;"))

-- Parse Access like (ex->a)

parseAccessOne :: Test
parseAccessOne = TestCase (assertEqual [] (Right (List [Symbol "a", Token "->", Symbol "b"], "")) (runParser parseAccess "a->b"))

parseAccessTwo :: Test
parseAccessTwo = TestCase (assertEqual [] (Left "Error") (runParser parseAccess "a.b"))

-- Parse Token

parseTokenOne :: Test
parseTokenOne = TestCase (assertEqual [] (Right (Token "+", "")) (runParser parseToken "+"))

parseTokenTwo :: Test
parseTokenTwo = TestCase (assertEqual [] (Right (Token "++", "")) (runParser parseToken "++"))

parseTokenThree :: Test
parseTokenThree = TestCase (assertEqual [] (Left "Error") (runParser parseToken "lol"))

-- Parse Comment

parseCommentOne :: Test
parseCommentOne = TestCase (assertEqual [] "" (parseComment "//lol"))

parseCommentTwo :: Test
parseCommentTwo = TestCase (assertEqual [] "" (parseComment "/*lol*/"))

parseCommentThree :: Test
parseCommentThree = TestCase (assertEqual [] "/*lol" (parseComment "/*lol"))

parseCommentFour :: Test
parseCommentFour = TestCase (assertEqual [] "" (parseComment "#lol"))

parseCommentFive :: Test
parseCommentFive = TestCase (assertEqual [] "//" (parseComment "//"))

parseCommentSix :: Test
parseCommentSix = TestCase (assertEqual [] "k" (parseComment "//lol\nk"))

parseCommentSeven :: Test
parseCommentSeven = TestCase (assertEqual [] "/*" (parseComment "/*"))

-- Parse Other (Expression inside function)

parseOtherOne :: Test
parseOtherOne = TestCase (assertEqual [] (Right (List [List [List [Symbol "a", Token "==", Number 0], Symbol "if", Body []], Symbol "else", Body []], "")) (runParser parseOther "(a == 0) if {} else {}"))

parseOtherTwo :: Test
parseOtherTwo = TestCase (assertEqual [] (Right (List [List [List [List [Token "++",Symbol "idx"],Token ";",List [Symbol "idx",Token "<",List [List [Symbol "str",Token ".",Symbol "len",List []]]]],Symbol "for",List [],Body []]],"")) (runParser parseOther "(++idx; idx < str.len) for () {}"))

-- Parse Cpt

parseCptOne :: Test
parseCptOne = TestCase (assertEqual [] (Left "Error") (runParser parseCpt "0"))

-- Variable definition
parseCptTwo :: Test
parseCptTwo = TestCase (assertEqual [] (Right (List [Global [Symbol "x", Symbol "int", Token "=", Number 0], Token ";"], "")) (runParser parseCpt "x int = 0;"))

parseCptThree :: Test
parseCptThree = TestCase (assertEqual [] (Left "Error") (runParser parseCpt "(1 > 2) if { false return; } else { true return; }"))

-- Function
parseCptFour :: Test
parseCptFour = TestCase (assertEqual [] (Right (Global [Symbol "_main", Symbol "int", Parameters [], Body [List [List [Number 0, Symbol "return"], Token ";"]]], "")) (runParser parseCpt "_main int () { 0 return; }"))

-- Import (Error because can't not load the file here)
parseCptFive :: Test
parseCptFive = TestCase (assertEqual [] (Left "Error") (runParser parseCpt "\"./src/hello.jg\" import;"))

-- Structure
parseCptSix :: Test
parseCptSix = TestCase (assertEqual [] (Right (List [Symbol "name", Symbol "struct", Body [List [List [Symbol "x", Symbol "int"], Token ";"], List [List [Symbol "y", Symbol "int"], Token ";"], List [List [Symbol "s", Symbol "string"], Token ";"], List [List [Symbol "b", Symbol "bool"], Token ";"]]], "")) (runParser parseCpt "name struct { x int; y int; s string; b bool; }"))

-- Test with datas

parseContentOne :: Test
parseContentOne = TestCase (assertEqual [] (Right (List [List [Global [Symbol "x", Symbol "int", Token "=", Number 42], Token ";"], List [Global [Symbol "y", Symbol "int", Token "=", Number 43], Token ";"], List [Global [Symbol "z", Symbol "int", Token "=", Number 44], Token ";"]], "")) (runParser (parseMultiple 0) "x int = 42; y int = 43; z int = 44;"))

parseContentTwo :: Test
parseContentTwo = TestCase (assertEqual [] (Left "Error in function/variable at line: 1") (runParser (parseMultiple 0) "\"./src/hello.jg\" import; 42"))

parseContentThree :: Test
parseContentThree = TestCase (assertEqual [] (Right (List [Global [Symbol "_main", Symbol "int", Parameters [], Body [List [List [Number 0, Symbol "return"], Token ";"]]], List [Global [Symbol "x", Symbol "int", Token "=", Number 42], Token ";"], List [Global [Symbol "y", Symbol "int", Token "=", Number 43], Token ";"], List [Global [Symbol "z", Symbol "int", Token "=", Number 44], Token ";"]], "")) (runParser (parseMultiple 0) "_main int () { 0 return; } x int = 42; y int = 43; z int = 44;                                             "))

parseContentFour :: Test
parseContentFour = TestCase (assertEqual [] (Left "Error in function/variable at line: 1") (runParser (parseMultiple 0) "42"))


testsParser :: Test.HUnit.Test
testsParser = TestList [
        getWhitespaceTest,
        getValidLettersTest,
        getTokensDListTest,
        getTokensAssign,
        getTokensListTest,
        parsePredOne,
        parsePredTwo,
        parsePredThree,
        parseCharOne,
        parseCharTwo,
        parseCharThree,
        parseAnyCharOne,
        parseAnyCharTwo,
        parseAnyCharThree,
        parseAnyCharFour,
        parseAnyCharExceptOne,
        parseAnyCharExceptTwo,
        parseManyOne,
        parseManyTwo,
        parseManyThree,
        parseSomeOne,
        parseSomeTwo,
        parseSomeThree,
        parseUIntOne,
        parseUIntTwo,
        parseUIntThree,
        parseUIntFour,
        parseIntOne,
        parseIntTwo,
        parseIntThree,
        parseIntFour,
        parseFloatOne,
        parseFloatTwo,
        parseFloatThree,
        parseNumberOne,
        parseNumberTwo,
        parseNumberThree,
        parseNumberFour,
        parseNumberFive,
        parseStringOne,
        parseStringTwo,
        parseStringLiteralOne,
        parseStringLiteralTwo,
        parseSymbolOne,
        parseSymbolTwo,
        parseSymbolThree,
        parseWhitespaceOne,
        parseWhitespaceTwo,
        parseWhitespaceThree,
        parseSeparatorOne,
        parseSeparatorTwo,
        parseManyFctOne,
        parseManyFctTwo,
        parseManyFctThree,
        parseListOne,
        parseListTwo,
        parseListThree,
        parseArrayOne,
        parseArrayTwo,
        parseArrayThree,
        parseArrayFour,

        parseKeyVariableOne,
        parseKeyVariableTwo,
        parseKeyVariableThree,

        parseParametersOne,
        parseParametersTwo,
        parseParametersThree,
        parseParametersFour,
        parseParametersFive,
        parseParametersSix,

        parseFunctionOne,
        parseFunctionTwo,

        parseVariableDefOne,
        parseVariableDefTwoT,
        parseVariableDefThree,
        parseVariableDefFour,

        parseAssignementOne,
        parseAssignementTwo,
        parseAssignementThree,
        parseAssignementFour,
        parseAssignementFive,

        -- Structures
        parseBodyStructOne,
        parseBodyStructTwo,
        parseBodyStructThree,
        parseBodyStructFour,
        parseBodyStructFive,

        parseStructDefOne,
        parseStructDefTwo,

        parseStructDefAssigOne,
        parseStructDefAssignTwo,

        parseScopeOne,
        parseScopeTwo,

        parseLineExprOne,
        parseLineExprTwo,
        parseLineExprThree,

        parseMultipleExpression,
        parseMultipleExpressionTwo,
        parseMultipleExpressionThree,

        parseGlobalExprOne,
        parseGlobalExprTwo,
        parseGlobalExprThree,

        parseBinaryOperationPlusPlusOne,
        parseBinaryOperationPlusPlusTwo,
        parseBinaryOperationPlusPlusThree,

        parseBinaryOperationMinusMinusOne,
        parseBinaryOperationMinusMinusTwo,
        parseBinaryOperationMinusMinusThree,

        parseCondOneT,
        parseCondTwoT,
        parseCondThreeT,
        parseCondFourT,
        parseCondFiveT,

        parseForLoopOne,
        parseForLoopTwo,
        parseForLoopThree,
        parseForLoopFour,
        parseForLoopFive,

        parseForEachOne,
        parseForEachTwo,

        parseSpecialOne,
        parseSpecialTwo,
        parseSpecialThree,

        parseReturnDefOne,
        parseReturnDefTwo,
        parseReturnDefThree,

        parseAccessOne,
        parseAccessTwo,

        parseTokenOne,
        parseTokenTwo,
        parseTokenThree,

        parseCommentOne,
        parseCommentTwo,
        parseCommentThree,
        parseCommentFour,
        parseCommentFive,
        parseCommentSix,
        parseCommentSeven,

        parseImportOne,
        parseImportTwo,
        parseImportThree,
        parseImportFour,
        parseImportFive,

        parseOtherOne,
        parseOtherTwo,

        parseCptOne,
        parseCptTwo,
        parseCptThree,
        parseCptFour,
        parseCptFive,
        parseCptSix,

        parseContentOne,
        parseContentTwo,
        parseContentThree,
        parseContentFour
    ]