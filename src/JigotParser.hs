--
-- EPITECH PROJECT, 2022
-- glados
-- File description:
-- Parser
--

module JigotParser (
    parseCpt,
    getWhitespace,
    getValidLetters,
    getTokensDList,
    parseSeparator,
    parseKeyVariable,
    getTokensWithAssign,
    parseCallFunc,
    getTokensList,
    parsePred,
    parseChar,
    parseAssignement,
    parseAssignementEQ,
    parseAnyChar,
    parseAnyCharExcept,
    parseArray,
    parseMany,
    parseSome,
    parseUInt,
    parseInt,
    parseString,
    parseVariableDef,
    parseVariableDefStrict,
    parseParameters,
    parseStringLiteral,
    parseFloat,
    parseNumber,
    parseOther,
    parseList,
    parseSymbol,
    parseStructDef,
    parseStructDefAssign,
    parseWhitespace,
    parseManyFct,
    parseImport,
    parseFunction,
    parseScope,
    parseExpr,
    parseFor,
    parseForEach,
    parseSpecial,
    parseMultiple,
    parseBodyStructAssign,
    parseBodyStructDeclaration,
    parseReturnDef,
    parseAccess,
    parseToken,
    parseLineExpr,
    parseComment,
    dropMultiComment,
    parseCasting,
    parseLineExprGlobal,
    parseBinaryOperationPlusPlus,
    parseBinaryOperationMinusMinus,
    parseCondOne,
    parseCondTwo,
    parseCallBuiltIn,
    parseExprMultiple,
    Parser(runParser)
) where

import JigotAST
import Control.Applicative
import Data.Function
import System.Directory
import Data.Char
import System.IO.Unsafe (unsafePerformIO)

-- Transform into a EITHER Parser
newtype Parser a = Parser {
    runParser :: String -> Either String (a, String)
}

instance Functor Parser where
    fmap fct parser = Parser $ \ s -> case runParser parser s of
        Left _ -> Left "Error"
        Right (x, xs) -> Right (fct x, xs)

instance Applicative Parser where
    pure x = Parser $ \ s -> Right (x, s)
    (<*>) p p' = Parser $ \s -> case runParser p s of
                      Left x -> Left x
                      Right (f, s') -> case runParser p' s' of
                              Left x -> Left x
                              Right (x, xs) -> Right (f x, xs)
    (<*) p p' = const <$> p <*> p'
    (*>) p p' = flip const <$> p <*> p'

instance Alternative Parser where
    (<|>) p p' = Parser $ \ s -> case runParser p s of
                            Right x -> Right x
                            Left _ -> runParser p' s

instance Monad Parser where
    (>>=) p f = Parser $ \ s -> case runParser p s of
        Left x -> Left x
        Right (x, xs) -> runParser (f x) xs

getWhitespace :: String
getWhitespace = " \t\v\f\r\n"

getValidLetters :: String
getValidLetters = (['a'..'z']++['A'..'Z'])++['_']

getTokensDList :: [String]
getTokensDList = ["<", ">", "<=", ">=", "==", "!=", "&&", "||", "+", "-", "++", "*", "/", "%", "**"]

getTokensWithAssign :: [String]
getTokensWithAssign = ["+=", "-=", "*=", "**=", "/=", "%=", "=", "++="]

getTokensList :: String
getTokensList = ['=', ':', '+', '-', '*', '/', '%', '!', '#', '?', '=', '>', '<', '&', '|']

parsePred :: (Char -> Bool) -> Parser Char
parsePred f = Parser $ \ s -> case s of
    [] -> Left "Error"
    (x:xs) -> case f x of
        True -> Right (x, xs)
        False -> Left "Error"

parseChar :: Char -> Parser Char
parseChar x = parsePred(==x)

parseAnyChar :: String -> Parser Char
parseAnyChar x = parsePred(`elem` x)

parseAnyCharExcept :: String -> Parser Char
parseAnyCharExcept x = parsePred(`notElem` x)

parseString :: String -> Parser String
parseString [] = pure []
parseString (x:xs) = parseChar x *> parseString xs

parseMany :: Parser a -> Parser [a]
parseMany p = ((:) <$> p <*> parseMany p) <|> pure []

parseSome :: Parser a -> Parser [a]
parseSome p = (:) <$> p <*> parseMany p

parseUInt :: Parser Int
parseUInt = fmap (\x -> read x :: Int) (parseSome (parseAnyChar ['0'..'9']))

parseInt :: Parser Int
parseInt = ((negate <$ parseChar '-') <*> parseUInt) <|> parseUInt

parseFloat :: Parser Float
parseFloat =  parseSome (parseAnyChar ['0'..'9']) >>=
    \x -> parseChar '.' >> parseSome (parseAnyChar ['0'..'9']) >>=
    \y -> pure (read (x++"."++y) :: Float)

parseNumber :: Parser CPT
parseNumber = (CFloat <$> parseFloat) <|> Number <$> parseInt

parseSymbol :: Parser CPT
parseSymbol = Symbol <$> parseSome (parseAnyChar (getValidLetters))

parseStringLiteral :: Parser CPT
parseStringLiteral = StringLiteral <$> (parseChar '\"' *> parseMany (parseAnyChar ([' '..'!'] ++ ['#'..'~'])) <* parseChar '\"')

parseWhitespace :: Parser a -> Parser a
parseWhitespace p = parseMany (parseAnyChar (getWhitespace)) *> p

parseSeparator :: Parser a -> Parser a
parseSeparator p = parseChar ',' *> p

parseManyFct :: (Parser a -> Parser a) -> Parser a -> Parser [a]
parseManyFct f p = ((:) <$> f p <*> parseManyFct f p) <|> pure []

-- OUR TYPE (PACKTRAK + MONOID)

parseArray :: Parser CPT
parseArray = Array <$> (parseChar '[' *> parseWhitespace parseBasic >>=
                                        \x -> parseManyFct parseSeparator (parseWhitespace parseBasic) <* parseWhitespace (parseChar ']') >>=
                                        \d -> pure (x:d)) <|> (parseWhitespace (parseString "[]") >> pure (Array []))

parseBody :: Parser CPT
parseBody = Body <$> (parseChar '{' *> parseManyFct parseWhitespace parseOther <* parseWhitespace (parseChar '}'))

parseListWExpr :: Parser CPT
parseListWExpr = List <$> (parseChar '(' *> parseCasting [parseExprMultiple] <* parseWhitespace (parseChar ')') >>= \x -> case x of
    List x -> pure x
    _ -> pure [x])

parseList :: Parser CPT
parseList = List <$> (parseChar '(' *> parseManyFct parseWhitespace (parseListWExpr <|> parseList <|> parseBasic) <* parseWhitespace (parseChar ')'))

parseKeyVariable :: Parser CPT
parseKeyVariable = Parser $ \ s -> case runParser (parseManyFct parseWhitespace parseSymbol) s of
    Right ([Symbol x, Symbol y], xs) -> Right (List [Symbol x, Symbol y], xs)
    _ -> Left "Error"

parseParameters :: Parser CPT
parseParameters = Parameters <$> (parseChar '(' *> parseKeyVariable >>=
                                    \x -> parseManyFct parseSeparator parseKeyVariable <* parseWhitespace (parseChar ')') >>=
                                    \d -> pure (x:d)) <|> (parseWhitespace (parseString "()") >> pure (Parameters [])) <|>
                                    (parseWhitespace (parseString "(void)") >> pure (Parameters []))

-- Parse function is like (name int () { ... })
parseFunction :: Parser CPT
parseFunction = List <$> (parseWhitespace parseSymbol >>=
                    \x -> parseWhitespace parseSymbol >>=
                    \y -> parseWhitespace parseParameters >>=
                    \z -> parseWhitespace parseBody >>= 
                    \b -> pure [x, y, z, b])

parseFunctionGlobal :: Parser CPT
parseFunctionGlobal = Global <$> (parseWhitespace parseSymbol >>=
                    \x -> parseWhitespace parseSymbol >>=
                    \y -> parseWhitespace parseParameters >>=
                    \z -> parseWhitespace parseBody >>= 
                    \b -> pure [x, y, z, b])

parseLineExprGlobal :: Parser CPT
parseLineExprGlobal = List <$> (parseVariableDef >>=
                        \(List x) -> parseWhitespace (parseChar ';') >> pure [Global x, Token ";"])

parseSingleKeyword :: Parser CPT
parseSingleKeyword = List <$> (parseWhitespace (parseString "return") >> pure [Symbol "return"])

parseLineExpr :: Parser CPT
parseLineExpr = List <$> ((
                        parseVariableDef <|>
                        parseAssignement <|>
                        parseSingleKeyword <|>
                        parseBinaryOperationPlusPlus <|>
                        parseBinaryOperationMinusMinus <|>
                        parseReturnDef <|>
                        parseStructDefAssign <|>
                        parseCallFunc <|>
                        parseCallBuiltIn) >>=
                \x -> parseWhitespace (parseChar ';') >> pure [x, Token ";"])

-- Parse variable def is like (a int = 42;)
-- a int = 42;
-- a int = 50 * 45 + 78;
-- a int = ex->v;
-- a int = () lol;
-- a int = lol.len;
-- a int = lol.len + 54;
-- a int = ex->v + 54;
-- a int = () lol + 1;
-- a int = 50.0 :: int;
-- a int = 50.0;
-- a int = 50.0 + 1;
parseVariableDef :: Parser CPT
parseVariableDef = List <$> (parseWhitespace parseSymbol >>=
                    \x -> parseWhitespace parseSymbol >>=
                    \y -> (parseWhitespace (parseChar '=') *> parseWhitespace (parseCasting [parseExprMultiple])) >>=
                    \z -> pure [x, y, Token "=", z])

-- Parse assignement is like (a = 42;)
-- a int;
-- b string;
-- c list_int;
parseVariableDefStrict :: Parser CPT
parseVariableDefStrict = List <$> (parseWhitespace parseSymbol >>=
                    \x -> parseWhitespace parseSymbol >>=
                    \y -> parseWhitespace (parseChar ';') >> pure [List [x, y], Token ";"])

-- Parse assignement is like (a = 42) or (ex->a = 42)
-- a = 42;
-- ex->v = ex->c;
-- ex->b = 40;
-- a = 50 * 45 + 78;
-- a = () lol;
-- a = lol.len;
-- a[0] = 42;
parseAssignement :: Parser CPT
parseAssignement = List <$> (parseWhitespace (parseSpecial <|> parseAccess <|> parseSymbol) >>=
                    \x -> parseWhitespace parseTokenWithAssign >>=
                    \t -> parseWhitespace (parseCasting [parseExprMultiple]) >>=
                    \z -> pure [x, t, z])

-- For structure
-- when in { ...}
-- { a = 0, b = 0, c = 0 }
-- TODO: Faire la gestion de savoir lors de l'attribution si la var est bien dans la struct
parseAssignementEQ :: Parser CPT
parseAssignementEQ = List <$> (parseWhitespace parseSymbol >>=
                    \x -> parseWhitespace (parseChar '=') >>=
                    \t -> parseWhitespace parseExprMultiple >>=
                    \z -> pure [x, Token "=", z])

-- Parse special body when assigning (ex struct_name = { ... })
-- { a = 0, b = 0, c = 0 }
-- Only = is allowed
parseBodyStructAssign :: Parser CPT
parseBodyStructAssign = Body <$> (parseChar '{' *> parseWhitespace parseAssignementEQ >>=
                                \x -> parseManyFct parseSeparator (parseWhitespace parseAssignementEQ) >>=
                                \d -> parseWhitespace (parseChar '}') >> pure (x:d)) <|> (parseWhitespace (parseString "{}") >> pure (Body []))

parseBodyStructDeclaration :: Parser CPT
parseBodyStructDeclaration = Body <$> (parseWhitespace (parseChar '{') *> parseManyFct parseWhitespace parseVariableDefStrict <* parseWhitespace (parseChar '}'))

-- Parse Definition of a structure (ex: name struct { ... })
-- name struct { a int; b string; c bool; }
-- Only strict definition is allowed
parseStructDef :: Parser CPT
parseStructDef = List <$> (parseWhitespace parseSymbol >>=
                    \x -> parseWhitespace (parseString "struct") >> parseWhitespace parseBodyStructDeclaration >>=
                    \y -> pure [x, Symbol "struct", y])

-- Parse Assignement of a structure to a value (ex struct_name = { ... })
-- name struct_<type> = { a = 0, b = 0, c = 0 }
-- Only = is allowed
parseStructDefAssign :: Parser CPT
parseStructDefAssign = List <$> (parseWhitespace parseSymbol >>=
                    \x -> parseWhitespace (parseString "struct_") >> parseSymbol >>=
                    \(Symbol y) -> parseWhitespace (parseChar '=') >> parseWhitespace parseBodyStructAssign >>=
                    \b -> pure [x, Symbol ("struct_" ++ y), Token "=", b])

-- Parse scoped element insided a function ({ ... })
-- Like a function body, but without name and other useless stuff
-- TODO: check if function
parseScope :: Parser CPT
parseScope = ScopeElement <$> (parseChar '{' *> parseManyFct parseWhitespace parseOther <* parseWhitespace (parseChar '}'))

-- Parse expression ++:
-- a++
-- ++a
parseBinaryOperationPlusPlus :: Parser CPT
parseBinaryOperationPlusPlus = List <$> ((parseWhitespace (parseString "++") >> parseWhitespace parseSymbol >>=
        \t -> pure [Token "++", t]) <|> (parseWhitespace parseSymbol >>=
        \t -> parseWhitespace (parseString "++") >> pure [Token "++", t]))

-- Parse expression --:
-- a--
-- --a
parseBinaryOperationMinusMinus :: Parser CPT
parseBinaryOperationMinusMinus = List <$> ((parseWhitespace (parseString "--") >> parseWhitespace parseSymbol >>=
        \t -> pure [Token "--", t]) <|> (parseWhitespace parseSymbol >>=
        \t -> parseWhitespace (parseString "--") >> pure [Token "--", t]))

bodyCond =  parseBody <|>
            (parseLineExpr >>= \a -> Parser $ \ s -> Right (Body [a], s))

-- if = <condition> if <body> or while
-- (a > 0) if { ... }
-- (a > 0) while { ... }
parseCondOne :: Parser CPT
parseCondOne = List <$> (parseWhitespace parseListWExpr >>=
                \x -> parseWhitespace parseSymbol >>=
                \y -> parseWhitespace bodyCond >>=
                \z -> case y of
                    Symbol "if" -> pure [x, y, z]
                    Symbol "while" -> pure [x, y, z]
                    _ -> Parser $ \_ -> Left "Expected if or while")

-- else body { ... }
-- ... else ...
parseCondTwo :: Parser CPT
parseCondTwo = List <$> (parseWhitespace parseCondOne >>=
                \x -> parseWhitespace (parseString "else") >> parseWhitespace (parseCondTwo <|> parseCondOne <|> parseBody) >>=
                \z -> pure [x, Symbol "else", z])

-- import = "file" import;
parseImport :: Parser CPT
parseImport = parseWhitespace parseStringLiteral >>=
                \x -> parseWhitespace (parseString "import;") >> case x of
                    StringLiteral "jigot" -> Parser $ \s -> Right (Global [x, Symbol "import"], s)
                    StringLiteral x -> unsafePerformIO $ do
                        directory <- getCurrentDirectory
                        fileexist <- doesFileExist (directory ++ "/" ++ x)
                        if fileexist then do
                            content <- readFile x
                            case runParser (parseMultiple 0) content of
                                Left _ -> return $ Parser $ \_ -> Left "Invalid content"
                                Right (List y, "") -> return $ pure (head y)
                                Right (y, left) -> return $ Parser $ \_ -> Left "Invalid content"
                        else return $ Parser $ \_ -> Left "File not found"
                    _ -> Parser $ \_ -> Left "Impossible to reach"

-- for = (<end>;<cond>) for (<init>) <body>
parseFor :: Parser CPT
parseFor = List <$> (parseWhitespace (parseChar '(') *> parseWhitespace (parseAssignement <|> parseBinaryOperationPlusPlus <|> parseBinaryOperationMinusMinus <|> pure (List []))
                >>= \x -> parseWhitespace (parseChar ';') >> parseWhitespace parseExprMultiple <* parseWhitespace (parseChar ')')
                >>= \y -> parseWhitespace (parseString "for") >> parseWhitespace (parseChar '(') >> (parseVariableDef <|> pure (List [])) <* parseWhitespace (parseChar ')')
                >>= \z -> parseWhitespace (bodyCond <|> (parseChar ';' >> pure (Body [])))
                >>= \b -> pure [List [List [x, Token ";", y], Symbol "for", z, b]])

-- foreach = (item <type> : <list>) for <body>
parseForEach :: Parser CPT
parseForEach = List <$> (parseWhitespace (parseChar '(') *> parseWhitespace parseSymbol
                >>= \x -> parseWhitespace parseSymbol
                >>= \y -> parseWhitespace (parseChar ':')
                >>= \z -> parseWhitespace parseSymbol <* parseWhitespace (parseChar ')')
                >>= \a -> parseWhitespace (parseString "for") >> parseWhitespace bodyCond
                >>= \b -> pure [List [List [x, y, Token ":", a], Symbol "for", b]])

-- Parse for index of a list or string
-- i[0]
-- i[0 + 10]
parseSpecial :: Parser CPT
parseSpecial = List <$> (parseWhitespace parseSymbol >>=
                        \x -> parseWhitespace (parseChar '[') *> parseWhitespace (parseExprMultiple <|> parseSymbol <|> parseNumber) <* parseWhitespace (parseChar ']') >>=
                        \y -> pure [x, Token "@", y])

-- When you call an instruction that take return of a function
parseReturnDef :: Parser CPT
parseReturnDef = List <$> (parseWhitespace parseCallFunc >>=
                    \x -> parseWhitespace parseSymbol >>=
                    \y -> pure [x, y])

parseAccess :: Parser CPT
parseAccess = List <$> (parseWhitespace parseSymbol >>=
                    \x -> parseWhitespace (parseString "->") >> parseSymbol >>=
                    \y -> pure [x, Token "->", y])

parseCallBuiltInTwo :: CPT -> Parser CPT
parseCallBuiltInTwo sym = List <$> (parseChar '.' >> parseSymbol >>=
                    \x -> (parseList <|> pure (List [])) >>=
                    \y -> pure [sym, Token ".", x, y])

-- List [Symbol "array", Token ".", Symbol "fn", List [Number 1], List [same]
parseCallBuiltIn :: Parser CPT
parseCallBuiltIn = List <$> (parseWhitespace parseBasic >>=
                    \x -> parseMany (parseCallBuiltInTwo x) >>=
                    \z -> case z of
                        [] -> Parser $ \_ -> Left "Invalid built-in"
                        _ -> pure z)

parseCasting :: [Parser CPT] -> Parser CPT
parseCasting parsers = (List <$> (parseWhitespace (foldl1 (<|>) parsers) >>=
                    \x -> parseWhitespace (parseString "::") >> parseWhitespace parseSymbol >>=
                    \y -> pure [x, Token "::", y])) <|> foldl1 (<|>) parsers

parseCallFunc :: Parser CPT
parseCallFunc = List <$> (parseWhitespace (parseListWExpr <|> parseSpecial <|> parseCallBuiltIn <|> parseList <|> parseAccess <|> parseBasic)
                    >>= \x -> parseWhitespace parseSymbol
                    >>= \y -> pure [x, y])

parseToken :: Parser CPT
parseToken = Parser $ \ s -> case runParser (parseManyFct parseWhitespace (parseAnyChar getTokensList)) s of
    Right (x, xs) | length x == 1 || x `elem` getTokensDList -> Right (Token x, xs)
    _ -> Left "Error"

parseTokenWithAssign :: Parser CPT
parseTokenWithAssign = Parser $ \ s -> case runParser (parseSome (parseAnyChar getTokensList)) s of
    Right (x, xs) | x `elem` getTokensWithAssign -> Right (Token x, xs)
    _ -> Left "Error"

-- A REFAIRE MARCHE PAS TROP BIEN MAIS SI POSSIBLE SEULEMENT ICI DONC ENLEVER LE STATE COMMENT

dropMultiComment :: String -> (String, Bool)
dropMultiComment s = case dropWhile (/= '*') s of
    ('*':'/':ys) -> (ys, True)
    ('*':o) -> dropMultiComment o
    _ -> ([], False)

parseComment :: String -> String
parseComment "" = ""
parseComment [x] = [x]
parseComment (x:xs) = case (x, head xs) of
    ('/', '/') -> if length xs >= 2
        then case dropWhile (/= '\n') (tail xs) of
            "" -> ""
            s -> parseComment (tail s)
        else "//"
    ('#', o) -> parseComment ('/':'/':o:xs)
    ('/', '*') -> if length xs >= 2
        then case dropMultiComment (tail xs) of
            (ys, True) -> parseComment ys
            _ -> x : parseComment xs
        else "/*"
    _ -> x : parseComment xs

-- EVAL EXPR (NEED TO CHECK THAT RETURN A [List, operator, LIST next]) /\ INSTABLE /\

-- All the stuff that can pass in a parseExprMultiple
assignParsers = parseAccess <|> -- ex->v
    parseCallFunc <|> -- () lol
    parseSpecial <|> -- i[0]
    parseCallBuiltIn <|> -- lol.len
    parseListWExpr <|>
    parseBasic -- String, Literal or Number

dexpr = parseListWExpr <|> assignParsers

-- List of valid expressions
-- 1 + 2
-- 1 + 2 + 3
-- lol.len + 5
-- i[1] * 5
-- i[1] * 5 + 5
-- ((number -1) factorial) * number
-- () lol + 5
-- v + 5
-- 5
-- "lol"
-- "lol" + 5
parseExprUtilsSnd :: String -> [Parser CPT] -> Parser CPT
parseExprUtilsSnd c list = List <$> (parseWhitespace (parseList <|> dexpr) >>=
                \x -> parseWhitespace (parseString c) *> parseWhitespace (foldl1 (<|>) (list ++ [dexpr])) >>=
                \z -> pure [x, Token c, z])

parseExprUtils :: [String] -> [Parser CPT] -> Parser CPT
parseExprUtils [] parsers = Parser $ \ _ -> Left "Error"
parseExprUtils (x:xs) parsers = parseExprUtilsSnd x parsers <|> parseExprUtils xs parsers

-- Postfix operators: !, ++, --, ::
-- Unary (Later with cast)
-- Multiplicative operators: *, /, %
-- Additive operators: +, -
-- Shift operators: <<, >>
-- Relational operators: <, >, <=, >=
-- Binary operators: &, ^, |, &&, ||
-- Conditional operators: :?

parseExprPostfix :: Parser CPT
parseExprPostfix = parseExprUtils ["!", "++", "--", "::"] []

parseExprMultiplicative :: Parser CPT
parseExprMultiplicative = parseExprUtils ["*", "/", "%", "**"] [ parseExprPostfix ]

parseExprAdditive :: Parser CPT
parseExprAdditive = parseExprUtils ["+", "-"] [ parseExprMultiplicative, parseExprPostfix ]

parseExprShift :: Parser CPT
parseExprShift = parseExprUtils ["<<", ">>"] [ parseExprAdditive, parseExprMultiplicative, parseExprPostfix ]

parseExprRelational :: Parser CPT
parseExprRelational = parseExprUtils ["<", ">", "<=", ">="] [ parseExprShift, parseExprAdditive, parseExprMultiplicative, parseExprPostfix ]

parseExprEquality :: Parser CPT
parseExprEquality = parseExprUtils ["==", "!="] [ parseExprRelational, parseExprShift, parseExprAdditive, parseExprMultiplicative, parseExprPostfix ]

parseExprBinary :: Parser CPT
parseExprBinary = parseExprUtils ["&", "^", "|", "&&", "||"] [ parseExprEquality, parseExprRelational, parseExprShift, parseExprAdditive, parseExprMultiplicative, parseExprPostfix ]

parseExpr :: Parser CPT
parseExpr = parseExprUtils [":", "?"] [ parseExprBinary, parseExprEquality, parseExprRelational, parseExprShift, parseExprAdditive, parseExprMultiplicative, parseExprPostfix ] <|>
            parseExprBinary <|>
            parseExprEquality <|>
            parseExprRelational <|>
            parseExprShift <|>
            parseExprAdditive <|>
            parseExprMultiplicative <|>
            parseExprPostfix

parseNot :: Parser CPT
parseNot = List <$> (parseWhitespace (parseString "!") *> parseWhitespace assignParsers >>= \x -> pure [Token "!", x])

parseExprMultiple :: Parser CPT
parseExprMultiple = List <$> (parseWhitespace parseExpr >>=
                \x -> parseWhitespace parseToken >>=
                \y -> parseWhitespace (parseExprMultiple <|> assignParsers) >>=
                \z -> pure [x, y, z]) <|> parseWhitespace (parseExpr <|> parseNot <|> assignParsers)

------

parseBasic :: Parser CPT
parseBasic = parseWhitespace (
    parseArray <|>
    parseNumber <|>
    parseStringLiteral <|>
    parseSymbol
    )

parseOther :: Parser CPT
parseOther = parseWhitespace (
    parseFunction <|>
    parseVariableDefStrict <|>
    parseLineExpr <|>
    parseCondTwo <|>
    parseCondOne <|>
    parseFor <|>
    parseForEach <|>
    parseScope
    )

-- Patcher ce parser pour simple le cpt
parseCpt :: Parser CPT
parseCpt = parseWhitespace (
    parseFunctionGlobal <|>
    parseLineExprGlobal <|>
    parseImport <|>
    parseStructDef)

parseMultiple :: Int -> Parser CPT
parseMultiple idx = Parser $ \ s -> case runParser parseCpt (dropWhile (`elem` getWhitespace) (parseComment s)) of
    Left x -> Left ("Error in function/variable at line: " ++ show (idx + 1))
    Right (x, xs) -> case runParser (parseMultiple (idx+1)) xs of
        Left _ -> Right (List [x], dropWhile (`elem` getWhitespace) (parseComment xs))
        Right (List xs', xs'') -> Right (List (x:xs'), dropWhile (`elem` getWhitespace) (parseComment xs''))