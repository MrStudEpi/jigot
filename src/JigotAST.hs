--
-- EPITECH PROJECT, 2022
-- glados
-- File description:
-- AbstractTree
--

module JigotAST
(
    cptToAST,
    getDefaultValue,
    getVType,
    VType(VString, VBool, VList, VFloat, VInt, VVoid, VStruct, VGlobal, VFunc),
    CPT(Symbol, Number, List, Token, Body, Array, Parameters, StringLiteral, CFloat, ScopeElement, Global),
    AST(
        Function,
        Call,
        Variable,
        Cond,
        Lambda,
        AGlobal,
        ASymbol,
        AArray,
        AType,
        ASeq,
        AScope,
        ANumber,
        ABool,
        AFloat,
        ALiteral,
        AToken,
        AStruct,
        name,
        rtype,
        vars,
        body,
        function,
        args,
        vname,
        vtype,
        vvalue,
        icond,
        ibody,
        repeatUntilFalse,
        ifFalse
    ),
) where

-- Voir pour utiliser les vrai types a la place
data VType = VString | VInt | VBool | VList VType | VFloat | VVoid | VStruct String | VGlobal VType | VFunc VType
    deriving (Eq, Show)

data CPT = Symbol String -- keyword ref to a function name / type or variable
         | Number Int -- number
         | CFloat Float -- float number
         | List [CPT] -- list of each Expression
         | Global [CPT] -- list of each Expression
         | Body [CPT] -- list of each Expression inside {}
         | Parameters [CPT] -- list of each Expression inside function parameters
         | Array [CPT] -- for a list of element
         | Token String -- token is operators and misc stuff
         | StringLiteral String  -- string literal
         | ScopeElement [CPT] -- element of a scope
         deriving (Show, Eq)

data AST = Function
        {
            name :: String,
            rtype :: VType,
            vars :: AST,
            body :: [AST]
        }
        | Lambda
        {
            name :: String,
            rtype :: VType,
            vars :: AST,
            body :: [AST]
        }
        | Call
        {
            function :: AST,
            args :: [AST]
        }
        | Variable
        {
            vname :: String,
            vtype :: VType,
            vvalue :: AST
        }
        | AGlobal
        {
            vname :: String,
            vtype :: VType,
            vvalue :: AST
        }
        | Cond
        {
            icond :: AST,
            ibody :: AST,
            repeatUntilFalse :: Bool,
            ifFalse :: Maybe AST
        }
        | ASymbol String
        | ALiteral String
        | AType VType
        | AArray [AST]
        | ASeq [AST]
        | AScope [AST]
        | ANumber Int
        | AFloat Float
        | ABool Bool
        | AToken String
        | AStruct String [AST]
        deriving (Eq, Show)

getVType :: String -> Maybe VType
getVType "string" = Just VString
getVType "int" = Just VInt
getVType "bool" = Just VBool
getVType "void" = Just VVoid
getVType ('l':'i':'s':'t':'_':xs) = Just VList <*> getVType xs
getVType ('s':'t':'r':'u':'c':'t':'_':xs) = Just (VStruct xs)
getVType "float" = Just VFloat
getVType _ = Nothing

getDefaultValue :: VType -> Maybe AST
getDefaultValue VString = Just (ALiteral "")
getDefaultValue VInt = Just (ANumber 0)
getDefaultValue VBool = Just (ABool False)
getDefaultValue VFloat = Just (AFloat 0.0)
getDefaultValue (VList _) = Just (AArray [])
getDefaultValue (VStruct _) = Just (ASeq [])
getDefaultValue _ = Nothing

cptToAST :: CPT -> Maybe AST
cptToAST (Symbol "true") = Just $ ABool True
cptToAST (Symbol "false") = Just $ ABool False
cptToAST (Symbol sym) = case getVType sym of
    Just x -> Just (AType x)
    Nothing -> Just (ASymbol sym)
cptToAST (StringLiteral str) = Just (ALiteral str)
cptToAST (ScopeElement scope) = Just AScope <*> traverse cptToAST scope
cptToAST (Number x) = Just (ANumber x)
cptToAST (CFloat x) = Just (AFloat x)
cptToAST (Array array) = Just AArray <*> traverse cptToAST array
-- call expr end with a token (#example: (42 + 42) print;)
cptToAST (List [x, Token ";"]) = cptToAST x
-- Sugar syntax for function call
cptToAST (List [Token "++", x]) = Just (Call (AToken "=")) <*> traverse cptToAST [x, List [x, Token "+", Number 1]]
cptToAST (List [Token "--", x]) = Just (Call (AToken "=")) <*> traverse cptToAST [x, List [x, Token "-", Number 1]]
cptToAST (List [Token "!", x]) = Just (Call (AToken "!")) <*> traverse cptToAST [x]
-- casting
cptToAST (List [left, Token "::", Symbol vtype]) = case getVType vtype of
    Just _ -> Just (Call (AToken "::")) <*> traverse cptToAST [left, Symbol vtype]
    Nothing -> Nothing
cptToAST (List [left, Token "+=", right]) = Just (Call (AToken "=")) <*> traverse cptToAST [left, List [left, Token "+", right]]
cptToAST (List [left, Token "-=", right]) = Just (Call (AToken "=")) <*> traverse cptToAST [left, List [left, Token "-", right]]
cptToAST (List [left, Token "*=", right]) = Just (Call (AToken "=")) <*> traverse cptToAST [left, List [left, Token "*", right]]
cptToAST (List [left, Token "/=", right]) = Just (Call (AToken "=")) <*> traverse cptToAST [left, List [left, Token "/", right]]
cptToAST (List [left, Token "%=", right]) = Just (Call (AToken "=")) <*> traverse cptToAST [left, List [left, Token "%", right]]
cptToAST (List [left, Token "++=", right]) = Just (Call (AToken "=")) <*> traverse cptToAST [left, List [left, Token "++", right]]
-- Special Dot -- example -> name.len || name.append("hello")
cptToAST (List [left, Token ".", right, List []]) = Just (Call (AToken ".")) <*> traverse cptToAST [left, right]
cptToAST (List [left, Token ".", right, List avs]) = Just (Call (AToken ".")) <*> traverse cptToAST [left, right, List avs]
cptToAST (Global [StringLiteral "jigot", Symbol "import"]) = Just (Call (AToken "jigot") [])
-- Structure Declaration
cptToAST (List [Symbol sname, Symbol "struct", Body sbody]) = Just (AStruct sname) <*> traverse cptToAST sbody
-- Structure Usage
cptToAST (List [Symbol vlname, Symbol vltype, Token "=", Body value]) = case getVType vltype of
    Just (VStruct x) -> Just (Variable vlname (VStruct x)) <*> cptToAST (List value)
    _ -> Nothing -- Cannot declare aything else than a structure
cptToAST (Global [Symbol vlname, Symbol vltype, Token "=", value]) = case (getVType vltype, cptToAST value) of
    (Just VVoid, _) -> Nothing -- Cannot declare a void struct variable
    (Just (VStruct x), _) -> Just (AGlobal vlname (VStruct x)) <*> cptToAST value
    (Just x, Just Cond{}) -> Just (AGlobal vlname x) <*> cptToAST value
    (Just x, Just (ASymbol "null")) -> Just (AGlobal vlname x) <*> getDefaultValue x
    (Just (VList x), _) -> Just (AGlobal vlname (VList x)) <*> cptToAST (List [value, Token "::", Symbol (drop 5 vltype)])
    (Just x, _) -> Just (AGlobal vlname x) <*> cptToAST (List [value, Token "::", Symbol vltype])
    _ -> Nothing
cptToAST (List [List [List incr, Token ";", List cond], Symbol "for", List variable, Body body]) = Just AScope <*> traverse cptToAST [List variable, List [List cond, Symbol "while", Body (body ++ [List [List incr, Token ";"]])]]
-- ForEach -> { uzQxX3j4TL int = 0; while (index < list.length) { item vtype = list@index; <body> uzQxX3j4TL++; } }
cptToAST (List [List [Symbol item,Symbol vtype, Token ":",Symbol list],Symbol "for",Body body]) = Just AScope <*> traverse cptToAST
    [List [Symbol "uzQxX3j4TL", Symbol "int", Token "=", Number 0], List [List [Symbol "uzQxX3j4TL", Token "<", List [Symbol list, Token ".", Symbol "len", List []]],
    Symbol "while", Body (List [Symbol item, Symbol vtype, Token "=", List [Symbol list, Token "@", Symbol "uzQxX3j4TL"]]:body ++ [List [Token "++", Symbol "uzQxX3j4TL"]])]]
cptToAST (List [cond, Token "?", List [trueV, Token ":", falseV]]) = Just Cond <*> cptToAST cond <*> cptToAST trueV <*> Just False <*> Just (cptToAST falseV)
cptToAST (List [left, Token op, right]) = Just (Call (AToken op)) <*> traverse cptToAST [left, right]
-- declare variable at initialization (#example: x int = 42)
-- define variable at initialization with default value (#example: x = 42) || (#example: x = (42 * 2) + 1) WIP deny les tpes interdits
cptToAST (List [Symbol name, Symbol vtype, Token "=", value]) = case (getVType vtype, cptToAST value) of
    (Just VVoid, _) -> Nothing -- Cannot declare a void variable
    (Just (VStruct x), _) -> Just (Variable name (VStruct x)) <*> cptToAST value -- Declare a struct
    (Just x, Just Cond{}) -> Just (Variable name x) <*> cptToAST value -- Cannot use auto cast on ternary
    (Just x, Just (ASymbol "null")) -> Just (Variable name x) <*> getDefaultValue x -- When you declare a variable with null value
    (Just (VList x), _) -> Just (Variable name (VList x)) <*> cptToAST (List [value, Token "::", Symbol (drop 5 vtype)]) -- drop the letter of list_
    (Just x, _) -> Just (Variable name x) <*> cptToAST (List [value, Token "::", Symbol vtype])
    _ -> Nothing
-- variable declaration (#example: x int) or (#example: \"x\" print) or call with a function that take a string as parameter
cptToAST (List [Symbol name, Symbol value]) = case getVType value of
    Just vtype -> Just (Variable name vtype) <*> getDefaultValue vtype
    Nothing -> Just Call <*> cptToAST (Symbol value) <*> traverse cptToAST [Symbol name]
-- call function (#example: (42 42) onClick) 
-- call lambda (#example: (42, 42) lambda (x int, y int) { (x + y) return })
-- with list
cptToAST (Parameters []) = Just (ASeq [])
cptToAST (Parameters (x:xs)) = Just ASeq <*> traverse cptToAST (x:xs)
cptToAST (List [List params, Symbol much]) = case cptToAST (List params) of
    Just (ASeq x) -> Just Call <*> cptToAST (Symbol much) <*> Just x
    Just x -> Just Call <*> cptToAST (Symbol much) <*> Just [x]
    _ -> Nothing --Just Call <*> cptToAST (Symbol much) <*> traverse cptToAST params ? Why
-- without list
cptToAST (List [param, Symbol name]) = Just Call <*> cptToAST (Symbol name) <*> traverse cptToAST [param]
-- define function (#example: onClick int (x int, y int) { (x + y) return })
cptToAST (Global [Symbol name, Symbol vtype, Parameters params, Body body]) = Just (Function name) <*> getVType vtype <*> cptToAST (Parameters params) <*> traverse cptToAST body
cptToAST (List [Symbol name, Symbol vtype, Parameters params, Body body]) = Just (Lambda name) <*> getVType vtype <*> cptToAST (Parameters params) <*> traverse cptToAST body
-- if statement (#example: (42 == 42) if { 42 return })
cptToAST (List [List cond, Symbol "if", Body body]) = Just Cond <*> cptToAST (List cond) <*> cptToAST (List body) <*> Just False <*> Just Nothing
-- if else statement (#example: (42 == 42) if { 42 return } else if { 0 return })
cptToAST (List [List before, Symbol "else", List after]) = case (cptToAST (List before), cptToAST (List after)) of
    (Just x@Cond{}, Just y@Cond{}) -> Just (Cond (icond x) (ibody x) False (Just y))
    _ -> Nothing
-- else statement (#example: (42 == 42) if { 42 return } else { 0 return })
cptToAST (List [List before, Symbol "else", Body elseBody]) = case cptToAST (List before) of
    Just (Cond cond body repeatUntilFalse _) -> Just (Cond cond body repeatUntilFalse) <*> Just (cptToAST (List elseBody))
    _ -> Nothing
-- while statement (#example: (42 == 42) while { 42 print })
cptToAST (List [List cond, Symbol "while", Body body]) = Just Cond <*> cptToAST (List cond) <*> cptToAST (List body) <*> Just True <*> Just Nothing
-- multi expressions management (like many functions in same file)
cptToAST (List [x]) = cptToAST x
cptToAST (List (expr:xs)) = Just ASeq <*> traverse cptToAST (expr:xs)
cptToAST (List []) = Just (ASeq [])
cptToAST _ = Nothing