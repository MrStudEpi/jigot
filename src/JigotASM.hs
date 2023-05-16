--
-- EPITECH PROJECT, 2022
-- glados
-- File description:
-- Stack
--

module JigotASM
(
    astToStackIST,
    getLastIndex,
    Environment(Environment, ename, retype, nextenv, value),
    Compiler(Compiler, runCompiler),
    StackV(StackNumber, StackFloat, StackString, StackBool, StackProcedure, StackArray, StackEProcedure, StackStruct, StackArrayIndex, StackCast),
    StackIST(
        Push,
        Expression,
        FileOperation,
        GetAttr,
        Scope,
        Collect,
        Get,
        Set,
        CallFn,
        OP,
        Print,
        Not,
        JumpIfFalse,
        Jump,
        Dot,
        Return,
        Misc,
        Exit
    ),
    Code(Code)
) where

import JigotAST
import Data.List
import Control.Monad

data StackV = StackString String
            | StackNumber Int
            | StackFloat Float
            | StackBool Bool
            | StackProcedure StackIST
            | StackEProcedure Environment
            | StackArray [StackV]
            | StackCast VType
            | StackArrayIndex Environment Int -- Environment is the array, Int is the index
            | StackStruct (Environment, String) -- Struct linked and attribute name
                deriving (Eq)

instance Show StackV where
    show (StackString s) = s
    show (StackNumber n) = "" ++ show n
    show (StackFloat f) = "" ++ show f
    show (StackBool b) = "" ++ show b
    show (StackProcedure _) = "Procedure"
    show (StackEProcedure e) = "EProcedure >> " ++ show e
    show (StackStruct st) = "Struct >> " ++ show st
    show (StackArray a) = "" ++ show a
    show (StackCast t) = "Cast >> " ++ show t
    show (StackArrayIndex e i) = "ArrayIndex >> " ++ show e ++ " " ++ show i

data StackIST = Push StackV -- push a value on the stack
                | GetAttr String String -- get an attribute of a struct
                | Scope [(StackIST, Int)] [Environment] -- scope with attributes
                | Expression String-- expression like <data>.<expression> with type that validate the expression
                | FileOperation Int -- read a file
                | CallFn -- Call a function
                | Set -- Set a variable 
                | Get String -- get a variable
                | Collect Int -- Collect int element of top of the stack to create stackArray
                | OP String 
                | Not -- Not
                | JumpIfFalse Int -- Jump if result if the function fn to first instruction is true to second index element instruction
                | Jump Int -- Jump to the index element instruction
                | Return -- Return
                | Print -- Print
                | Exit -- Exit code
                | Misc
                | Dot
                deriving (Show, Eq)

data Environment = Environment {
    ename :: String,
    retype :: VType,
    nextenv :: [Environment],
    value :: Maybe (Either Code StackV)
} deriving (Show)

instance Eq Environment where
    (==) (Environment n1 _ _ _) (Environment n2 _ _ _) = n1 == n2

newtype Code = Code [(StackIST, Int)] deriving (Show, Eq)

-- Le changer avec runCompiler
newtype Compiler a = Compiler {
    runCompiler :: [Environment] -> Either String (a, [Environment])
}

instance Functor Compiler where

instance Applicative Compiler where
    pure x = Compiler $ \ _ -> Right (x, [])

getLastIndex :: [(StackIST, Int)] -> Int
getLastIndex [(_, index)] = index
getLastIndex (_:xs) = getLastIndex xs
getLastIndex _ = -1

getReservedKeywords :: [String]
getReservedKeywords = ["if", "else", "while", "for", "return", "true", "false", "null", "print", "exit", "len", "append", "tail", "popl", "popr"]

astToStackISTCond :: AST -> Int -> Compiler Code
-- if / else if
astToStackISTCond (Cond cond body False (Just other)) index = Compiler $ runCompiler (astToStackIST cond index) >=>
    \ (Code x, e') -> runCompiler (astToStackIST body (index + length x + 1)) e' >>=
    \ (Code x', e'') -> runCompiler (astToStackIST other (index + length x + 3)) e'' >>=
    \ (Code x'', e''') -> Right (Code (x ++ [(JumpIfFalse (index + length x + 3), index + length x)] ++ [(Scope x' e'', (index + length x) + 1)] ++ [(Jump (index + length x + 3 + length x''), index + length x + 2)] ++ x''), e''')
-- else
astToStackISTCond (Cond cond body False Nothing) index = Compiler $ runCompiler (astToStackIST cond index) >=>
    \ (Code x, e') -> runCompiler (astToStackIST body (index + length x + 1)) e' >>=
    \ (Code x', e'') -> Right (Code (x ++ [(JumpIfFalse (index + length x + 2), index + length x)] ++ [(Scope x' e'', index + length x + 1)]), [])
-- for / while / foreach
astToStackISTCond (Cond cond body True Nothing) index = Compiler $ runCompiler (astToStackIST cond index) >=>
    \ (Code x, e') -> runCompiler (astToStackIST body (index + length x + 1)) e' >>=
    \ (Code x', e'') -> Right (Code (x ++ [(JumpIfFalse (index + length x + 3), index + length x)] ++ [(Scope x' e'', index + length x + 1)] ++ [(Jump index, index + length x + 2)]), [])
astToStackISTCond _ _ = Compiler $ \ _ -> Left "Error: Condition can't be compile in this context"

astToStackISTFunction :: AST -> Int -> Compiler Code
-- Function def
astToStackISTFunction (Function fname vtype vars body) index = Compiler $ \ e -> case (find (\x -> fname == ename x) e, find (\x -> fname == x) getReservedKeywords) of
    (_, Just x) -> Left (x ++ " is a reserved keyword")
    (Just (Environment _ (VFunc _) _ _), _) -> Left ("Function already defined: " ++ fname)
    (_, _) -> case runCompiler (astToStackIST (ASeq body) index) e of
        Right (Code x', e') -> case runCompiler (astToStackIST vars index) [] of
            Right (Code _, e'') -> Right (Code [], [Environment fname (VFunc vtype) e'' (Just (Left $ Code [(Scope x' e', 0)]))])
            Left x -> Left x
        Left x -> Left x
astToStackISTFunction _ _ = Compiler $ \ _ -> Left "Error"

-- Just (Environment yname ytype yenv yvalue)
generateEmptyStruct :: [Environment] -> Maybe [Environment]
generateEmptyStruct elements = traverse (\e@(Environment yname ytype yenv yvalue) -> case getDefaultValue ytype of
        Just x -> case x of
            ALiteral n -> Just (Environment yname ytype yenv (Just (Right (StackString n))))
            ANumber n -> Just (Environment yname ytype yenv (Just (Right (StackNumber n))))
            AFloat n -> Just (Environment yname ytype yenv (Just (Right (StackFloat n))))
            ABool n -> Just (Environment yname ytype yenv (Just (Right (StackBool n))))
            AArray n -> Just (Environment yname ytype yenv (Just (Right (StackArray []))))
            ASeq _ -> case generateEmptyStruct yenv of
                Just subenv -> Just (Environment yname ytype subenv Nothing)
                Nothing -> Nothing
            _ -> Nothing
        Nothing -> Nothing
    ) elements

createAttributes :: [Environment] -> [(StackIST, Int)] -> String -> Maybe [(StackIST, Int)]
createAttributes elements (x:(Get y, j):(Get "=", k):(CallFn, l):xs) sname = case find (\y' -> ename y' == y) elements of
    Just t -> case createAttributes elements xs sname of
        Just d -> Just ([x, (GetAttr sname y, j), (Get "=", k), (CallFn, l)] ++ d)
        Nothing -> Nothing
    Nothing -> Nothing
createAttributes _ x' _ = Just x'
-- Structure (nextenv = parameters) / The declared variable / The name of the structure in variable (ex)
-- Objectif: cloner la structure et la mettre dans la variable
-- Objectif declarer toutes les variables de la structure avec leur valeur par defaut et dans le code juste mettre ce qui sont redeclarer

-- Check for variable with expression assignation
-- Check if the variable is already defined
astToStackISTVariable :: AST -> Int -> Compiler Code
-- Find structure linked to the variable and push it on the stack
astToStackISTVariable var@(Variable name (VStruct sname) value@ASeq{}) index = Compiler $ \ e -> case (find (\x -> (ename x == name && retype x == VStruct name)) e, find (\x -> ename x == sname) e) of
    (Just x, _) -> Left ("Variable already defined: " ++ name)
    (_, Just structe) -> case runCompiler (astToStackIST value index) e of
        Right (Code x', _) -> case (generateEmptyStruct (nextenv structe), createAttributes (nextenv structe) x' name) of
            (Just datas, Just code') -> Right (Code code', [Environment name (VStruct sname) datas Nothing])
            _ -> Left "Error: Structure has a variable with invalid type / is not set properly"
        Left x -> Left x
    (Nothing, Nothing) -> Left ("Structure not found: " ++ sname)
astToStackISTVariable (Variable name vtype value) index = Compiler $ \ e -> case find (\x -> ename x == name) e of
    Just x -> case retype x of
        VFunc _ -> case runCompiler (astToStackIST value index) e of
            Right (Code x', _) -> Right (Code (x' ++ [(Get name, length x' + index),(Get "=", length x' + index + 1),(CallFn, length x' + index + 2)]), [Environment name vtype [] Nothing])
            Left x -> Left x
        _ -> Left ("Variable already defined: " ++ name)
    Nothing -> case runCompiler (astToStackIST value index) e of
        Right (Code x', _) -> Right (Code (x' ++ [(Get name, length x' + index),(Get "=", length x' + index + 1),(CallFn, length x' + index + 2)]), [Environment name vtype [] Nothing])
        Left x -> Left x
astToStackISTVariable _ _ = Compiler $ \ _ -> Left "Error: Variable can't be compile in this context"

-- Pas d'utilisation de scope dans un call
astToStackISTCall :: AST -> Int -> Compiler Code
astToStackISTCall (Call (AToken "->") l@[ASymbol a, ASymbol b]) index = Compiler $ \ e -> case runCompiler (astToStackIST (ASeq (reverse l)) index) e of
    Right (Code _, e') -> Right (Code [(GetAttr a b, index)], e')
    Left x -> Left x
astToStackISTCall (Call from list) index = Compiler $ runCompiler (astToStackIST (ASeq (reverse list)) index) >=>
    \ (Code x, e') -> runCompiler (astToStackIST from (index + length x)) e' >>=
    \ (Code x', _) -> Right (Code (x ++ x' ++ [(CallFn, index + length x + length x')]), e')
astToStackISTCall _ _ = Compiler $ \ _ -> Left "Error: Call can't be compile in this context"

findDuplicate :: [Environment] -> Maybe [Environment]
findDuplicate [] = Just []
findDuplicate (x:xs) = case find (\y -> ename x == ename y) xs of
    Just _ -> Nothing
    Nothing -> case findDuplicate xs of
        Just x' -> Just (x:x')
        Nothing -> Nothing

astToStackIST :: AST -> Int -> Compiler Code
astToStackIST (ASymbol x) index = pure (Code [(Get x, index)])
astToStackIST (AToken x) index = pure (Code [(Get x, index)])
astToStackIST (AType x) index = pure (Code [(Push (StackCast x), index)])
astToStackIST (ANumber x) index = pure (Code [(Push (StackNumber x), index)])
astToStackIST (AFloat x) index = pure (Code [(Push (StackFloat x), index)])
astToStackIST (ABool x) index = pure (Code [(Push (StackBool x), index)])
astToStackIST (ALiteral x) index = pure (Code [(Push (StackString x), index)])
astToStackIST (AScope list) index = Compiler $ runCompiler (astToStackIST (ASeq list) index) >=>
    \ (Code x', e') -> Right (Code [(Scope x' e', index)], [])
astToStackIST (AStruct name vars) index = Compiler $ \ e -> case find (\x -> ename x == name && retype x == VStruct "@") e of
    Just x -> Left ("Structure already defined: " ++ name)
    Nothing -> runCompiler (astToStackIST (ASeq vars) index) e >>=
        \ (Code x', e') -> Right (Code [], [Environment name (VStruct "@") e' Nothing])
astToStackIST (AArray x) index = Compiler $ runCompiler (astToStackIST (ASeq (reverse x)) index) >=>
    \ (Code code, e') -> Right (Code (code ++ [(Collect (length x), index + length code)]), e')
astToStackIST (ASeq []) _ = pure (Code [])
astToStackIST (ASeq (x:xs)) index = Compiler $ \ e -> case runCompiler (astToStackIST x index) e of
    Right (Code liste, e') -> case runCompiler (astToStackIST (ASeq xs) (index + length liste)) e' of
        Right (Code [], es) -> case findDuplicate (e' ++ es) of 
            Just e'' -> Right (Code liste, e'')
            Nothing -> Left "Duplicate variable or function"
        Right (Code list, es) -> case findDuplicate (e' ++ es) of 
            Just e'' -> Right (Code (liste ++ list), e' ++ es)
            Nothing -> Left "Duplicate variable or function"
        Left x -> Left x
    Left x -> Left x
astToStackIST lam@Lambda{} index = astToStackISTFunction (Function (name lam) (rtype lam) (vars lam) (body lam)) index
astToStackIST f@(Function "_main" VInt _ _) index = Compiler $ \ e -> case runCompiler (astToStackISTFunction f { rtype = VGlobal (rtype f)} index) e of
    Right (Code [], e') -> Right (Code [(Get "_main", index), (CallFn, index + 1), (Return, index + 2)], e')
    Right (Code x, [Environment "_main" vtype args (Just (Left (Code code)))]) ->
        Right (Code [(Get "_main", index), (CallFn, index + 1), (Return, index + 2)], [Environment "_main" vtype args (Just (Left (Code (x++code))))])
    Left x -> Left x
astToStackIST var@AGlobal{} index = astToStackISTVariable (Variable (vname var) (VGlobal (vtype var)) (vvalue var)) index
astToStackIST var@Variable{} index = Compiler $ \ e -> case (find (\x -> vname var == x) getReservedKeywords, getVType (vname var)) of
    (Just x, _) -> Left (x ++ " is a reserved keyword")
    (_, Just x) -> Left (vname var ++ " is a type cannot be used for name")
    (Nothing, Nothing) -> runCompiler (astToStackISTVariable var index) e
astToStackIST fn@Function{} index = astToStackISTFunction (Function (name fn) (VGlobal (rtype fn)) (vars fn) (body fn)) index
astToStackIST call@Call{} index = astToStackISTCall call index
astToStackIST cond@Cond {} index = astToStackISTCond cond index
astToStackIST _ _= Compiler $ \ _ -> Left "Error: Can't compile this AST"