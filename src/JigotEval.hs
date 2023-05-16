--
-- EPITECH PROJECT, 2022
-- glados
-- File description:
-- JigotEval
--

module JigotEval (
    evalCode,
    StackState(StackState, stack, env, code),
    Stack(Stack),
    CodeReader(NormalRead, JumpRead, ReturnRead, PrintRead, ExitRead),
    runEvaluator,
    getStackISTAt,
    doOp,
    getBuiltIn,
    isTypeCoherent,
    getTypeLinked,
    createSubEnv,
    editEnv,
    editGlobalEnv,
    miscJigot,
    getStackISTAt,
    isEnvironmentCoherent,
    collectOnlyGlobal,
    findAbsoluteValue
) where

import JigotASM
import JigotAST (VType(VString, VFloat, VInt, VBool, VList, VVoid, VStruct, VGlobal, VFunc))

import System.IO.Unsafe (unsafePerformIO)
import Data.List
import System.Directory
import Control.Lens
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.Bits

newtype Stack = Stack [StackV]  deriving (Show)

data StackState = StackState {
    stack :: Stack,
    env :: [Environment],
    code :: Code
} deriving (Show)

data CodeReader = NormalRead | JumpRead Code | ReturnRead | PrintRead String | ScopeRead | ExitRead Int
    deriving (Show)

newtype Evaluator a = Evaluator {
    runEvaluator :: a -> Either String (a, CodeReader)
}

-- Need to do IST to File (Future)
-- Need to understand IST and execute it

getBuiltIn :: String -> Maybe StackIST
getBuiltIn "print" = Just Print
getBuiltIn "return" = Just Return
getBuiltIn "jigot" = Just Misc
getBuiltIn "exit" = Just Exit
-- Expressions (after '.')
getBuiltIn "len" = Just (Expression "len")
getBuiltIn "append" = Just (Expression "append")
getBuiltIn "tail" = Just (Expression "tail")
getBuiltIn "popl" = Just (Expression "popl")
getBuiltIn "popr" = Just (Expression "popr")
-- Utils
getBuiltIn "readFile" = Just (FileOperation 0)
getBuiltIn "writeFile" = Just (FileOperation 1)
getBuiltIn "." = Just Dot
-- Operators
getBuiltIn "::" = Just (OP "::") -- OK
getBuiltIn "@" = Just (OP "@") -- OK
getBuiltIn "+" = Just (OP "+") -- OK
getBuiltIn "**" = Just (OP "**") -- OK
getBuiltIn "=" = Just Set
getBuiltIn "-" = Just (OP "-") -- OK
getBuiltIn "*" = Just (OP "*") -- OK
getBuiltIn "/" = Just (OP "/") -- OK
getBuiltIn "%" = Just (OP "%") -- OK
getBuiltIn "==" = Just (OP "==") -- OK
getBuiltIn "!=" = Just (OP "!=") -- OK
getBuiltIn ">" = Just (OP ">") -- OK
getBuiltIn ">=" = Just (OP ">=") -- OK
getBuiltIn "++" = Just (OP "++") -- OK
getBuiltIn "<" = Just (OP "<") -- OK
getBuiltIn "<=" = Just (OP "<=") -- OK
getBuiltIn "&&" = Just (OP "&&") -- OK
getBuiltIn "||" = Just (OP "||") -- OK
getBuiltIn "&" = Just (OP "&") -- OK
getBuiltIn "|" = Just (OP "|") -- OK
getBuiltIn "^" = Just (OP "^") -- OK
getBuiltIn ">>" = Just (OP ">>") -- OK
getBuiltIn "<<" = Just (OP "<<") -- OK
getBuiltIn "!" = Just Not
getBuiltIn _ = Nothing

-- Operation Stack (Stack after ist)
-- Need to find a way to access stack at all moment

isTypeCoherent :: VType -> StackV -> Bool
isTypeCoherent VInt (StackNumber _) = True
isTypeCoherent VFloat (StackFloat _) = True
isTypeCoherent VString (StackString _) = True
isTypeCoherent VBool (StackBool _) = True
isTypeCoherent (VList n) (StackArray list) = all (isTypeCoherent n) list
isTypeCoherent (VGlobal n) d = isTypeCoherent n d
isTypeCoherent (VFunc n) d = isTypeCoherent n d
isTypeCoherent x (StackEProcedure (Environment _ y _ _)) = x == y
isTypeCoherent _ _ = False

getAbsType :: VType -> VType
getAbsType (VGlobal n) = n
getAbsType (VFunc n) = n
getAbsType x = x

getTypeLinked :: StackV -> VType
getTypeLinked (StackNumber _) = VInt
getTypeLinked (StackFloat _) = VFloat
getTypeLinked (StackString _) = VString
getTypeLinked (StackBool _) = VBool
getTypeLinked (StackArray (x:_)) = VList (getTypeLinked x)
getTypeLinked (StackEProcedure (Environment _ y _ _)) = y
getTypeLinked _ = VVoid

doOp :: StackIST -> StackV -> StackV -> Maybe StackV
doOp (OP "+") (StackNumber n1) (StackNumber n2) = Just (StackNumber (n1 + n2))
doOp (OP "+") (StackNumber n1) (StackFloat n2) = Just (StackFloat (fromIntegral n1 + n2))
doOp (OP "+") (StackFloat n1) (StackNumber n2) = Just (StackFloat (n1 + fromIntegral n2))
doOp (OP "+") (StackFloat n1) (StackFloat n2) = Just (StackFloat (n1 + n2))
doOp (OP "-") (StackNumber n1) (StackNumber n2) = Just (StackNumber (n1 - n2))
doOp (OP "-") (StackFloat n1) (StackFloat n2) = Just (StackFloat (n1 - n2))
doOp (OP "*") (StackNumber n1) (StackNumber n2) = Just (StackNumber (n1 * n2))
doOp (OP "*") (StackFloat n1) (StackFloat n2) = Just (StackFloat (n1 * n2))
doOp (OP "/") (StackNumber n1) (StackNumber n2) = if n2 == 0 then Nothing else Just (StackNumber (n1 `div` n2))
doOp (OP "/") (StackFloat n1) (StackFloat n2) = if n2 == 0 then Nothing else Just (StackFloat (n1 / n2))
doOp (OP "%") (StackNumber n1) (StackNumber n2) = if n2 == 0 then Nothing else Just (StackNumber (n1 `mod` n2))
doOp (OP "**") (StackNumber n1) (StackNumber n2) = Just (StackNumber (n1 ^ n2))
doOp (OP "**") (StackFloat n1) (StackFloat n2) = Just (StackFloat (n1 ** n2))
doOp (OP "==") (StackNumber n1) (StackNumber n2) = Just (StackBool (n1 == n2))
doOp (OP "==") (StackFloat n1) (StackFloat n2) = Just (StackBool (n1 == n2))
doOp (OP "==") (StackString n1) (StackString n2) = Just (StackBool (n1 == n2))
doOp (OP "==") (StackBool n1) (StackBool n2) = Just (StackBool (n1 == n2))
doOp (OP "!=") (StackNumber n1) (StackNumber n2) = Just (StackBool (n1 /= n2))
doOp (OP "!=") (StackFloat n1) (StackFloat n2) = Just (StackBool (n1 /= n2))
doOp (OP "!=") (StackString n1) (StackString n2) = Just (StackBool (n1 /= n2))
doOp (OP "!=") (StackBool n1) (StackBool n2) = Just (StackBool (n1 /= n2))
doOp (OP ">") (StackNumber n1) (StackNumber n2) = Just (StackBool (n1 > n2))
doOp (OP ">") (StackFloat n1) (StackFloat n2) = Just (StackBool (n1 > n2))
doOp (OP ">") (StackString n1) (StackString n2) = Just (StackBool (n1 > n2))
doOp (OP ">=") (StackNumber n1) (StackNumber n2) = Just (StackBool (n1 >= n2))
doOp (OP ">=") (StackFloat n1) (StackFloat n2) = Just (StackBool (n1 >= n2))
doOp (OP ">=") (StackString n1) (StackString n2) = Just (StackBool (n1 >= n2))
doOp (OP "<") (StackNumber n1) (StackNumber n2) = Just (StackBool (n1 < n2))
doOp (OP "<") (StackFloat n1) (StackFloat n2) = Just (StackBool (n1 < n2))
doOp (OP "<") (StackString n1) (StackString n2) = Just (StackBool (n1 < n2))
doOp (OP "<=") (StackNumber n1) (StackNumber n2) = Just (StackBool (n1 <= n2))
doOp (OP "<=") (StackFloat n1) (StackFloat n2) = Just (StackBool (n1 <= n2))
doOp (OP "<=") (StackString n1) (StackString n2) = Just (StackBool (n1 <= n2))
doOp (OP "&&") (StackBool n1) (StackBool n2) = Just (StackBool (n1 && n2))
doOp (OP "||") (StackBool n1) (StackBool n2) = Just (StackBool (n1 || n2))
doOp (OP "&") (StackNumber n1) (StackNumber n2) = Just (StackNumber (n1 .&. n2))
doOp (OP "|") (StackNumber n1) (StackNumber n2) = Just (StackNumber (n1 .|. n2))
doOp (OP "^") (StackNumber n1) (StackNumber n2) = Just (StackNumber (n1 `xor` n2))
doOp (OP "<<") (StackNumber n1) (StackNumber n2) = Just (StackNumber (n1 `shiftL` n2))
doOp (OP ">>") (StackNumber n1) (StackNumber n2) = Just (StackNumber (n1 `shiftR` n2))
doOp (OP "++") (StackString n1) (StackString n2) = Just (StackString (n1 ++ n2))
doOp (OP "++") (StackArray n1) (StackArray n2) = Just (StackArray (n1 ++ n2))
doOp (OP "++") (StackArray n1) n2= Just (StackArray (n1 ++ [n2]))
doOp (OP "::") (StackNumber n) (StackCast VInt) = Just (StackNumber n)
doOp (OP "::") (StackNumber n) (StackCast VFloat) = Just (StackFloat (fromIntegral n))
doOp (OP "::") (StackNumber n) (StackCast VString) = Just (StackString (show n))
doOp (OP "::") (StackNumber n) (StackCast VBool) = Just (StackBool (n /= 0))
doOp (OP "::") (StackFloat n) (StackCast VFloat) = Just (StackFloat n)
doOp (OP "::") (StackFloat n) (StackCast VInt) = Just (StackNumber (floor n))
doOp (OP "::") (StackFloat n) (StackCast VString) = Just (StackString (show n))
doOp (OP "::") (StackFloat n) (StackCast VBool) = Just (StackBool (n /= 0))
doOp (OP "::") (StackString n) (StackCast VString) = Just (StackString n)
doOp (OP "::") (StackString n) (StackCast VInt) = Just (StackNumber (fromMaybe 0 (readMaybe n :: Maybe Int)))
doOp (OP "::") (StackString n) (StackCast VFloat) = Just (StackFloat (fromMaybe 0 (readMaybe n :: Maybe Float)))
doOp (OP "::") (StackString n) (StackCast VBool) = Just (StackBool (n /= "False"))
doOp (OP "::") (StackBool n) (StackCast VBool) = Just (StackBool n)
doOp (OP "::") (StackBool n) (StackCast VInt) = Just (StackNumber (if n then 1 else 0))
doOp (OP "::") (StackBool n) (StackCast VFloat) = Just (StackFloat (if n then 1 else 0))
doOp (OP "::") (StackBool n) (StackCast VString) = Just (StackString (if n then "True" else "False"))
doOp (OP "::") (StackArray n) (StackCast x) = Just (StackArray (fmap (\y -> fromMaybe y (doOp (OP "::") y (StackCast x))) n))
doOp _ _ _ =  Nothing

createSubEnv :: [Environment] -> [Environment] -> [StackV] -> Maybe ([Environment], [StackV])
createSubEnv ant [] [] = Just (ant, [])
createSubEnv _ _ [] = Nothing
createSubEnv ant [] list = Just (ant, list)
createSubEnv ant (env:env') (x:xs) = if isTypeCoherent (retype env) x then
    case createSubEnv ant env' xs of
        Just (env'', xs') -> case find (\xe -> ename xe == ename env) ant of
            Just eng -> Just ((eng {value = Just (Right x)}):delete eng ant, xs')
            Nothing -> Just ((env {value = Just (Right x)}):env'', xs')
        Nothing -> Nothing
    else Nothing

editEnv :: [Environment] -> [Environment] -> [Environment]
editEnv env [] = env
editEnv env (x:xs) = case find (\xe -> ename xe == ename x) env of
    Just eng -> editEnv (eng {value = value x}:delete eng env) xs
    Nothing -> editEnv env xs

editGlobalEnv :: [Environment] -> [Environment] -> [Environment]
editGlobalEnv env [] = env
editGlobalEnv env (x:xs) = case find (\xe -> ename xe == ename x) env of
    Just eng -> case retype eng of
        (VFunc (VGlobal _)) -> editGlobalEnv (eng {value = value x}:delete eng env) xs
        (VGlobal _) -> editGlobalEnv (eng {value = value x}:delete eng env) xs
        _ -> editGlobalEnv env xs
    Nothing -> editGlobalEnv env xs

miscJigot :: IO()
miscJigot = do
    putStrLn "⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿"
    putStrLn "⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿"
    putStrLn "⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⣿⣿⣿⣿"
    putStrLn "⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⠟⢉⣉⠙⠻⣿⣿⡿⠋⠀⠈⢻⣿⣿"
    putStrLn "⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠟⠋⠀⠀⢸⢩⣽⣦⣨⠟⠁⠀⠀⠀⠚⠿⣿"
    putStrLn "⣿⣿⣿⣿⣿⣿⣿⡿⠿⠟⠛⠉⠀⠀⠀⠀⠀⠸⡜⣿⠟⠁⠀⠀⠀⠀⠀⠀⠀⣸"
    putStrLn "⣿⣿⣿⣿⣿⣿⡏⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢳⡘⣧⣀⣠⣤⠶⣶⣿⣿⣿⣿"
    putStrLn "⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠳⣌⠻⣿⣿⠀⢿⣿⣿⣿⣿"
    putStrLn "⣿⣿⣿⣿⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠳⠶⠾⢀⣿⣿⣿⣿⣿"
    putStrLn "⣿⡿⠛⠛⠛⠛⢿⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣠⣤⣶⣿⣿⣿⣿⣿⣿"
    putStrLn "⣿⡄⠀⠀⠀⠀⠈⢿⣆⠀⠀⠀⠀⠀⠀⠀⠀⣠⣴⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿"
    putStrLn "⣿⣿⣶⡖⠀⠀⠀⣴⣿⣷⣄⡀⠀⠀⠀⣴⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿"
    putStrLn "⣿⣿⣿⣇⠀⢀⣾⣿⣿⣿⣿⣿⣷⣶⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿"
    putStrLn "⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿"
    putStrLn "⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿"

getStackISTAt :: Int -> Code -> Maybe Code
getStackISTAt n (Code ((x,i):xs)) = if n == i then Just (Code ((x,i):xs)) else getStackISTAt n (Code xs)
getStackISTAt _ _ = Nothing

isEnvironmentCoherent :: [Environment] -> Environment -> Bool
isEnvironmentCoherent [] _ = True
isEnvironmentCoherent list a = case filter (\y -> ename y == ename a) list of
    [] -> True
    [x] -> True
    _ -> False

findAbsoluteValue :: StackV -> [Environment] -> Either String ((Stack, [Environment]), CodeReader)
findAbsoluteValue (StackEProcedure e) env = case (isEnvironmentCoherent env e, value e) of
    (True, Just (Right stackv)) -> Right ((Stack [stackv], []), NormalRead)
    -- Enter in function code with is own stack
    (True, Just (Left code')) -> case runEvaluator (evalCode code') (StackState (Stack []) env code') of
        Right (StackState ret env' _, readMode) -> Right ((ret, env'), readMode)
        Left x -> Left x
    (False, _) -> Left "Your code contains two environments with the same name, please fix it"
    (_ ,Nothing) -> Right ((Stack [StackEProcedure e], []), NormalRead) -- A verifier si on produit une erreur ou pas
findAbsoluteValue (StackStruct (struct, attr)) env = case find (\y -> ename y == attr) (nextenv struct) of
    Just eattr -> findAbsoluteValue (StackEProcedure eattr) env
    _ -> Left "Attribute not found"
findAbsoluteValue (StackArrayIndex e idx) env = case findAbsoluteValue (StackEProcedure e) env of
    Right ((Stack [StackArray arr], _), _) -> if idx < length arr
        then Right ((Stack [arr!!idx], []), NormalRead)
        else Left ("Index out of bounds: " ++ ename e ++ "[" ++ show idx ++ "]")
    Right ((Stack [StackString str], _), _) -> if idx < length str
        then Right ((Stack [StackString [str!!idx]], []), NormalRead)
        else Left ("Index out of bounds: " ++ ename e ++ "[" ++ show idx ++ "]")
    _ -> Left "Array not found"
findAbsoluteValue value _ = Right ((Stack [value], []), NormalRead)

collectOnlyGlobal :: [Environment] -> [Environment]
collectOnlyGlobal [] = []
collectOnlyGlobal (e@(Environment _ (VFunc (VGlobal n)) _ _):xs) = e:collectOnlyGlobal xs
collectOnlyGlobal (e@(Environment _ (VGlobal n) _ _):xs) = e:collectOnlyGlobal xs
collectOnlyGlobal (x:xs) = collectOnlyGlobal xs

------------ EVALUATOR ------------

evalOperation :: StackIST -> Evaluator StackState
-- Faire gaffe le (x:xs) laisse des problems de pattern matching
evalOperation Not = Evaluator $ \(StackState (Stack stack) env code) -> case stack of
    (StackBool b:xs) -> Right (StackState (Stack (StackBool (not b):xs)) env code, NormalRead)
    (StackNumber i:xs) -> Right (StackState (Stack (StackBool (i == 0):xs)) env code, NormalRead)
    _ -> Left "Not a valid value"
evalOperation (FileOperation 0) = Evaluator $ \(StackState (Stack stack) env code) -> case stack of
    (StackString s:xs) -> unsafePerformIO $ do
        directory <- getCurrentDirectory
        fileexist <- doesFileExist (directory ++ "/" ++ s)
        if fileexist then do
            content <- readFile (directory ++ "/" ++ s)
            return (Right (StackState (Stack (StackString content:xs)) env code, NormalRead))
        else return (Left "File not found")
    _ -> Left "Not a string"
evalOperation (FileOperation 1) = Evaluator $ \(StackState (Stack stack) env code) -> case stack of
    (StackString s:StackString value:xs) -> unsafePerformIO $ do
        directory <- getCurrentDirectory
        writeFile (directory ++ "/" ++ s) value
        return (Right (StackState (Stack xs) env code, NormalRead))
    _ -> Left "Stack doesn't have enough elements"
evalOperation Misc = Evaluator $ \st -> unsafePerformIO $ do
    miscJigot
    return $ Right (st, NormalRead)
-- To FIX: if the list has only one element, it will return an error
evalOperation (OP n) = Evaluator $ \ st@(StackState (Stack stack) env code) -> case stack of
    (x@StackEProcedure{}:StackEProcedure e':xs) -> Left ("Variable not found " ++ ename e')
    (x:x':xs) -> case doOp (OP n) x x' of
        Just res -> Right (StackState (Stack (res:xs)) env code, NormalRead)
        Nothing -> case (n, x') of
            ("/", StackNumber 0) -> Left "Division by zero"
            ("/", StackFloat 0) -> Left "Division by zero"
            ("%", StackNumber 0) -> Left "Modulo by zero"
            ("@", _) -> Left "Invalid index for operation"
            _ -> Left "Not valid parameters for operation"
    _ -> Left "Stack doesn't have enough elements"
evalOperation op = Evaluator $ \st -> Left ("Invalid operation: " ++ show op)

-- TO DO: Make it work
evalISTAssign :: StackState -> Either String (StackState, CodeReader)
evalISTAssign (StackState (Stack (StackStruct (eng, x'):x'':xs)) env code) = case find (\y -> ename y == x') (nextenv eng) of
    Just sy -> case findAbsoluteValue x'' env of
        Right ((Stack [result], _), readMode) -> if isTypeCoherent (retype sy) result
            then Right (StackState (Stack xs) (eng { nextenv = sy {value = Just (Right result)}:delete sy (nextenv eng)}:delete eng env) code, NormalRead)
            else Left ("Type mismatch (1): expected " ++ show (retype sy) ++ " but got " ++ show x'')
        _ -> Left ("Error: when trying to assign a value to struct" ++ show (ename eng))
    _ -> Left ("Error: when trying to access a struct attribute: " ++ x' ++ " in struct " ++ show (ename eng))
-- Find the array in environment and replace it with at index idx the new value
evalISTAssign (StackState (Stack (StackArrayIndex e idx:x'':xs)) env code) = case (findAbsoluteValue (StackEProcedure e) env, findAbsoluteValue x'' env) of
    (Right ((Stack [StackArray (a':as)], _), _), Right ((Stack [res], _), _)) -> case (idx < length (a':as), getTypeLinked a' == getTypeLinked res) of
        (True, True) -> Right (StackState (Stack xs) (e { value = Just (Right (StackArray ((element idx .~ res) (a':as))))}:delete e env) code, NormalRead)
        _ -> Left ("Index out of bounds: " ++ ename e ++ "[" ++ show idx ++ "]")
    (Right ((Stack [StackString str], _), _), Right ((Stack [StackString [letter]], _), _)) -> if idx < length str
        then Right (StackState (Stack xs) (e { value = Just (Right (StackString ((element idx .~ letter) str)))}:delete e env) code, NormalRead)
        else Left ("Index out of bounds: " ++ ename e ++ "[" ++ show idx ++ "]")
    _ -> Left "Array not found / Array is Empty / Invalid Expression"
evalISTAssign (StackState (Stack (StackEProcedure e:StackEProcedure x':xs)) env code) = if getAbsType (retype e) == getAbsType (retype x')
    then Right (StackState (Stack xs) (e {nextenv = nextenv x', value = value x'}:delete e env) code, NormalRead)
    else Left ("Type mismatch (2): expected " ++ show (retype e) ++ " but got " ++ show x')
evalISTAssign (StackState (Stack (StackEProcedure eng:StackArrayIndex x' idx:xs)) env code) = case findAbsoluteValue (StackArrayIndex x' idx) env of
    Right ((Stack [v'], _), _) -> Right (StackState (Stack xs) (eng {value = Just (Right v')}:delete eng env) code, NormalRead)
    _ -> Left "Array not found / Array is Empty / Invalid Expression"
evalISTAssign (StackState (Stack (StackEProcedure eng:x':xs)) env code) = if isTypeCoherent (retype eng) x'
    then Right (StackState (Stack xs) (eng {value = Just (Right x')}:delete eng env) code, NormalRead)
    else Left ("Type mismatch (3): expected " ++ show (retype eng) ++ " but got " ++ show x')
evalISTAssign _ = Left "Invalid assignment"

evalISTCall :: StackState -> Either String (StackState, CodeReader)
-- Call findAbsoluteValue on each xs element to get the value of the procedure and then call evalISTBasic with the procedure
evalISTCall st@(StackState (Stack (StackProcedure Set:xs)) env code) = evalISTAssign (StackState (Stack xs) env code)
evalISTCall st@(StackState (Stack (StackProcedure (OP "@"):xs)) env code) = case xs of
    (StackEProcedure l:n:xs') -> case findAbsoluteValue n env of
        Right ((Stack [StackNumber n], _), _) -> Right (StackState (Stack (StackArrayIndex l n:xs')) env code, NormalRead)
        _ -> Left "Not a number"
    _ -> Left "Invalid parameters for @ operation"
evalISTCall st@(StackState (Stack (StackProcedure Dot:xs)) env code) = case xs of
    (StackEProcedure l:StackProcedure p:xs') -> case findAbsoluteValue (StackEProcedure l) env of
        Right ((Stack [x], _), _) -> case evalExpression p (StackState (Stack (x:xs')) env code) of
            Right (nstack, Just ndata) -> Right (StackState (Stack nstack) (l { value = Just (Right ndata)}:delete l env) code, NormalRead)
            Right (nstack, Nothing) -> Right (StackState (Stack nstack) env code, NormalRead)
            Left x -> Left x
        _ -> Left "Not a valid data"
    (e:StackProcedure p:xs') -> case evalExpression p (StackState (Stack (e:xs')) env code) of
            Right (nstack, _) -> Right (StackState (Stack nstack) env code, NormalRead)
            Left x -> Left x
    _ -> Left "Invalid parameters for dor operation"
evalISTCall st@(StackState (Stack (StackProcedure fn:xs)) env code) = case traverse (`findAbsoluteValue` env) xs of
    Right sts -> runEvaluator (evalISTBasic fn) (StackState (foldl (\(Stack st') ((Stack st'', _), _)-> Stack $ st' ++ st'') (Stack []) sts) env code)
    Left x -> Left x
-- Function a retravailler (au lieu d'ajouter env il faudrait remplacer si exsistant)
evalISTCall (StackState (Stack (StackEProcedure e':xs)) env code) = case traverse (`findAbsoluteValue` env) xs of
    Right sts -> case createSubEnv (collectOnlyGlobal env) (nextenv e') (foldl (\stackv (Stack stackv2, _) -> stackv ++ stackv2) [] (fmap fst sts)) of
        Just (newenv, stnext) -> case findAbsoluteValue (StackEProcedure e') newenv of
            Right (_, ExitRead code) -> Right (StackState (Stack []) [] (Code [(Exit, -1)]), ExitRead code)
            Right ((Stack [ret], env'), ReturnRead) -> if isTypeCoherent (retype e') ret then Right (StackState (Stack (ret:stnext)) (editGlobalEnv env env') code, NormalRead) else Left ("Type error: got " ++ show (getTypeLinked ret) ++ ", but expected: " ++ show (retype e'))
            Right ((Stack [], env'), _) -> if retype e' == VVoid || retype e' == VGlobal VVoid || retype e' == VFunc (VGlobal VVoid) || retype e' == VFunc VVoid then Right (StackState (Stack stnext) (editGlobalEnv env env') code, NormalRead) else Left "Type error: not a void function"
            Right (_, _) -> Left ("Function (" ++ show (ename e') ++ ") did not return a value")
            Left x -> Left x
        Nothing -> Left ("Missing or wrong arguments for function: " ++ ename e')
    Left x -> Left x
evalISTCall _ = Left "You call an undefined function/procedure"

{- debug :: Evaluator StackState -> Evaluator StackState
debug (Evaluator f) = Evaluator $ \ st -> case f st of
        Right (st', readMode) -> unsafePerformIO $ do
            putStrLn $ "\n--------------"
            putStrLn $ ""
            putStrLn $ "Env: " ++ show (env st')
            putStrLn $ ""
            putStrLn $ "Code: " ++ show (code st')
            putStrLn $ ""
            putStrLn $ "Stack: " ++ show (stack st')
            putStrLn $ ""
            putStrLn $ "Read mode: " ++ show readMode
            putStrLn $ "--------------\n"
            return (Right (st', readMode))
        Left x -> Left x -}

-- Le refaire en sorte pour que ça retourne (valeur, new array / string)
-- Return: Error or (Next stack value, new array)
evalExpression :: StackIST -> StackState -> Either String ([StackV], Maybe StackV)
evalExpression (Expression "len") (StackState (Stack (StackString x:xs)) env code) = Right (StackNumber (length x):xs, Nothing)
evalExpression (Expression "len") (StackState (Stack (StackArray x:xs)) env code) = Right (StackNumber (length x):xs, Nothing)
evalExpression (Expression "append") (StackState (Stack (StackString s:StackString s':xs)) env code) = Right (StackString (s ++ s'):xs, Just (StackString (s ++ s')))
evalExpression (Expression "append") (StackState (Stack (StackArray a:a':xs)) env code) = Right (StackArray (a ++ [a']):xs, Just (StackArray (a ++ [a'])))
evalExpression (Expression "tail") (StackState (Stack (StackString (_:str):xs)) env code) = Right (StackString str:xs, Just (StackString str))
evalExpression (Expression "tail") (StackState (Stack (StackArray (_:arr):xs)) env code) = Right (StackArray arr:xs, Just (StackArray arr))
evalExpression (Expression "popl") (StackState (Stack (StackString (x':rest):xs)) env code) = Right (StackString [x']:xs, Just (StackString rest))
evalExpression (Expression "popl") (StackState (Stack (StackArray (x':rest):xs)) env code) = Right (x':xs, Just (StackArray rest))
evalExpression (Expression "popr") (StackState (Stack (StackString str:xs)) env code) = Right (StackString [last str]:xs, Just (StackString (init str)))
evalExpression (Expression "popr") (StackState (Stack (StackArray arr:xs)) env code) = Right (last arr:xs, Just (StackArray (init arr)))
evalExpression exp st = Left ("Invalid expression: " ++ show exp ++ " with stack: " ++ show (stack st))

evalISTBasic :: StackIST -> Evaluator StackState
evalISTBasic (Scope ists e') = Evaluator $ \ st -> case runEvaluator (evalCode (Code ists)) (StackState (stack st) (e'++env st) (Code ists)) of
    Right (st', ScopeRead) -> Right (st' {env = editEnv (env st) (env st'), code = code st'}, NormalRead)
    Right (st', mmode) -> Right (st {stack = stack st', env = editEnv (env st) (env st')}, mmode)
    Left x -> Left x
evalISTBasic (Push x) = Evaluator $ \ (StackState (Stack stack) env code) -> Right (StackState (Stack (x:stack)) env code, NormalRead)
evalISTBasic (Get x) = Evaluator $ \ (StackState (Stack stack) env code) -> case getBuiltIn x of
    Just x -> Right (StackState (Stack (StackProcedure x:stack)) env code, NormalRead)
    Nothing -> case find (\x' -> ename x' == x) env of
        Just x@Environment{} -> Right (StackState (Stack (StackEProcedure x:stack)) env code, NormalRead)
        Nothing -> Left ("Variable not found: " ++ x)
evalISTBasic (GetAttr sname attribute) = Evaluator $ \ (StackState (Stack stack) env code) -> case find (\y -> ename y == sname) env of
    Just y@Environment{} -> case find (\y' -> ename y' == attribute) (nextenv y) of
        Just _ -> Right (StackState (Stack (StackStruct (y, attribute):stack)) env code, NormalRead)
        Nothing -> Left ("Attribute not found: " ++ attribute ++ ", in " ++ sname)
    Nothing -> Left ("Structure not found: " ++ sname)
evalISTBasic Print = Evaluator $ \ st@(StackState (Stack stack) env code) -> case stack of
    (StackProcedure _:xs) -> Left "Can't print a procedure"
    (StackString "":xs) -> Right (StackState (Stack xs) env code, PrintRead "(empty)")
    (StackEProcedure (Environment _ _ _ (Just (Right (StackString "")))):xs) -> Right (StackState (Stack xs) env code, PrintRead "(empty)")
    (StackEProcedure (Environment _ _ _ Nothing):xs) -> Right (StackState (Stack xs) env code, PrintRead "(null)")
    (x:xs) -> Right (StackState (Stack xs) env code, PrintRead (show x))
    _ -> Left "Nothing to print"
evalISTBasic Return = Evaluator $ \ (StackState (Stack list) env _) -> Right (StackState (if not (null list) then Stack [head list] else Stack []) env (Code [(Return, -1)]), ReturnRead)
evalISTBasic (Collect n) = Evaluator $ \ (StackState (Stack stack) env code) -> if length stack >= n
    then Right (StackState (Stack (StackArray (take n stack):drop n stack)) env code, NormalRead)
    else Left "Not enough elements on the stack"
evalISTBasic CallFn = Evaluator $ \ st -> evalISTCall st
evalISTBasic Exit = Evaluator $ \ (StackState (Stack stack) _ _) -> if not (null stack)
    then case head stack of
        StackNumber x -> Right (StackState (Stack []) [] (Code [(Exit, -1)]), ExitRead x)
        _ -> Left "Exit code must be a number"
    else Left "Nothing to exit"
evalISTBasic (JumpIfFalse rel) = Evaluator $ \ st@(StackState (Stack stack) _ code) -> if not (null stack)
    then case (head stack == StackBool False, getStackISTAt rel code) of
        (True, Just x) -> Right ((st {stack = Stack (tail stack)}), JumpRead x)
        (True, _) -> Right (st {stack = Stack (tail stack)} , ScopeRead)
        _ -> Right ((st {stack = Stack []}), NormalRead)
    else Left "Nothing to jump on"
evalISTBasic (Jump rel) = Evaluator $ \ st@(StackState _ _ (Code code)) -> case getStackISTAt rel (Code code) of
    Just x -> Right (st, JumpRead x)
    Nothing -> if rel > getLastIndex code then Left "Jump out of range" else Left ("Invalid Jump (" ++ show rel ++ ")")
evalISTBasic Set = Evaluator $ \ st -> evalISTAssign st
evalISTBasic ist = evalOperation ist

-- Trouver un autre moyen que ça
evalCode :: Code -> Evaluator StackState
evalCode (Code []) = Evaluator $ \ st -> Right (st, NormalRead)
evalCode (Code ((x,_):xs)) = Evaluator $ \ st -> case runEvaluator ({- debug $  -}evalISTBasic x) st of
        Right (st', NormalRead) -> runEvaluator (evalCode (Code xs)) st'
        Right (st', JumpRead ncode) -> runEvaluator (evalCode ncode) st'
        Right (st', ReturnRead) -> Right (st', ReturnRead)
        Right (st', ScopeRead) -> Right (st'{code = Code xs}, ScopeRead)
        Right (st', ExitRead x) -> Right (st', ExitRead x)
        Right (st', PrintRead x) -> unsafePerformIO $ do
            let toprint = init . tail . show $ x
            putStrLn toprint
            return $ runEvaluator (evalCode (Code xs)) st'
        Left x -> Left x
