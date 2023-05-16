--
-- EPITECH PROJECT, 2022
-- glados
-- File description:
-- Core
--

module Core (
        compile
) where

import JigotParser
import JigotAST
import JigotASM
import JigotEval

import Data.List

-- Either error or exit code
compile :: [Environment] -> String -> Either String (Maybe StackV, [Environment])
compile env line = case runParser (parseMultiple 0) line of
        Left _ -> Left "Error: Compiler was unable to parse the code (Parser), reason: unexpected start of input"
        Right (cpt, "") -> case cptToAST cpt of
            Nothing -> Left "Error: Compiler found invalid expression (CPT->AST)"
            Just ast -> case runCompiler (astToStackIST ast 0) env of
                Left x -> Left ("Error: Compiler was unable to compile the code (AST->IST), reason: " ++ x)
                Right (code, env') -> case find (\y -> ename y == "_main") env' of
                    Nothing -> Left "Error: reference indefinie vers main" -- case when we doesn't have a main
                    Just _ -> case runEvaluator (evalCode code) (StackState (Stack []) env' code) of
                        Right (StackState (Stack [ret]) env'' _, ReturnRead) -> Right (Just ret, env'')
                        Right (_, ExitRead n) -> Right (Just (StackNumber n), [])
                        Right (_, _) -> Left "Error: Evaluator was unable to execute the code (IST->Stack), reason: Invalid return value"
                        Left x -> Left ("Error: Evaluator was unable to execute the code (IST->Stack), reason: " ++ x)
        Right (_, _) -> Left "Error: Compiler was unable to parse the code (Parser), reason: unexpected end of input"
