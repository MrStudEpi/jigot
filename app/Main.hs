--
-- EPITECH PROJECT, 2022
-- glados
-- File description:
-- Main
--

module Main (main) where

import System.IO
import System.Exit
import Core
import JigotASM
import JigotEval (miscJigot)

import System.Environment
import Data.List
import Control.Monad
import System.FilePath.Posix
import System.Directory

data Flag = Flag {
    fname :: String,
    shortname :: String,
    description :: String
}

data Mode = Normal | Multine | LSD
    deriving (Eq)

data Parameter = Parameter String Int [Environment] Mode

dparameter :: Parameter
dparameter = Parameter "" 0 [] Normal

flags :: [Flag]
flags = [
    Flag "--help" "-h" "USAGE: ./glados < [files] [flags]",
    Flag "--interpreter" "-i" "Interpret code in a Jigot style",
    Flag "--compile" "-c" "Compile code in a Jigot style",
    Flag "--execute" "-e" "Execute a .jg file"
    ]

printEnv :: [Environment] -> IO ()
printEnv [] = return ()
printEnv (Environment name vtype subenv value:xs) = do
    putStrLn "-----"
    putStrLn (name ++ "(" ++ show vtype ++ ") = " ++ show value)
    printEnv subenv
    putStrLn "-----"
    printEnv xs

printHelp :: IO ()
printHelp = do
    putStrLn "COMMAND:"
    putStrLn ""
    putStrLn ":h - Print this help"
    putStrLn ":q - Quit the interpreter"
    putStrLn ":e - Print the environment"
    putStrLn ":r - Run the code in the current buffer"
    putStrLn ":c - Clear the buffer"
    putStrLn ":l - Load a file"
    putStrLn ":s - Show the buffer"
    putStrLn ":ec - Clear the environment"
    putStrLn ":m - Toggle the multiline mode"
    putStrLn ":jigot - Print information about the Jigot language"
    putStrLn ""
    putStrLn "MISC:"
    putStrLn ""
    putStrLn "What's your favorite food ? (Interpreter Only) - Print until sigint the language LOGO"
    putStrLn "\"jigot\" import (In Code) - Print one time the language LOGO"

doCompile :: Parameter -> String -> IO ()
doCompile (Parameter _ _ env mmode) temp = case compile env temp of
    Left x -> putStrLn (show x) >> exeInterpreter (Parameter "" 84 env mmode)
    Right (res, env') -> case res of
        Just (StackNumber n) -> exeInterpreter (Parameter "" n env' mmode)
        Just _ -> exeInterpreter (Parameter "" 84 env' mmode)
        Nothing -> exeInterpreter (Parameter "" 84 env' mmode)

exeInterpreter :: Parameter -> IO()
exeInterpreter p@(Parameter ex ret env mmode) = do
    when (mmode == Normal) $ do
        putStr ("jigot " ++ show ret ++ "$> ")
        hFlush stdout
    when (mmode == Multine) $ do
        done <- isEOF
        when done $ exeInterpreter (Parameter ex 0 env Normal)
    when (mmode == LSD) $ miscJigot >> exeInterpreter p
    str <- getLine
    let temp = ex ++ str
    when (mmode == Multine) $ exeInterpreter (Parameter (temp ++ "\n") ret env mmode)
    when (mmode == Normal) $ case str of
        "What's your favorite food ?" -> exeInterpreter (Parameter "" ret env LSD)
        (':':xs) -> case xs of
            "h" -> printHelp >> exeInterpreter (Parameter ex 0 env mmode)
            "s" -> putStrLn ex >> exeInterpreter (Parameter ex 0 env mmode)
            "c" -> exeInterpreter (Parameter "" 0 env Normal)
            ('r':xs) -> case ex of
                "" -> exeInterpreter (Parameter ex 0 env mmode)
                o -> doCompile p ex
            ('l':xs') -> do let file = dropWhile (== ' ') xs'
                            exist <- doesFileExist file
                            if exist then do
                                content <- readFile file
                                exeInterpreter (Parameter content 0 [] Normal)
                            else do
                                putStrLn ("File " ++ file ++ " not found")
                                exeInterpreter (Parameter ex 84 env mmode)
            "ec" -> exeInterpreter (Parameter ex 0 [] Normal)
            "m" -> exeInterpreter (Parameter "" 0 [] Multine)
            "e" -> printEnv env >> exeInterpreter (Parameter ex 0 env Normal)
            "jigot" -> miscJigot >> putStrLn "Version 1.0.0 - @Paul G, Paul R., Tom, Corentin (Jigot)" >> exeInterpreter p
            "q" -> exitWith (ExitSuccess)
            _ -> putStrLn "Unknown command" >> exeInterpreter (Parameter ex 84 env mmode)
        _ -> exeInterpreter p

toOneLine :: [String] -> String
toOneLine [] = ""
toOneLine (x:xs) = x <> "\n" <> toOneLine xs

exeGladosFiles :: [String] -> Int -> IO()
exeGladosFiles [] code = if code == 0 then exitSuccess else exitWith (ExitFailure code)
exeGladosFiles (x:xs) code = do
    f <- openFile x ReadMode
    c <- hGetContents f
    let ext = takeExtension x
    when (ext /= ".jg") $ do
        putStrLn ("Error: " ++ x ++ " is not a .jg file")
        exeGladosFiles xs 84
    when (null c) $ putStrLn "File is empty" >> exeGladosFiles xs code
    unless (null c) $ setCurrentDirectory (takeDirectory x) >> case compile [] (toOneLine (lines c)) of
        Left x -> putStr (show x) >> putStr "\n" >> exitWith (ExitFailure 84)
        Right (res, e) -> case res of
            Just (StackNumber n) -> exeGladosFiles xs n
            Just _ -> putStrLn "Error: Invalid return value" >> exitWith (ExitFailure 84)
            Nothing -> exitWith (ExitFailure 84)
    hClose f

execute :: Maybe Flag -> [String] -> IO()
execute (Just (Flag "--help" _ _ )) [x] = printUsage
execute (Just (Flag "--interpreter" _ _ )) [x] = exeInterpreter dparameter {- 
execute (Flag "--compile" _ _) file = exeComp file
execute (Flag "--execute" _ _) file = exeExe file -}
execute Nothing files = exeGladosFiles files 0
execute _ _ = printUsage

printUsage :: IO ()
printUsage = putStrLn "USAGE: ./glados < [files] to execute a file |" >>
            putStrLn "       ./glados -i or --interpreter : interpreter mode" >>
            putStrLn "       ./glados -c or --compile [file] : compile mode" >>
            putStrLn "       ./glados -e or --execute [file] : execute mode"

main :: IO ()
main = do
    args <- getArgs
    case args of
        (f:xs) -> execute (find (\x -> (fname x == f) || (shortname x == f)) flags) (f:xs)
        _ -> printUsage
