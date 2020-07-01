{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parser
import Interpreter
import Text.Megaparsec
import Data.Text
import System.Environment
import Control.Monad.Except

main :: IO ()
main = do
        args <- getArgs
        case args of
            [] -> error "No argument"
            [arg] -> do 
                       inpt <- readFile arg
                       case runParser pMain "" (pack inpt) of
                           Right stmts -> runExceptT (runProgram stmts) >>= eval
                           _ -> print "Error in parsing file"
            _ -> error "Too many arguments"

eval :: Either String () -> IO()
eval (Right _ ) = return ()
eval (Left e ) = print ("The intepreter found an error: " ++ e)  