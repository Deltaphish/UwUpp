{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parser
import Interpreter
import Text.Megaparsec
import Data.Text
import System.Environment

main :: IO ()
main = do
        args <- getArgs
        case args of
            [] -> error "No argument"
            [arg] -> do 
                       inpt <- readFile arg
                       case runParser pMain "" (pack inpt) of
                           Right stmts -> runProgram stmts
                           _ -> print "Error in parsing file"
            _ -> error "Too many arguments"