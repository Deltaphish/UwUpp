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
                           Right stmts -> print $ runProgram stmts
                           _ -> print "done"
            _ -> error "Too many arguments"