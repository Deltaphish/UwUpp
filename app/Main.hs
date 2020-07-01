{-# LANGUAGE OverloadedStrings #-}
module Main where

import Runtime
import Parser
import Interpreter
import AST
import Text.Megaparsec
import qualified Data.Text.IO as TIO
import System.Environment
import Control.Monad.Except


main :: IO ()
main = do
        args <- getArgs
        case args of
            [] -> error "No argument"
            [arg] -> do 
                stmtsM <- parseFile arg
                case stmtsM of
                    Just stmts -> do 
                        result <- runExceptT $ runProgram stmts
                        reportResult Clear result
                    Nothing -> putStrLn "Error parsing file"
            _ -> error "Too many arguments"


parseFile :: String -> IO( Maybe [Stmt] )
parseFile f = do
    contents <- TIO.readFile f
    let ast = runParser pMain f contents
    case ast of
        Right stmts -> return $ Just stmts
        _           -> return $ Nothing

