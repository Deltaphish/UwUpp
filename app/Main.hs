{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parser
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
                       parseTest pMain (pack inpt)
            _ -> error "Too many arguments"