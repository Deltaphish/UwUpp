module Main where

import UwU.CLI

main :: IO ()
main = do
    conf <- cli
    ast <- parseFile conf
    run ast
