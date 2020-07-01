module Main where

import CLI

main :: IO ()
main = do
    conf <- cli
    ast <- parseFile conf
    run ast
