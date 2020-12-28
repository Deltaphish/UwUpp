module Main where

import UwU.CLI

main :: IO ()
main = do
    conf <- cli
    parseFile conf
