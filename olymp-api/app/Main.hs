module Main where

import Olymp (makeApplication, parseArgs, run)

main :: IO ()
main = uncurry run =<< makeApplication =<< parseArgs
