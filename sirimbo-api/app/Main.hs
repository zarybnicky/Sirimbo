module Main where

import Olymp.Cli (parseCli)

main :: IO ()
main = do
  (config, command) <- parseCli
  command config

