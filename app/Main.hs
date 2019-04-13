module Main where

import Parser

main :: IO ()
main = parseBf str
  where str = "<><[+[-]]<+-" 
