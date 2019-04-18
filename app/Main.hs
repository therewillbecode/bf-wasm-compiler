module Main where

import Parser (parseBF)

import Transformer
import Types

main :: IO ()
main =
  case parseBF helloWorld of
    (Left err) -> print err
    (Right brainfckAST) -> print $ transformAST brainfckAST
  where
    str = "+[-[<<[+[--->]-[<<<]]]>>>-]>-.---.>..>.<<<<-.<+.>>>>>.>.<<.<-."
    helloWorld =
      "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
