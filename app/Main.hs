module Main where

import System.IO

import CodeGenerator
import Parser

main :: IO ()
main =
  case parseBF helloWorld of
    (Left err) -> print err
    (Right brainfckAST) -> do
      let wasmAst = transformAST brainfckAST
      writeFile "bf.wat" (show wasmAst)
  where
    str = "+[-[<<[+[--->]-[<<<]]]>>>-]>-.---.>..>.<<<<-.<+.>>>>>.>.<<.<-."
    helloWorld =
      "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
