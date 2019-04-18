{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parseBF
  ) where

import Control.Monad

import Data.Either
import Data.Functor

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

import Text.Pretty.Simple (pPrint)

import System.IO

import Prelude
import Types

{-
parseBF :: String -> Either
parseBF str =
  case parse bfProgram "" str of
    (Left err) -> pPrint err
    (Right res) -> pPrint res
-}
parseBF :: String -> Either ParseError BFProgram
parseBF = parse bfProgram ""

bfProgram :: Parser BFProgram
bfProgram = do
  ops <- many bfOp
  return $ BFProgram ops

bfOp :: Parser BFOp
bfOp =
  moveLeft <|> moveRight <|> inc <|> dec <|> readStdIn <|> writeStdOut <|>
  bfLoop

moveLeft :: Parser BFOp
moveLeft = MoveLeft <$ satisfy (== '<')

moveRight :: Parser BFOp
moveRight = MoveRight <$ satisfy (== '>')

inc :: Parser BFOp
inc = Inc <$ satisfy (== '+')

dec :: Parser BFOp
dec = Dec <$ satisfy (== '-')

readStdIn :: Parser BFOp
readStdIn = ReadStdIn <$ satisfy (== ',')

writeStdOut :: Parser BFOp
writeStdOut = WriteStdOut <$ satisfy (== '.')

bfLoop :: Parser BFOp
bfLoop = do
  char '['
  ops <- many bfOp
  char ']'
  return $ BFLoop ops
