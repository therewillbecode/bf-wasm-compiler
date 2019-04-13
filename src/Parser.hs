{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parseBf
    ) where

import Control.Monad

import Data.Functor
import Data.Either

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import Text.Parsec

import System.IO

import Prelude

-- bf-program : (bf-op | bf-loop)*
-- bf-op : ">" | "<" | "+" | "-" | "." | ","
-- bf-loop : "[" (bf-op | bf-loop)* "]"

data Program = Program [Op] deriving Show

data Op = MoveRight | MoveLeft | Inc | Dec | ReadStdIn | WriteStdOut | Loop [Op] deriving Show

program :: Parser Program
program = do
       ops <- many op
       return $ Program ops

op :: Parser Op
op = moveLeft <|> moveRight <|> inc <|> dec <|> readStdIn <|> writeStdOut <|> loop

moveLeft :: Parser Op
moveLeft = MoveLeft <$ satisfy (== '<')

moveRight :: Parser Op
moveRight = MoveRight <$ satisfy (== '>')

inc :: Parser Op
inc = Inc <$ satisfy (== '+')

dec :: Parser Op
dec = Dec <$ satisfy (== '-')

readStdIn :: Parser Op
readStdIn = ReadStdIn <$ satisfy (== ',')

writeStdOut :: Parser Op
writeStdOut = WriteStdOut <$ satisfy (== '.')

loop :: Parser Op
loop = do 
    char '['
    ops <- many op
    char ']'
    return $ Loop ops

parseBf :: String -> IO ()
parseBf str = 
 case (parse program "" str) of
    (Left err) -> print err
    (Right res) -> print res
