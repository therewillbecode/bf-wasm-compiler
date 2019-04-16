{-# LANGUAGE OverloadedStrings #-}

{-
   Transforms a BF AST to a Webassembly AST
-}
module Transformer where

import Control.Monad

import Data.Either
import Data.Functor

import Text.Pretty.Simple (pPrint)

import Prelude
import Types
