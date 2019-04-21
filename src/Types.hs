{-# LANGUAGE OverloadedStrings #-}

{-
   Web assembly and BF types.
-}
module Types where

import Control.Monad

import Data.Either
import Data.Functor

import Text.Pretty.Simple (pPrint)

import Prelude

{----- Brainfck -----}
newtype BFProgram =
  BFProgram [BFOp]
  deriving (Show)

data BFOp
  = MoveRight
  | MoveLeft
  | Inc
  | Dec
  | ReadStdIn
  | WriteStdOut
  | BFLoop [BFOp]
  deriving (Show)

{---- Webassembly ----}
newtype WasmProgram =
  WasmProgram [WasmOp]

newtype VariableName =
  VariableName String

data WasmOp
  -- Control Instructions
  = Br Int
  | BrIf Int
  | Block VariableName
          [WasmOp]
  | Call VariableName
  | WasmLoop VariableName
             [WasmOp]
  -- Memory Instructions
  | LoadI32
  | StoreI32
  -- Variable instructions
  | GetGlobal VariableName
  | SetGlobal VariableName
  --Numeric Instructions
  | AddI32
  | SubI32
  | MulI32
  | EqzI32
  | ConstI32 Int
  -- Bitwise instructions
  | AndI32

instance Show WasmProgram where
  show (WasmProgram wasmOps) =
    "(module \n (memory 1) \n" ++
    wasmGlobal "ptr" ++
    wasmGlobal "currCellVal" ++
    wasmGetPtrFunc ++
    wasmMainFunc code ++ "\n(export \"get_ptr\" (func $get_ptr)) \n )"
    where
      code = unlines $ show <$> wasmOps

instance Show VariableName where
  show (VariableName var) = '$' : var

instance Show WasmOp where
  show (ConstI32 val) = "  i32.const " ++ show val
  show (GetGlobal var) = "  get_global " ++ show var
  show (SetGlobal var) = "  set_global " ++ show var
  show AddI32 = "  i32.add"
  show SubI32 = "  i32.sub"
  show MulI32 = "  i32.mul"
  show LoadI32 = "  i32.load"
  show StoreI32 = "  i32.store"
  show EqzI32 = "  i32.eqz"
  show AndI32 = "  i32.and"
  show (Call var) = "  call " ++ show var
  show (Br stackDepth) = "  br " ++ show stackDepth
  show (BrIf cond) = "  br_if " ++ show cond
  show (Block var ops) = "    (block  " ++ show var ++ blockOps ++ "    )"
    where
      blockOps = unlines $ show <$> ops
  show (WasmLoop var ops) =
    "\n      (loop " ++ show var ++ "\n  " ++ loopOps ++ "\n     br  0" ++ ")"
    where
      loopOps = unlines $ (++) "        " . show <$> ops

wasmGlobal name = "  (global $" ++ name ++ " (mut i32) (i32.const 0))\n"

wasmImport = "  (import \"js\" \"print\" (func $print))"

-- function that is exported so that JS can inspect ptr value
wasmGetPtrFunc = "  (func $get_ptr (result i32) \n    get_global $ptr \n  )\n\n"

wasmMainFunc body =
  "  (export \"main\" (func $main)) \n  (func $main\n" ++ body ++ " )"
