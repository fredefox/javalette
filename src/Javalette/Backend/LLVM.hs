{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module Javalette.Backend.LLVM
  ( backend
  ) where

import System.IO
import System.FilePath
import System.Process

import Javalette.Backend.Internals
import qualified Javalette.Syntax as Jlt
import qualified Javalette.Backend.LLVM.Language as LLVM
import Javalette.PrettyPrint
import Javalette.Backend.LLVM.CodeGenerator
  ( compileProg
  , CompilerErr
  )

backend :: Backend
backend = Backend
  { compiler = compile
  }

compile :: FilePath -> Jlt.Prog -> IO ()
compile fp = ioStuff . compileProg
  where
    ioStuff :: Either CompilerErr LLVM.Prog -> IO ()
    ioStuff (Left e)  = putStrLnStdErr . prettyShow $ e
    ioStuff (Right p) = do
      let assembly = prettyShow p
      putStrLn assembly
      writeFile (fp <.> "llvm") assembly
      callLLVM  (fp <.> "llvm")

putStrLnStdErr :: String -> IO ()
putStrLnStdErr = hPutStrLn stderr

callLLVM :: FilePath -> IO ()
callLLVM f = do
  s <- readProcess llvmAssemblerCmd [f] []
  putStrLn s

llvmAssemblerCmd :: String
llvmAssemblerCmd = "/usr/bin/llvm-as-3.8"
