{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Javalette.Backend.LLVM
  ( backend
  ) where

import System.IO
import System.FilePath
import System.Process
import Control.Monad

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
      writeFile  (fp <.> "ll") assembly
      doAssemble (fp <.> "ll")
      -- let linked = fp ++ "-linked" <.> "bc"
      -- doLink     [fp <.> "bc", runtime] (linked <.> "bc")
      -- doLlc (linked <.> "bc")
      -- doCompile (linked <.> "o")

runtime :: FilePath
runtime = "lib/runtime.bc"

putStrLnStdErr :: String -> IO ()
putStrLnStdErr = hPutStrLn stderr

doAssemble :: FilePath -> IO ()
doAssemble f = echoCommand (readProcess llvmAssemblerCmd [f] [])

echoCommand :: IO String -> IO ()
echoCommand cmd = cmd >>= putStrLn

llvmAssemblerCmd :: String
llvmAssemblerCmd = "llvm-as-3.8"

llvmLinkCmd :: String
llvmLinkCmd = "llvm-link-3.8"
-- llvmLinkCmd = "echo"

llvmCompileCmd :: String
llvmCompileCmd = "llc-3.8"

doLink :: [FilePath] -> FilePath -> IO ()
doLink fs out = echoCommand (readProcess llvmLinkCmd (fs ++ ["-o", out]) [])

compileCmd :: String
compileCmd = "clang"

doLlc :: FilePath -> IO ()
doLlc f = echoCommand (readProcess llvmCompileCmd [f, "-filetype=obj"] [])

doCompile :: FilePath -> IO ()
doCompile f = echoCommand (readProcess compileCmd [f] [])
