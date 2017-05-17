{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Javalette.Backend.LLVM
  ( backend
  -- Used internally
  , optParser
  ) where

import System.IO
import System.FilePath
import System.IO.Temp
import System.Process
import Options.Applicative
import Data.Semigroup
import Control.Monad

import Javalette.Backend.Internals
import qualified Javalette.Syntax as Jlt
import qualified Javalette.Backend.LLVM.Language as LLVM
import Javalette.PrettyPrint hiding ((<>))
import Javalette.Backend.LLVM.CodeGenerator
  ( compileProg
  , CompilerErr
  )
import qualified Javalette.Options as StdOpts

backend :: Backend
backend = Backend
  { runBackend = compile
  , backendOptions = optParser
  }

data LLVMOpts = LLVMOpts
  { runtime :: FilePath
  }

optParser :: ParserInfo (StdOpts.Args LLVMOpts)
optParser = StdOpts.parseArgsAdditional $ LLVMOpts
  <$> strOption
    (  long "runtime"
    <> short 'r'
    <> metavar "RUNTIME"
    <> help "Path to files to link against"
    <> value "lib/runtime.bc"
    )

-- I don't know how combine the parser defined by a backend with the main
-- parser. See [Agda.Compiler.Backend.parseBackendOptions] for inspiration.
--
-- [Agda.Compiler.Backend.parseBackendOptions]:
--   https://github.com/agda/agda/blob/master/src/full/Agda/Compiler/Backend.hs#L119
compile :: LLVMOpts -> FilePath -> Jlt.Prog -> IO ()
compile opts = compile' opts

compile' :: LLVMOpts -> FilePath -> Jlt.Prog -> IO ()
compile' opts fp = ioStuff . compileProg
  where
    ioStuff :: Either CompilerErr LLVM.Prog -> IO ()
    ioStuff (Left e)  = putStrLnStdErr . prettyShow $ e
    ioStuff (Right p) = do
      let assembly = prettyShow p
      putStrLn assembly
      writeFile  (fp <.> "ll") assembly
      doAssemble (fp <.> "ll")
      withSystemTempDirectory (fp ++ "-linked") $ \linked -> do
        let rt     = runtime opts
        doLink     [fp <.> "bc", rt] (linked <.> "bc")
        doLlc (linked <.> "bc")
        doCompile (linked <.> "o") fp

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
compileCmd = "gcc"

doLlc :: FilePath -> IO ()
doLlc f = echoCommand (readProcess llvmCompileCmd [f, "-filetype=obj"] [])

doCompile :: FilePath -> FilePath -> IO ()
doCompile obj out = echoCommand (readProcess compileCmd [obj, "-no-pie", "-o", out] [])
