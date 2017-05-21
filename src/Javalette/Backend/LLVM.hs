{-# LANGUAGE ViewPatterns #-}
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

import Javalette.Backend ( Backend )
import qualified Javalette.Backend           as Backend
import qualified Javalette.Backend.Internals as Internal
import qualified Javalette.Syntax as Jlt
import qualified Javalette.Backend.LLVM.Language as LLVM
import Javalette.PrettyPrint hiding ((<>))
import Javalette.Backend.LLVM.CodeGenerator
  ( compileProg
  , CompilerErr
  )

backend :: Backend
backend = Backend.backend Internal.Backend
  { Internal.runBackend = compile
  , Internal.backendOptions = optParser
  , Internal.enable = long "llvm"
  }

data LLVMOpts = LLVMOpts
  { runtime :: FilePath
  }

optParser :: Parser LLVMOpts
optParser = LLVMOpts
  <$> strOption
    (  long "runtime"
    <> short 'r'
    <> metavar "RUNTIME"
    <> help "Path to files to link against"
    <> value "lib/runtime.bc"
    )

compile :: LLVMOpts -> FilePath -> Jlt.Prog -> IO ()
compile opts jltFp = ioStuff . compileProg
  where
    ioStuff :: Either CompilerErr LLVM.Prog -> IO ()
    ioStuff (Left e)  = putStrLnStdErr . prettyShow $ e
    ioStuff (Right p) = do
      let fp       = dropExtension jltFp
          assembly = prettyShow p
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
