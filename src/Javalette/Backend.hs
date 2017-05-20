{-# LANGUAGE RankNTypes #-}
{- | A wrapper hiding details that only backends need to look at -}
module Javalette.Backend
  ( Backend()
  , backend
  , runBackend
  , runBackends
  ) where

import Options.Applicative
import Control.Monad

import Javalette.Syntax (Prog)
import qualified Javalette.Backend.Internals as I
import Javalette.Utils.Box
import qualified Javalette.Options as StdOpts

type Compilation = FilePath -> Prog -> IO ()

-- | A compiler backend.
type Backend = Box Parser Compilation

-- | Create a backend from its internal representation.
backend :: I.Backend -> Backend
backend (I.Backend run optsP enable) = box ((,) <$> optsP <*> switch enable)
  -- NOTE disabled because the test-runner does not pass this flag to the
  -- compiler.
  $ \(opts, b) f p -> {- when b -} (run opts f p)

argparse :: Parser a -> IO a
argparse = execParser . (`info` StdOpts.programInfo) . (<**> helper)

stdopts :: Box Parser a -> IO (StdOpts.StdArgs, a)
stdopts = unboxWith argparse . both (entrench StdOpts.argsParser)

-- | Run a backend. The function passed to is the function that parses a program
-- from a string.
runBackend :: Backend -> (String -> IO Prog) -> IO ()
runBackend b parse = do
  (StdOpts.Args fps , cpl) <- stdopts b
  fps `forM_` \fp -> do
    s <- readFile fp
    p <- parse s
    cpl fp p

-- | Run all backends in a combined context.
runBackends :: [Backend] -> (FilePath -> IO Prog) -> IO ()
runBackends bs = runBackend (mconcat bs)
