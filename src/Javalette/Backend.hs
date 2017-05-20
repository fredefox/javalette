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
import Control.Monad.Shade

import Javalette.Syntax (Prog)
import qualified Javalette.Backend.Internals as I
import qualified Javalette.Options as StdOpts

type Compilation = FilePath -> Prog -> IO ()

-- | A compiler backend.
type Backend = ShadeT Parser Compilation

-- | Create a backend from its internal representation.
backend :: I.Backend -> Backend
backend (I.Backend run optsP enable) = shade ((,) <$> optsP <*> switch enable)
  -- NOTE disabled because the test-runner does not pass this flag to the
  -- compiler.
  $ \(opts, b) f p -> {- when b -} (run opts f p)

argparse :: Parser a -> IO a
argparse = execParser . (`info` StdOpts.programInfo) . (<**> helper)

stdopts :: ShadeT Parser a -> IO (StdOpts.StdArgs, a)
stdopts = shadow . transfer argparse . both (hide StdOpts.argsParser)

both :: Applicative f => f a -> f b -> f (a, b)
both a b = (,) <$> a <*> b

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
