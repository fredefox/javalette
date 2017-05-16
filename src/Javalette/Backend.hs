{- | A wrapper hiding details that only backends need to look at -}
module Javalette.Backend
  ( Javalette.Backend.Internals.Backend()
  , runBackend
  ) where

import Options.Applicative

import Javalette.Syntax (Prog)
import Javalette.Backend.Internals hiding (runBackend)
import qualified Javalette.Backend.Internals as I (runBackend)
import Javalette.Options as StdOpts

runBackend :: Backend -> FilePath -> Prog -> IO ()
runBackend
  Backend
    { I.runBackend = run
    , backendOptions = optParser
    } fp p = do
    opts <- execParser optParser
    run (argsAdditionalArguments opts) fp p
