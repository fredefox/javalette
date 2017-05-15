{- | A wrapper hiding details that only backends need to look at -}
module Javalette.Backend
  ( Javalette.Backend.Internals.Backend()
  , runBackend
  ) where

import Javalette.Syntax (Prog)
import Javalette.Backend.Internals hiding (runBackend)
import qualified Javalette.Backend.Internals as I (runBackend)

runBackend :: Backend -> FilePath -> Prog -> IO ()
-- runBackend
--   ( Backend Backend'
--     { I.runBackend = run
--     , backendOptions = optParser
--     }
--   ) fp p = do
--     opts <- execParser (info (optParser <**> helper) mempty)
--     run opts fp p
runBackend
  Backend
  { I.runBackend = run
  } = run undefined
