module Ids
  ( generate
  ) where

import Control.Monad.IO.Class ( MonadIO(..) )
import qualified System.Random ( newStdGen, randomRs )
import qualified Data.Text

type Id = Data.Text.Text

generate :: (MonadIO m) => m Id
generate = do
  chars <- System.Random.randomRs ('a', 'z') <$> System.Random.newStdGen
  return $ Data.Text.pack . take 11 $ chars
