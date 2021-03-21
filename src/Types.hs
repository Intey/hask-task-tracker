{-# LANGUAGE Rank2Types #-}
module Types


where

import qualified Database.MongoDB       as Mongo

{-| Env declare computations in environment with result 'v' -}
newtype Env = Env {
  envRunDb :: forall v. Mongo.Action IO v -> IO v
}

