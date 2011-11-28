-- | This module exports a 'Structued' type class which can be used to
-- convert from Haskel \"record types\" to @BSON@ objects and vice versa.
-- Though users may provide their own definitions for record types, we
-- provide a Template Haskell function that can be used to
-- automatically do this. See "Database.MongoDB.Structured.Deriving.TH".
module Database.MongoDB.Structured ( Structured(..) ) where

import Data.Bson
import Database.MongoDB.Query (Collection)

class Structured a where
  collection :: a -> Collection            -- ^ Collection name is then name of type
  toBSON     :: a -> Document              -- ^ Convert record to a BSON object
  fromBSON   :: Monad m => Document -> m a -- ^ Convert BSON object to record
