{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | This module exports a 'Structued' type class which can be used to
-- convert from Haskel \"record types\" to @BSON@ objects and vice versa.
-- Though users may provide their own definitions for record types, we
-- provide a Template Haskell function that can be used to
-- automatically do this. See "Database.MongoDB.Structured.Deriving.TH".
--
-- A record type is expected to have an \"_id\" of type 'SObjId',
-- which is set when performing a query.
--
module Database.MongoDB.Structured ( Structured(..)
                                   , SObjId(..)
                                   , noSObjId, isNoSObjId
                                   , toSObjId, unSObjId
                                   ) where

import Data.Bson
import Database.MongoDB.Query (Collection)
import Data.Typeable

class Structured a where
  collection :: a -> Collection     -- ^ Collection name is then name of type
  toBSON     :: a -> Document       -- ^ Convert record to a BSON object
  fromBSON   :: Document -> Maybe a -- ^ Convert BSON object to record

newtype SObjId = SObjId (Maybe ObjectId)
   deriving(Show, Read, Eq, Ord, Typeable, Val)

noSObjId :: SObjId
noSObjId = SObjId Nothing

isNoSObjId :: SObjId -> Bool
isNoSObjId = (==) noSObjId

unSObjId :: SObjId -> ObjectId
unSObjId (SObjId (Just x)) = x
unSObjId _ = error "invalid use"

toSObjId :: ObjectId -> SObjId
toSObjId = SObjId . Just
