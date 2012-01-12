{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | This module exports a 'Structued' type class which can be used to
-- convert Haskel \"record types\" to @BSON@ objects and vice versa.
-- As a Mongo document has an \"_id\" field, we impose the requirement
-- a record type have a field whose type is 'SObjId' (corresponding to
-- \"_id\").
module Database.MongoDB.Structured.Types ( Structured(..)
                                           -- * Structured \"_id\"
                                          , SObjId(..)
                                          , noSObjId, isNoSObjId
                                          , toSObjId, unSObjId
                                          ) where
import Database.MongoDB.Query (Collection)
import Data.Bson
import Data.Typeable

-- | Structured class used to convert between a Haskell record type
-- and BSON document.
class Structured a where
  collection :: a -> Collection     -- ^ Collection name is then name of type
  toBSON     :: a -> Document       -- ^ Convert record to a BSON object
  fromBSON   :: Document -> Maybe a -- ^ Convert BSON object to record

-- | Type corresponding to the \"_id\" field of a document in a
-- structured object.
newtype SObjId = SObjId (Maybe ObjectId)
   deriving(Show, Read, Eq, Ord, Typeable, Val)

-- | The \"_id\" field is unset.
noSObjId :: SObjId
noSObjId = SObjId Nothing

-- | Check if the \"_id\" field is unset.
isNoSObjId :: SObjId -> Bool
isNoSObjId = (==) noSObjId

-- | Get the \"_id\" field (assumes that it is set0.
unSObjId :: SObjId -> ObjectId
unSObjId (SObjId (Just x)) = x
unSObjId _ = error "invalid use"

-- | Set the \"_id\" field.
toSObjId :: ObjectId -> SObjId
toSObjId = SObjId . Just
