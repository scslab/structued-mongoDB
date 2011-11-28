{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Database.MongoDB.Structured.Deriving.Generics where

import Prelude 
import qualified Prelude as Prelude
import Data.Typeable
import Data.Bson
import Database.MongoDB.Query (Collection)
import GHC.Generics



class Structured t where
  collection :: t -> Collection -- ^ Collection name is then name of type
  toBSON     :: t -> Document   -- ^ Convert record to a BSON object

  default collection :: (Generic t, Structured' (Rep t)) => t -> Collection
  collection = collection' . from
  default toBSON :: (Generic t, Structured' (Rep t)) => t -> Document
  toBSON = toBSON' . from


class Structured' f where
  collection' :: f t -> Collection -- ^ Collection name is then name of type
  toBSON'     :: f t -> Document   -- ^ Convert to BSON object
  collection' = error "Invalid use of collection'"

-- Meta-data
instance (Structured' p, Datatype d) => Structured' (D1 d p) where
  collection' d = u $ datatypeName d
  toBSON' d@(M1 x) = toBSON' x

instance (Structured' p, Constructor c) => Structured' (C1 c p) where
  toBSON' (M1 x) = toBSON' x 

instance (Structured' p, Selector s) => Structured' (S1 s p) where
  toBSON' s@(M1 x) = [(u $ selName s) := (value . head . toBSON' $ x)]

instance Val a => Structured' (K1 i a) where
  toBSON' (K1 x) = [(u "INVALID") =: x]

-- Data
instance Structured' U1 where
  toBSON' U1 = []

instance (Structured' p1, Structured' p2) => Structured' (p1 :*: p2) where
  toBSON' (x :*: y) = toBSON' x ++ toBSON' y

instance (Structured' p1, Structured' p2) => Structured' (p1 :+: p2) where
  toBSON' = error "Sums are not supported"
--


data User = User { userId :: Int
                 , userFirstName :: String
                 , userLastName :: String
                 }
            deriving(Generic, Show, Read, Eq, Ord, Typeable)
instance Structured User

instance Val User where
  val u = val $ toBSON u
  cast' = undefined


data Profile = Profile { profileId   :: Int
                       , profileName :: String
                       , profileUser :: User
                       }
              deriving(Generic, Show, Read, Eq, Ord, Typeable)
instance Structured Profile

doc :: Document
doc = [ (u "profileId") =: (42 :: Int) , (u "profileName") =: "Woo" ]
undoc :: Profile
undoc = Profile { profileId = 42 , profileName = "Woo", profileUser = user1}

user1 :: User
user1 = User { userId = 1337 , userFirstName = "w00t", userLastName = "pwn" }

main = do
  putStrLn $ "Collection " ++ show (collection undoc)
  putStrLn $ "BSON " ++ show (toBSON undoc)

{-
NOTES:
It's not clear that defining fromBSON ::  Document -> t is so straight
forward.
-}
