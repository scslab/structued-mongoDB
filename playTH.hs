{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Database.MongoDB.Structured
import Database.MongoDB.Structured.Deriving.TH
import Data.Typeable
import Language.Haskell.TH
import Data.Bson

data User = User { userId :: SObjId
                 , userFirstName :: String
                 , userLastName :: String
                 }
            deriving(Show, Read, Eq, Ord, Typeable)
$(deriveStructured ''User)

data Profile = Profile { profileId       :: SObjId
                       , profileName     :: String
                       , profileUser     :: User
                       , profileKeywords :: [Maybe String]
                       }
              deriving(Show, Read, Eq, Ord, Typeable)
$(deriveStructured ''Profile)

prof1 :: Profile
prof1 = Profile { profileId       = noSObjId
                , profileName     = "Woo"
                , profileUser     = user1
                , profileKeywords = [Just "foo", Nothing, Just "bar"] 
                }

user1 :: User
user1 = User { userId = noSObjId , userFirstName = "w00t", userLastName = "pwn" }

main = do
  i <- genObjectId
  putStrLn $ "Collection "  ++ show (collection user1)
  putStrLn $ "To BSON "     ++ show (toBSON user1)
  putStrLn $ "FromTo BSON " ++ show (fromBSON (((u "_id") =: i) : (toBSON user1)) :: Maybe User)
  putStrLn $ "Collection "  ++ show (collection prof1)
  putStrLn $ "To BSON "     ++ show (toBSON prof1)
  putStrLn $ "FromTo BSON " ++ show (fromBSON (toBSON prof1) :: Maybe Profile)

