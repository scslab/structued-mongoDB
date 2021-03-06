{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.UString hiding (find, sort, putStrLn)
import Database.MongoDB.Connection
import Data.Maybe (fromJust)
import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)

import Data.Typeable
import Data.Bson
import Database.MongoDB.Structured
import Database.MongoDB.Structured.Deriving.TH
import Database.MongoDB.Structured.Query


data Address = Address { addrId :: SObjId
                       , streetNr :: Int
                       , streetName :: String
                       } deriving (Show, Read, Eq, Ord, Typeable)
$(deriveStructured ''Address)

data User = User { userId    :: SObjId
                 , firstName :: String
                 , lastName  :: String
                 , favNr     :: Int
                 , addr      :: Address
                 } deriving(Show, Read, Eq, Ord, Typeable)
$(deriveStructured ''User)

insertUsers = insertMany 
  [ User { userId = noSObjId
         , firstName = "deian"
         , lastName = "stefan"
         , favNr = 3
         , addr = Address { addrId = noSObjId
                          , streetNr = 123
                          , streetName = "Mission" }
         }
  
  , User { userId = noSObjId
         , firstName = "amit" 
         , lastName = "levy"
         , favNr = 42 
         , addr = Address { addrId = noSObjId
                          , streetNr = 42
                          , streetName = "Market" }
         }
  
  , User { userId = noSObjId
         , firstName = "david"
         , lastName = "mazieres"
         , favNr = 1337 
         , addr = Address { addrId = noSObjId
                          , streetNr = 821
                          , streetName = "Valencia" }
         }
  ]

run = do
   delete (select ( (.*) :: QueryExp User))
   insertUsers
   let query = (select (Addr .! StreetNr .== 123 .|| FavNr .>= 3))
                  { limit = 2
                  , sort = [asc FirstName]
                  , skip = 0 }
   liftIO $ print query
   users <- find query >>= rest
   liftIO $ printFunc users
    where printFunc users = forM_ users $ \u ->
            putStrLn . show $ (fromJust $ u :: User)

main = do
   pipe <- runIOE $ connect (host "127.0.0.1")
   e <- access pipe master "auth" run
   close pipe
   print e

