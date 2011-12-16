{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Data.UString hiding (find, sort, putStrLn)
import Database.MongoDB.Connection
import Control.Monad.Trans (liftIO)

import Data.Typeable
import Data.Bson
import Database.MongoDB.Structured
import Database.MongoDB.Structured.Deriving.TH
import Database.MongoDB.Structured.Query

data Location = Location { locId  :: SObjId
                         , city   :: String
                         , state  :: String
                         } deriving(Show, Read, Eq, Ord, Typeable)
$(deriveStructured ''Location)

data Team = Team { teamId :: SObjId
                 , name   :: String
                 , home   :: Location
                 , league :: String
                 } deriving(Show, Read, Eq, Ord, Typeable)
$(deriveStructured ''Team)


clearTeams = delete (select [] (u "Team"))

insertTeams = insertMany [
  Team { teamId = noSObjId
       , name   = "Yankees"
       , home   = Location { locId = noSObjId, city = "New York", state = "NY" }
       , league = "Nation" },
  Team { teamId = noSObjId
       , name   = "Mets"
       , home   = Location { locId = noSObjId, city = "New York", state = "NY" }
       , league = "Nation" }
  ]

findTeams = find (select [] (u "Team")) >>= rest

printDocs title docs = liftIO $ do
  putStrLn title
  let f :: Document -> Maybe Team
      f = fromBSON
  mapM_ (print . f) docs

run = do
   clearTeams
   insertTeams
   docs <- findTeams
   printDocs "National League Teams" docs

main = do
   pipe <- runIOE $ connect (host "127.0.0.1")
   e <- access pipe master (u "baseball") run
   close pipe
   print e

fun = 
  Team { teamId = noSObjId
       , name   = "Yankees"
       , home   = Location { locId = noSObjId, city = "New York", state = "NY" }
       , league = "Nation" }
