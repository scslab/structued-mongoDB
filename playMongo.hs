{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
import Data.UString hiding (find, sort, putStrLn)
import Database.MongoDB.Connection
import Control.Monad.Trans (liftIO)

import Data.Typeable
import Data.Bson
import Database.MongoDB.Structured
import Database.MongoDB.Structured.Deriving.TH
import Database.MongoDB.Structured.Query

data Location = Location { city   :: String
                         , state  :: String
                         } deriving(Show, Read, Eq, Ord, Typeable)
$(deriveStructured ''Location)

data Team = Team { name   :: String
                 , home   :: Location
                 , league :: String
                 } deriving(Show, Read, Eq, Ord, Typeable)
$(deriveStructured ''Team)


clearTeams = delete (select [] (u "Team"))

insertTeams = insertMany [
  Team { name   = "Yankees"
       , home   = Location { city = "New York", state = "NY" }
       , league = "Nation" },
  Team { name   = "Mets"
       , home   = Location { city = "New York", state = "NY" }
       , league = "Nation" }
  ]

findTeams = find (select [] (u "Team")) >>= rest

printDocs title docs = liftIO $ do
  putStrLn title
  mapM_ (print .  exclude [(u "_id")]) docs

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
