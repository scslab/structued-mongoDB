{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
import Database.MongoDB.Structured
import Database.MongoDB.Structured.Deriving.TH
import Control.Monad.Trans (liftIO)
import Data.Typeable
import Control.Monad (mapM_)
import Control.Monad.IO.Class
import Data.Maybe (isJust, fromJust)

data Address = Address { addrId :: SObjId
                       , city   :: String
                       , state  :: String
                       } deriving (Show, Eq, Typeable)
$(deriveStructured ''Address)

data Team = Team { teamId :: SObjId
                 , name   :: String
                 , home   :: Address
                 , league :: String
                 } deriving (Show, Eq, Typeable)
$(deriveStructured ''Team)

main = do
   pipe <- runIOE $ connect (host "127.0.0.1")
   e <- access pipe master "baseball" run
   close pipe
   print e

run = do
   clearTeams
   insertTeams
   allTeams >>= printDocs "All Teams"
   nationalLeagueTeams >>= printDocs "National League Teams"
   newYorkTeams >>= printDocs "New York Teams"

-- Delete all teams:
clearTeams :: Action IO ()
clearTeams = delete (select ( (.*) :: QueryExp Team))

insertTeams :: Action IO [Value]
insertTeams = insertMany [
   Team { teamId = noSObjId
        , name   = "Yankees"
        , home   = Address { addrId = noSObjId
                           , city  = "New York"
                           , state = "NY"
                           }
        , league = "American"}
  , Team { teamId = noSObjId
         , name   = "Mets"
         , home   = Address { addrId = noSObjId
                            , city  = "New York"
                            , state = "NY"
                            }
         , league = "National"}
  , Team { teamId = noSObjId
         , name   = "Phillies"
         , home   = Address { addrId = noSObjId
                            , city  = "Philadelphia"
                            , state = "PA"
                            }
         , league = "National"}
  , Team { teamId = noSObjId
         , name   = "Red Sox"
         , home   = Address { addrId = noSObjId
                            , city  = "Boston"
                            , state = "MA"
                            }
         , league = "National"}
  ]

allTeams :: Action IO [Maybe Team]
allTeams = let query = (select ((.*) :: QueryExp Team))
                            { sort = [asc (Home .! City)]}
           in find query >>= rest
           
nationalLeagueTeams :: Action IO [Maybe Team]
nationalLeagueTeams = rest =<< find (select (League .== "National"))

newYorkTeams :: Action IO [Maybe Team]
newYorkTeams = rest =<< find (select (Home .! State .== "NY"))

printDocs :: MonadIO m => String -> [Maybe Team] -> m ()
printDocs title teams' = liftIO $ do
  let teams = (map fromJust) . filter (isJust) $ teams'
  putStrLn title 
  mapM_ (putStrLn . show) teams

