{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
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


data User = User { userId    :: SObjId
                 , firstName :: String
                 , lastName  :: String
                 , favNr     :: Int
                 } deriving(Show, Read, Eq, Ord, Typeable)
$(deriveStructured ''User)


insertUsers = insertMany 
  [ User { userId = noSObjId, firstName = "deian", lastName = "stefan", favNr = 3 }
  , User { userId = noSObjId, firstName = "amit" , lastName = "levy", favNr = 42 }
  , User { userId = noSObjId, firstName = "david", lastName = "mazieres", favNr = 1337 }
  ]


run = do
   delete (select [ ] "User")
   insertUsers
   --let query = (select [ "firstName" =: ("deian" :: String )] "User")
   --let query = (select [ "favNr" =: ["$gt" =: (3 :: Int)] ] "User")
   --let query = (select [ "$or"  =: [[ "favNr" =: ["$gt" =: (3 :: Int)]]
   --                                ,["firstName" =: ("deian" :: String)]
   --                                ]] "User")
   --let query = (select (expToSelector (FirstName .== "deian" .|| FavNr .>= 3)) "User")
   let query = mySelect (FirstName .== "deian" .|| FavNr .>= 3)
   liftIO $ print query
   users <- find query >>= rest
   liftIO $ printFunc users
    where printFunc users = forM_ users $ \u ->
            putStrLn . show $ (fromJust . fromBSON $ u :: User)

main = do
   pipe <- runIOE $ connect (host "127.0.0.1")
   e <- access pipe master "auth" run
   close pipe
   print e

--
--
--

class Val t => Selectable a f t | f -> a, f -> t where
  -- | Given facet, return the BSON field name
  s :: f -> t -> Label

data UserId = UserId deriving (Show, Eq)
instance Selectable User UserId SObjId where s _ _ = "_id"

data FirstName = FirstName deriving (Show, Eq)
instance Selectable User FirstName String where s _ _ = "firstName"

data LastName = LastName deriving (Show, Eq)
instance Selectable User LastName String where s _ _ = "lastName"

data FavNr = FavNr deriving (Show, Eq)
instance Selectable User FavNr Int where s _ _ = "favNr"


--
--
--

data QueryExp a = EqExp   !Label   !Value
                | LBinExp !UString !Label !Value
                | AndExp  (QueryExp a) (QueryExp a) 
                | OrExp   (QueryExp a) (QueryExp a) 
                | NotExp  (QueryExp a)
                deriving (Eq, Show)

infix   4 .==, ./=, .<, .<=, .>, .>=
infixr  3 .&&
infixr  2 .||

-- | Combinator for @==@
(.==) :: (Val t, Selectable a f t) => f -> t -> QueryExp a
(.==) f v = EqExp  (s f v) (val v)

-- | Combinator for @$ne@
(./=) :: (Val t, Selectable a f t) => f -> t -> QueryExp a
(./=) f v = LBinExp "$ne" (s f v) (val v)

-- | Combinator for @<@
(.< ) :: (Val t, Selectable a f t) => f -> t -> QueryExp a
(.< ) f v = LBinExp "$lt" (s f v) (val v)

-- | Combinator for @<=@
(.<=) :: (Val t, Selectable a f t) => f -> t -> QueryExp a
(.<=) f v = LBinExp "$lte" (s f v) (val v)

-- | Combinator for @>@
(.> ) :: (Val t, Selectable a f t) => f -> t -> QueryExp a
(.> ) f v = LBinExp "$gt" (s f v) (val v)

-- | Combinator for @>=@
(.>=) :: (Val t, Selectable a f t) => f -> t -> QueryExp a
(.>=) f v = LBinExp "$gte" (s f v) (val v)

-- | Combinator for @$and@
(.&&) :: QueryExp a -> QueryExp a -> QueryExp a
(.&&) = AndExp

-- | Combinator for @$or@
(.||) :: QueryExp a -> QueryExp a -> QueryExp a
(.||) = OrExp

-- | Combinator for @$not@
not :: QueryExp a -> QueryExp a
not = NotExp

expToSelector :: Structured a => QueryExp a -> Document
expToSelector (EqExp l v)      = [ l := v ]
expToSelector (LBinExp op l v) = [ l =: [ op := v ]]
expToSelector (AndExp e1 e2)   = [ "$and" =: [expToSelector e1, expToSelector e2] ]
expToSelector (OrExp e1 e2)    = [ "$or"  =: [expToSelector e1, expToSelector e2] ]
expToSelector (NotExp e)       = [ "$not" =: expToSelector e]

mySelect :: (Structured a, Select s) => QueryExp a -> s
mySelect e = select (expToSelector e) (collection . coll $ e)
  where coll :: Structured a => QueryExp a -> a
        coll _ = undefined
        


--
--
--
