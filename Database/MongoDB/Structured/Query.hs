module Database.MongoDB.Structured.Query (
                                          -- * Insert
                                           insert, insert_
                                         , insertMany, insertMany_
                                         , insertAll, insertAll_
                                         , module Database.MongoDB.Query
                                         ) where

import qualified Database.MongoDB.Query as M
import Database.MongoDB.Query hiding ( insert, insert_
                                     , insertMany, insertMany_
                                     , insertAll, insertAll_)
import Database.MongoDB.Structured
import Database.MongoDB.Internal.Util
import Data.Bson
import Data.List (sortBy, groupBy)
import Data.Functor
import Control.Monad

-- | Inserts document to its corresponding collection and return
-- the \"_id\" value.
insert :: (MonadIO' m, Structured a) => a -> Action m Value
insert s = M.insert (collection s) (toBSON s)

-- | Same as 'insert' but discarding result.
insert_ :: (MonadIO' m, Structured a) => a -> Action m ()
insert_ s = insert s >> return ()

-- | Inserts documents to their corresponding collection and return
-- their \"_id\" values.
insertMany :: (MonadIO' m, Structured a) => [a] -> Action m [Value]
insertMany = insertManyOrAll (M.insertMany)

-- | Same as 'insertMany' but discarding result.
insertMany_ :: (MonadIO' m, Structured a) => [a] -> Action m ()
insertMany_ ss = insertMany ss >> return ()

-- | Inserts documents to their corresponding collection and return
-- their \"_id\" values. Unlike 'insertMany', this function keeps
-- inserting remaining documents even if an error occurs.
insertAll :: (MonadIO' m, Structured a) => [a] -> Action m [Value]
insertAll = insertManyOrAll (M.insertAll)

-- | Same as 'insertAll' but discarding result.
insertAll_ :: (MonadIO' m, Structured a) => [a] -> Action m ()
insertAll_ ss = insertAll ss >> return ()

-- | Helper function that carries out the hard work for 'insertMany'
-- and 'insertAll'.
insertManyOrAll :: (MonadIO' m, Structured a) =>
   (Collection -> [Document] -> Action m [Value]) -> [a] -> Action m [Value]
insertManyOrAll insertFunc ss = do
  let docs  = map (\s -> (collection s, toBSON s)) ss
      gdocs = (groupBy (\(a,_) (b,_) -> a == b))
              . (sortBy (\(a,_) (b,_) -> compare a b)) $ docs
  concat <$> (forM gdocs $ \ds ->
                if (null ds)
                  then return []
                  else insertFunc (fst . head $ ds) (map snd ds)
             )
