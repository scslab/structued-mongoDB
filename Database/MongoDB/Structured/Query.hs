{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-| This module exports several classes and combinators that operated on
  'Structured' types. Specifically, we provide the structured versions
  of @mongoDB@''s combinators, including structured query creation.
-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.MongoDB.Structured.Query (
                                          -- * Insert
                                           insert, insert_
                                         , insertMany, insertMany_
                                         , insertAll, insertAll_
                                         -- * Update
                                         , save
                                         -- * Delete
                                         , delete, deleteOne
                                         -- * Order
                                         , asc
                                         , desc
                                         -- * Query
                                         , StructuredQuery
                                         , limit
                                         , skip
                                         , sort
                                         , find
                                         , findOne
                                         , fetch
                                         , count
                                         -- * Structured selections/queries
                                         , StructuredSelection
                                         , StructuredSelect(select)
                                         , Selectable(..)
                                         , (.!)
                                         , QueryExp
                                         , (.*)
                                         , (.==), (./=), (.<), (.<=), (.>), (.>=)
                                         , (.&&), (.||), not_
                                         -- * Cursor
                                         , StructuredCursor
                                         , closeCursor, isCursorClosed
                                         , nextBatch, next, nextN, rest
                                         -- * Rexports
                                         , module Database.MongoDB.Query
                                         , Value
                                         ) where

import qualified Database.MongoDB.Query as M
import Database.MongoDB.Query (Action
                              , access
                              , Failure(..)
                              , ErrorCode
                              , AccessMode(..)
                              , GetLastError
                              , master
                              , slaveOk
                              , accessMode
                              , MonadDB(..)
                              , Database
                              , allDatabases
                              , useDb
                              , thisDatabase
                              , Username
                              , Password
                              , auth)
import Database.MongoDB.Structured.Types
import Database.MongoDB.Internal.Util
import Data.Bson
import Data.List (sortBy, groupBy)
import Data.Functor
import Data.Word
import Data.CompactString.UTF8 (intercalate)
import Control.Monad
import Control.Monad.MVar
import Control.Monad.IO.Class


--
-- Insert
--

-- | Inserts document to its corresponding collection and return
-- the \"_id\" value.
insert :: (MonadIO' m, Structured a) => a -> Action m Value
insert x = M.insert (collection x) (toBSON x)

-- | Same as 'insert' but discarding result.
insert_ :: (MonadIO' m, Structured a) => a -> Action m ()
insert_ x = insert x >> return ()

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
   (M.Collection -> [Document] -> Action m [Value]) -> [a] -> Action m [Value]
insertManyOrAll insertFunc ss = do
  let docs  = map (\x -> (collection x, toBSON x)) ss
      gdocs = (groupBy (\(a,_) (b,_) -> a == b))
              . (sortBy (\(a,_) (b,_) -> compare a b)) $ docs
  concat <$> (forM gdocs $ \ds ->
                if (null ds)
                  then return []
                  else insertFunc (fst . head $ ds) (map snd ds)
             )

--
-- Update
--

-- | Save document to collection. If the 'SObjId' field is set then
-- the document is updated, otherwise we perform an insert.
save :: (MonadIO' m, Structured a) => a -> Action m ()
save x = M.save (collection x) (toBSON x)


--
-- Delete
--

-- | Delete all documents that match the selection/query.
delete :: MonadIO m => StructuredSelection -> Action m ()
delete = M.delete . unStructuredSelection 

-- | Delete the first documents that match the selection/query.
deleteOne :: MonadIO m => StructuredSelection -> Action m ()
deleteOne = M.deleteOne . unStructuredSelection 


--
-- Query
--

-- | Find documents satisfying query
find :: (MonadControlIO m, Functor m)
     => StructuredQuery -> Action m StructuredCursor
find q = StructuredCursor <$> (M.find . unStructuredQuery $ q)

-- | Find documents satisfying query
findOne :: (MonadIO m, Structured a)
     => StructuredQuery -> Action m (Maybe a)
findOne q = do 
  res <- M.findOne . unStructuredQuery $ q
  return $ res >>= fromBSON

-- | Same as 'findOne' but throws 'DocNotFound' if none match.
fetch :: (MonadIO m, Functor m, Structured a)
     => StructuredQuery -> Action m (Maybe a)
fetch q = fromBSON <$> (M.fetch . unStructuredQuery $ q)

-- | Count number of documents satisfying query.
count :: (MonadIO' m) => StructuredQuery -> Action m Int
count = M.count . unStructuredQuery


--
--
--

-- | Wrapper for @mongoDB@'s @Cursor@.
newtype StructuredCursor = StructuredCursor { unStructuredCursor :: M.Cursor }

-- | Return next batch of structured documents.
nextBatch :: (Structured a, MonadControlIO m, Functor m)
          => StructuredCursor -> Action m [Maybe a]
nextBatch c = (map fromBSON) <$> M.nextBatch (unStructuredCursor c)

-- | Return next structured document. If failed return 'Left',
-- otherwise 'Right' of the deserialized result.
next :: (Structured a, MonadControlIO m)
     => StructuredCursor -> Action m (Either () (Maybe a))
next c = do
    res <- M.next (unStructuredCursor c)
    case res of
      Nothing -> return (Left ())
      Just r  -> return (Right $ fromBSON r)

-- | Return up to next @N@ documents.
nextN :: (Structured a, MonadControlIO m, Functor m)
      => Int -> StructuredCursor -> Action m [Maybe a]
nextN n c = (map fromBSON) <$> M.nextN n (unStructuredCursor c)


-- | Return the remaining documents in query result.
rest :: (Structured a, MonadControlIO m, Functor m)
     => StructuredCursor -> Action m [Maybe a]
rest c = (map fromBSON) <$> M.rest (unStructuredCursor c)

-- | Close the cursor.
closeCursor :: MonadControlIO m => StructuredCursor -> Action m ()
closeCursor = M.closeCursor . unStructuredCursor

-- | Check if the cursor is closed.
isCursorClosed :: MonadIO m => StructuredCursor -> Action m Bool
isCursorClosed = M.isCursorClosed . unStructuredCursor



--
-- Structured selections/queries
--

-- | Wrapper for @mongoDB@'s @Selection@ type.
newtype StructuredSelection =
  StructuredSelection { unStructuredSelection :: M.Selection }
  deriving(Eq, Show)

-- | Wrapper for @mongoDB@'s @Query@ type.
data StructuredQuery = StructuredQuery
                        { selection :: StructuredSelection
                        -- ^ Actual query.
                        , skip      :: Word32 
                        -- ^ Number of matching objects to skip
                        -- (default: 0).
                        , limit     :: Word32
                        -- ^ Maximum number of objects to return
                        -- (default: 0, no limit).
                        , sort      :: [OrderExp]
                        -- ^ Sortresult by this order.
                        }
  deriving(Eq, Show)


-- | Analog to @mongoDB@'s @Select@ class
class StructuredSelect aQorS where
  -- | Create a selection or query from an expression
  select :: Structured a => QueryExp a -> aQorS

instance StructuredSelect StructuredSelection where
  select = StructuredSelection . expToSelection

instance StructuredSelect StructuredQuery where
  select x = StructuredQuery (StructuredSelection $ expToSelection x)
                              0 0 ([])

unStructuredQuery :: StructuredQuery -> M.Query
unStructuredQuery sq = M.Query [] -- options
                               (unStructuredSelection $ selection sq)
                               [] -- project
                               (skip sq) -- skip
                               (limit sq) -- limit
                               (expToOrder $ sort sq) -- sort
                               False 0 []

-- | Class defining a selectable type. Type 'a' corresponds to the
-- record type, 'f' corresponds to the field or facet, and 't'
-- corresponds to the field/facet type.
class Val t => Selectable a f t | f -> a, f -> t where
  -- | Given facet, return the BSON field name
  s :: f -> t -> Label

-- | Nested fields (used for extracting the names of fields in a
-- nested record). 
data Nested f f' = Nested Label

-- | Combining two field names to create a 'Nested' type.
(.!) :: (Selectable r f t, Selectable t f' t') => f -> f' -> Nested f f'
(.!) f f' = Nested $ intercalate (u ".") [(s f undefined), (s f' undefined)]

instance (Selectable r f t, Selectable t f' t') =>
          Selectable r (Nested f f') t' where
  s (Nested l) _ = l

-- | A query expression.
data QueryExp a = StarExp
                | EqExp   !Label   !Value
                | LBinExp !UString !Label !Value
                | AndExp  (QueryExp a) (QueryExp a) 
                | OrExp   (QueryExp a) (QueryExp a) 
                | NotExp  (QueryExp a)
                deriving (Eq, Show)

infix   9 .! 
infix   4 .==, ./=, .<, .<=, .>, .>=
infixr  3 .&&
infixr  2 .||

-- | Combinator for @==@
(.*) :: (Structured a) => QueryExp a
(.*) = StarExp

-- | Combinator for @==@
(.==) :: (Val t, Selectable a f t) => f -> t -> QueryExp a
(.==) f v = EqExp  (s f v) (val v)

-- | Combinator for @$ne@
(./=) :: (Val t, Selectable a f t) => f -> t -> QueryExp a
(./=) f v = LBinExp (u "$ne") (s f v) (val v)

-- | Combinator for @<@
(.< ) :: (Val t, Selectable a f t) => f -> t -> QueryExp a
(.< ) f v = LBinExp (u "$lt") (s f v) (val v)

-- | Combinator for @<=@
(.<=) :: (Val t, Selectable a f t) => f -> t -> QueryExp a
(.<=) f v = LBinExp (u "$lte") (s f v) (val v)

-- | Combinator for @>@
(.> ) :: (Val t, Selectable a f t) => f -> t -> QueryExp a
(.> ) f v = LBinExp (u "$gt") (s f v) (val v)

-- | Combinator for @>=@
(.>=) :: (Val t, Selectable a f t) => f -> t -> QueryExp a
(.>=) f v = LBinExp (u "$gte") (s f v) (val v)

-- | Combinator for @$and@
(.&&) :: QueryExp a -> QueryExp a -> QueryExp a
(.&&) = AndExp

-- | Combinator for @$or@
(.||) :: QueryExp a -> QueryExp a -> QueryExp a
(.||) = OrExp

-- | Combinator for @$not@
not_ :: QueryExp a -> QueryExp a
not_ = NotExp

-- | Convert a query expression to a 'Selector'.
expToSelector :: Structured a => QueryExp a -> M.Selector
expToSelector (StarExp)        = [ ]
expToSelector (EqExp l v)      = [ l := v ]
expToSelector (LBinExp op l v) = [ l =: [ op := v ]]
expToSelector (AndExp e1 e2)   = [ (u "$and") =: [expToSelector e1
                                                 , expToSelector e2] ]
expToSelector (OrExp e1 e2)    = [ (u "$or") =: [expToSelector e1
                                                , expToSelector e2] ]
expToSelector (NotExp e)       = [ (u "$not") =: expToSelector e]

-- | Convert query expression to 'Selection'.
expToSelection :: Structured a => QueryExp a -> M.Selection
expToSelection e = M.Select { M.selector = (expToSelector e)
                            , M.coll = (collection . c $ e) }
  where c :: Structured a => QueryExp a -> a
        c _ = undefined

-- | An ordering expression
data OrderExp = Desc Label
              | Asc Label
  deriving(Eq, Show)

-- | Sort by field, ascending
asc :: Selectable a f t => f -> OrderExp
asc f = Asc (s f undefined)

-- | Sort by field, descending
desc :: Selectable a f t => f -> OrderExp
desc f = Desc (s f undefined)

-- | Convert a list of @OrderExp@ to a @mongoDB@ @Order@
expToOrder :: [OrderExp] -> M.Order
expToOrder exps = map _expToLabel exps
  where _expToLabel (Desc fieldName) = fieldName := val (-1 :: Int)
        _expToLabel (Asc fieldName) = fieldName := val (1 :: Int)

