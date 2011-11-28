-- | This module exports a 'Structued' type class which can be used to
-- convert from Haskel \"record types\" to @BSON@ objects and vice versa.
-- We use Templace Haskell to provide a function 'deriveStructured'
-- which can be used to automatically generate an instance of such
-- types for the 'Structured' and BSON's @Val@ classes.
--
-- Example use:
--
-- > data User = User { userId :: Int
-- >                  , userFirstName :: String
-- >                  , userLastName :: String
-- >                  }
-- >             deriving(Show, Read, Eq, Ord, Typeable)
-- > $(deriveStructured ''User)
-- > 
-- > data Profile = Profile { profileId       :: Int
-- >                        , profileName     :: String
-- >                        , profileUser     :: User
-- >                        , profileKeywords :: [Maybe String]
-- >                        }
-- >               deriving(Show, Read, Eq, Ord, Typeable)
-- > $(deriveStructured ''Profile)
--
--
--
{-# LANGUAGE TemplateHaskell #-}
module Database.MongoDB.Structured.Deriving.TH ( deriveStructured ) where

import Database.MongoDB.Structured
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad
import Database.MongoDB.Query (Collection)
import Data.Bson
import qualified Data.Bson as BSON
import Data.Functor ((<$>))
import Data.List (isPrefixOf)

--
import Debug.Trace
debug x = trace x (return ())
--

data T1 = T1

-- | This function generates 'Structured' and @Val@ instances for
-- record types.
deriveStructured :: Name -> Q [Dec]
deriveStructured t = do
  let className      = ''Structured
  let collectionName = 'collection
  let toBSONName     = 'toBSON
  let fromBSONName   = 'fromBSON

  -- Get record fields:
  rt <- reify t
  TyConI (DataD _ _ _ (RecC conName fields:[]) _) <- getFields t
  let fieldNames = map first fields
      sObjIds = lookForSObjId fields

  guardSObjId sObjIds
  let sObjName = (first . head) sObjIds

  collectionFunD <- funD_collection collectionName conName
  toBSONFunD     <- funD_toBSON toBSONName fieldNames sObjName
  fromBSONFunD   <- funD_fromBSON fromBSONName conName fieldNames sObjName
  -- Generate Structured instance:
  let structuredInst = InstanceD [] (AppT (ConT className) (ConT t)) 
                         [ collectionFunD
                         , toBSONFunD
                         , fromBSONFunD ]
  -- Generate Val instance:
  valInst <- gen_ValInstance t

  return [structuredInst, valInst]
    where getFields t = do
            r <- reify t
            case r of
              TyConI (DataD _ _ _ (RecC _ _:[]) _) -> return ()
              _ -> report True "Unsupported type. Can only derive for\
                               \ single-constructor record types."
            return r
          lookForSObjId = filter f
            where f (_,_,(ConT n)) = (n == ''SObjId)
                  f _ = False
          guardSObjId ids = if length ids /= 1
                              then report True "Expecting 1 SObjId field."
                              else return ()
          first (a,_,_) = a

-- | Generate the declaration for 'toBSON'.
--
-- Suppose we have
--
-- >  data User = User { userId        :: SObjId
-- >                   , userFirstName :: String
-- >                   , userLastName  :: String
-- >                   }
--
-- This function generates:
--
-- >  toBSON x = [ (u "_id")           := val (userId x)
-- >             , (u "userFirstName") := val (userFirstName x)
-- >             , (u "userLastName")  := val (userLastName x)
-- >             ]
--
-- The "_id" is created only if userId is not 'noSObjId'.
-- 
funD_toBSON :: Name     -- ^ toSBSON Name
            -> [Name]   -- ^ List of field Names
            -> Name     -- ^ SObjId Name
            -> Q Dec    -- ^ toBSON declaration
funD_toBSON toBSONName fieldNames sObjName = do
  x <- newName "x"
  toBSONBody <- NormalB <$> (gen_toBSON (varE x) fieldNames)
  let toBSONClause = Clause [VarP x] (toBSONBody) []
  return (FunD toBSONName [toBSONClause])
    where gen_toBSON x []     = [| [] |]
          gen_toBSON x (f:fs) =
            let l = nameBase f 
                i = nameBase sObjName
                v = appE (varE f) x
             in if l /= i
                  then [| ((u l) := val $v) : $(gen_toBSON x fs) |]
                  else [| let y  = ((u "_id") := val (unSObjId $v))
                              ys = $(gen_toBSON x fs)
                          in if isNoSObjId $v
                               then ys
                               else y : ys
                       |]

-- | Generate the declaration for 'collection'
funD_collection :: Name    -- ^ collection Name
                -> Name    -- ^ Name of type constructor
                -> Q Dec   -- ^ collection delclaration
funD_collection collectionName conName = do
  x <- newName "x"
  let n = nameBase conName
  d <- [d| collectionName _ = (u n) |]
  let [FunD _ cs] = d
  return (FunD collectionName cs)

funD_fromBSON :: Name     -- ^ fromSBSON Name
              -> Name     -- ^ Name of type constructor
              -> [Name]   -- ^ List of field Names
              -> Name     -- ^ SObjId name
              -> Q Dec    -- ^ fromBSON declaration
funD_fromBSON fromBSONName conName fieldNames sObjName = do
  doc <- newName "doc"
  fromBSONBody <- NormalB <$>
                    (gen_fromBSON conName fieldNames (varE doc) [] sObjName)
  let fromBSONClause = Clause [VarP doc] (fromBSONBody) []
  return (FunD fromBSONName [fromBSONClause])

-- | This function generates the body for the 'fromBSON' function
-- Suppose we have
--
-- >  data User = User { userId        :: SObjId
-- >                   , userFirstName :: String
-- >                   , userLastName  :: String
-- >                   }
--
-- Given the constructor name (@User@), field names, a document
-- expression (e.g., @doc@), and empty accumulator, this function generates:
--
-- >  fromBSON doc = lookup (u "_id")             doc >>= \val_1 ->
-- >                 lookup (u "userFirstName")   doc >>= \val_2 ->
-- >                 lookup (u "userLastName")    doc >>= \val_3 ->
-- >                 return User { userId        = val_1
-- >                             , userFirstName = val_2
-- >                             , userLastname  = val_3
-- >                             }
--
--

-- | BSON's lookup with Maybe as underlying monad.
lookup_m :: Val v => Label -> Document -> Maybe v
lookup_m = BSON.lookup

-- | Lookup _id. If not found, do not fail. Rather return 'noSObjId'.
lookup_id :: Document -> Maybe SObjId
lookup_id d = Just (SObjId (lookup_m (u "_id") d :: Maybe ObjectId))


gen_fromBSON :: Name            -- ^ Constructor name
             -> [Name]          -- ^ Field names
             -> Q Exp           -- ^ Document expression
             -> [(Name, Name)]  -- ^ Record field name, variable name pairs
             -> Name            -- ^ SObjId name
             -> Q Exp           -- ^ Record with fields set
gen_fromBSON conName []     _   vals sObjName = do
  (AppE ret _ )  <- [| return () |]
  let fieldExp = reverse $ map (\(l,v) -> (l, VarE v)) vals
  return (AppE ret (RecConE conName fieldExp))

gen_fromBSON conName (l:ls) doc vals sObjName =
  let lbl = nameBase l
  in if lbl == (nameBase sObjName)
      then [| lookup_id $doc >>= \val ->
              $(gen_fromBSON conName ls doc ((l,'val):vals) sObjName) |]
      else [| lookup_m (u lbl) $doc >>= \val ->
              $(gen_fromBSON conName ls doc ((l,'val):vals) sObjName) |]

-- | Given name of type, generate instance for BSON's @Val@ class.
gen_ValInstance :: Name -> Q Dec
gen_ValInstance t = do
  let valE = varE 'val
  [InstanceD valCtx (AppT valCType _) decs] <-
             [d| instance Val T1 where
                   val d = $valE (toBSON d)
                   cast' v = case v of
                               (Doc d) -> fromBSON d 
                               _ -> error "Only Doc supported"
             |]
  let decs' = (fixNames 'cast') <$> ((fixNames 'val) <$> decs)
  return (InstanceD valCtx (AppT valCType (ConT t)) decs') 
    where fixNames aN (FunD n cs) | (nameBase aN)
                                      `isPrefixOf` (nameBase n) = FunD aN cs
          fixNames _  x = x 
