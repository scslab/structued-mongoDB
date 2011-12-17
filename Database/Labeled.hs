--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Database.Labeled where

import LIO.TCB (mintTCB, unlabelTCB, ioTCB)
import LIO
import LIO.MonadCatch
import LIO.DCLabel hiding (label)
import DCLabel.PrettyShow

import Control.Monad (unless, forM)

-- | A callectino is a list of labeled objects.
data Collection a = C { colLabel :: DCLabel
                      , colClear :: DCLabel
                      , colEntry :: [DCLabeled a]
                      }

-- FOR DEBUGGING:
instance Show a => Show (Collection a) where
  show x = "C { colLabel = " ++ prettyShow (colLabel x) ++ 
             ", colClear = " ++ prettyShow (colClear x) ++
             ", colEntry = [" ++ showEntries (colEntry x) ++ "]}"
    where showEntries [] = ""
          showEntries (x:xs) = "\n\t" ++ prettyShow (labelOf x) ++ ": " ++
                                 show (unlabelTCB x) ++ showEntries xs
          
--


-- | Simple sum type for a policy labeled field. A field can be
-- labeled, or unabled, depending on whether or not the policy was applied.
data PolicyLabeled a = PU a             -- ^ Policy not applied 
                     | PL (DCLabeled a) -- ^ Policy applied

-- FOR DEBUGGING:
instance Show a => Show (PolicyLabeled a) where
  show (PU x)   = "PU " ++ show x
  show (PL x)   = "PL " ++ prettyShow (labelOf x) 

--

class MkPolicyLabeled a b where
  mkPolicyLabeled :: a -> PolicyLabeled b

instance MkPolicyLabeled a a where mkPolicyLabeled = PU 
instance MkPolicyLabeled (DCLabeled a) a where mkPolicyLabeled = PL

-- | A row policy.
class RowPolicy r where
  rowPolicy :: r -> DCLabel

-- Verbose: 
-- class ColPolicy record facet typeOfFacet
--       | facet -> record, facet -> typeOfFacet where
class Facet r f t | f -> r, f -> t where

-- | A policy for a 'PolicyLabeled' type.
class ColPolicy r f where
  colPolicy :: Facet r f t => r -> f -> DCLabel

-- | Users should not be allowed to create an instance of this class.
-- TODO: We should endorse the result, and check in 'appPolicy' that it
-- was endorsed here.
class AppColPolicy r f where
  appColPolicyP :: (Facet r f t, ColPolicy r f) =>
    DCPrivTCB -> r -> f -> PolicyLabeled t -> DC (PolicyLabeled t)
  appColPolicyP p r f x =
    let l = colPolicy r f
    in case x of 
         (PU y)   -> labelP p l y >>= return . PL
         p@(PL y) -> if labelOf y == l 
                       then return p
                       else throwIO (userError "Policy mismatch")
  appColPolicy :: (Facet r f t, ColPolicy r f) =>
               r -> f -> PolicyLabeled t -> DC (PolicyLabeled t)
  appColPolicy = appColPolicyP noPriv
-- | Apply row and column policies. Instances for this should be
-- automatically derived.
class LabelDoc r where
  labelDocP :: RowPolicy r => DCPrivTCB -> r -> DC (DCLabeled r)
  labelDoc :: RowPolicy r => r -> DC (DCLabeled r)
  labelDoc = labelDocP noPriv


--
-- DB operations
--

insertP :: LabelDoc a => 
  DCPrivTCB -> Collection a -> DCLabeled a -> DC (Collection a)
insertP p c e = do
  curL <- getLabel
  -- can write to collection
  unless (leqp p curL (colLabel c)) $ throwIO LerrHigh
  -- label of row is below collection clearance
  unless (leqp p (labelOf e) (colClear c)) $ throwIO LerrClearance
  let es = e : colEntry c
  return $ c { colEntry = es }
  
queryP :: LabelDoc a => DCPrivTCB -> Collection a -> DC ([DCLabeled a])
queryP p c = do
  taintP p (colLabel c)
  return $ colEntry c

--
-- Example
--


data User = User { userId       :: Int
                 , userName     :: String
                 , userPassword :: PolicyLabeled String
                 }
                 deriving(Show)



instance RowPolicy User where
  rowPolicy u =  newDC ((userName u) .\/. "admin") (userName u)

data UserPassword = UserPassword

instance Facet User UserPassword String where

instance ColPolicy User UserPassword where
  colPolicy u _ =  newDC (userName u) (userName u)

-- | Need to make sure code cannot actually define 'appColPolicy'
instance AppColPolicy User UserPassword
                   

-- | Trusted generated instance. Can be automatically derived.
instance LabelDoc User where
  labelDocP p u =
    let lr = rowPolicy u
    in appColPolicyP p u UserPassword (userPassword u) >>= \pass ->
          labelP p lr $ u { userPassword = pass }
  

printDC :: String -> DC ()
printDC x = do
  l <- getLabel
  ioTCB . putStrLn $ prettyShow l ++ " %\t" ++ x

main = evalPrettyDC $ do
  taint (newDC (<>) (<>)) -- starting label shoul be empty label
  let c0 :: Collection User
      c0 = C { colLabel = newDC (<>) (<>), colClear = ltop, colEntry = [] }
  printDC . show $ c0

  printDC "ALICE:"
  c1 <- aliceCode c0
  printDC . show $ c1

  printDC "BOB:"
  --c2 <- bobBadCode c1
  c2 <- bobGoodCode c1
  printDC . show $ c2

  printDC "ALICE:"
  --c3 <- aliceBadCode c2
  c3 <- aliceCode2 c2
  printDC . show $ c3

    where evalPrettyDC io = do
            (_, l) <- evalDC io 
            putStrLn $ prettyShow l


aliceCode c = withClearance (newDC "alice" (<>)) $ do
  let p = mintTCB $ newPriv ("alice") :: DCPrivTCB
  let x = User { userId = 1
               , userName = "alice"
               , userPassword = mkPolicyLabeled "password"
               }
  lx <- labelDocP p x
  insertP p c lx

bobBadCode c = withClearance (newDC "bob" (<>)) $ do
  let p = mintTCB $ newPriv ("bob") :: DCPrivTCB
  let x = User { userId = 1
               , userName = "alice"
               , userPassword = mkPolicyLabeled "password"
               }
  lx <- labelDocP p x
  insertP p c lx

bobGoodCode c = withClearance (newDC "bob" (<>)) $ do
  let p = mintTCB $ newPriv ("bob") :: DCPrivTCB
  bob_password <- labelP p (newDC "bob" "bob") "123"
  let x = User { userId = 1
               , userName = "bob"
               , userPassword = mkPolicyLabeled bob_password
               }
  lx <- labelDocP p x
  insertP p c lx

aliceBadCode c = withClearance (newDC "alice" (<>)) $ do
  let p = mintTCB $ newPriv ("alice") :: DCPrivTCB
  clr <- getClearance
  es <- queryP p c
  forM es (unlabelP p)

aliceCode2 c = withClearance (newDC "alice" (<>)) $ do
  let p = mintTCB $ newPriv ("alice") :: DCPrivTCB
  clr <- getClearance
  es <- queryP p c >>= return . filter (\x -> (leqp p (labelOf x) clr))
  forM es (unlabelP p)
  
