{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Test.Snowdrop.Core.Types
       ( -- * Types
         T (..)
       , Id
       , Value
       , B
       , Err
       , IdValB (..)
       , InterpetResult (..)
       , Command (..)

         -- general functions
       , runCommands

         -- counter monad
       , getOperNum

         -- result monad
       , getRes
       ) where

import           Universum

import           Control.Monad.Except (throwError)
import           Data.Default (def)

import           Snowdrop.Core (CSMappendException (..), ERoComp, IdSumPrefixed (..), Prefix (..),
                                SValue, StatePException (..), iteratorFor, queryOne, querySet)
import           Snowdrop.Util
import           Test.Snowdrop.Core.Executor (Counter, TestCtx, countERoComp, runERoComp)

----------------------------------------------------------------------------
-- Types & functions
----------------------------------------------------------------------------

-- Type helper for generation of different types.
data T = TI Int | TS String
    deriving (Show, Eq, Ord)

deriveView withInjProj ''T
deriveIdView withInjProj ''T

type Id = T

type instance SValue Id = T

instance IdSumPrefixed Id where
    idSumPrefix (TI _) = Prefix 0
    idSumPrefix (TS _) = Prefix 1

data Err = ErrCSM (CSMappendException Id)
         | ErrStateP StatePException
  deriving (Eq, Show)

deriveView withInj ''Err

instance Exception Err

type Value = T
type B = T

-- | Data type to represent different functions to use in 'iteratorFor'.
-- Instead of keeping functions as they are we are using different constructors
-- so the 'Show' and 'Eq' instances are possible, which is necessary for testing.
data IdValB = IdValB1 | IdValB2 | IdValB3 | IdValB4 | IdValB5 | IdValB6
    deriving (Show, Eq, Enum, Bounded)

-- | By the given 'IdValB' returns the corresponding function to use in
-- 'iteratorFor' form the predermined list of functions of this type.
getIdValBFun :: IdValB -> ((Id, Value) -> B -> B)
getIdValBFun IdValB1 = sumTI
getIdValBFun IdValB2 = sumTS
getIdValBFun IdValB3 = sumWithLen
getIdValBFun IdValB4 = sumWithShow
getIdValBFun IdValB5 = fstTI
getIdValBFun IdValB6 = fstTS

-- Random functions.
sumTI, sumTS, sumWithLen, sumWithShow, fstTI, fstTS :: (Id, Value) -> B -> B

-- Returns the sum of all 'Int's.
sumTI (i, v) b = TI $ vOr0 i + vOr0 v + vOr0 b
  where
    vOr0 :: T -> Int
    vOr0 (TI x) = x
    vOr0 _      = 0

-- Returns the sum of all 'String's.
sumTS (i, v) b = TS $ vOrEmpty i ++ vOrEmpty v ++ vOrEmpty b
  where
    vOrEmpty :: T -> String
    vOrEmpty (TS x) = x
    vOrEmpty _      = ""

-- Returns the sum of 'T' but with 'String's replaced with its length.
sumWithLen (i, v) b = TI $ vOrLen i + vOrLen v + vOrLen b
  where
    vOrLen :: T -> Int
    vOrLen (TI x) = x
    vOrLen (TS x) = length x

-- Returns the sum of 'T' but with 'Int's replaced with its 'String' representations.
sumWithShow (i, v) b = TS $ vOrShow i ++ vOrShow v ++ vOrShow b
  where
    vOrShow :: T -> String
    vOrShow (TS x) = x
    vOrShow (TI x) = show x

-- Returns the first met 'Int'.
fstTI (i@(TI _), _) _ = i
fstTI (_, v@(TI _)) _ = v
fstTI _ b@(TI _)      = b
fstTI _ _             = TI 0

-- Returns the first met 'String'.
fstTS (i@(TS _), _) _ = i
fstTS (_, v@(TS _)) _ = v
fstTS _ b@(TS _)      = b
fstTS _ _             = TS "oops"

-- | Represents the result of the interpretation of the list
-- of 'Command's, which are interpreted with the 'runCommands'.
data InterpetResult = IR
    { irQuerySets :: [Map Id Value]
    , irQueryOnes :: [Maybe Value]
    , irIterators :: [T]
    }
    deriving (Show, Eq)

instance Semigroup InterpetResult where
    IR qs1 qo1 i1 <> IR qs2 qo2 i2 = IR
        { irQuerySets = qs1 <> qs2
        , irQueryOnes = qo1 <> qo2
        , irIterators = i1  <> i2
        }

instance Monoid InterpetResult where
    mappend = (<>)
    mempty  = IR [] [] []

-- | Data structure to represent operation.
data Command
    = QuerySet (Set Id)
    | QueryOne Id
    | ThrowError Err
    | IteratorFor Prefix B IdValB
    deriving (Show, Eq)

-- | Interpret Commands one by one with modifying final 'InterpetResult'.
runCommands :: [Command] -> ERoComp Err Id Value (TestCtx Id Value) InterpetResult
runCommands []                         = pure mempty
runCommands (QuerySet ids : cmds)      = querySet ids >>= \mp -> runCommands cmds >>= addMap mp
runCommands (QueryOne i   : cmds)      = queryOne i >>= \mb -> runCommands cmds >>= addMaybe mb
runCommands (ThrowError e : cmds)      = throwError e >> runCommands cmds
runCommands (IteratorFor p initB f : cmds) = iteratorFor p initB (getIdValBFun f)
                                           >>= \b -> runCommands cmds >>= addB b

-- | Change 'irQuerySets' of 'InterpretResult' by adding the given 'Map'.
addMap :: Monad m => Map Id Value -> InterpetResult -> m InterpetResult
addMap m ir = pure $ ir { irQuerySets = m : irQuerySets ir }

-- | Change 'irQueryOnes' of 'InterpretResult' by adding the given 'Maybe'.
addMaybe :: Monad m => Maybe Value -> InterpetResult -> m InterpetResult
addMaybe v ir = pure $ ir { irQueryOnes = v : irQueryOnes ir }

-- | Change 'irIterators' of 'InterpretResult' by adding the given @a@.
addB :: Monad m => T -> InterpetResult -> m InterpetResult
addB b ir = pure $ ir { irIterators = b : irIterators ir }

----------------------------------------------------------------------------
-- for counting operations
----------------------------------------------------------------------------

-- | Gets the number of query and iteration operations.
getOperNum :: ERoComp Err Id Value (TestCtx Id Value) InterpetResult -> Either Err Counter
getOperNum comp = runReader (countERoComp comp) def

----------------------------------------------------------------------------
-- Result of concat test stuff
----------------------------------------------------------------------------

-- | Calculate the result of the given 'ERoComp' with provided map.
getRes :: Map Id Value
       -> ERoComp Err Id Value (TestCtx Id Value) InterpetResult
       -> Either Err InterpetResult
getRes mp er = runReader (runERoComp er) mp

