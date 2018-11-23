{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

-- | ValueOp primitive.

module Snowdrop.Core.ChangeSet.ValueOp
       (
         ValueUpd (..)
       , ValueOp (..)
       , ValueOpEx (..)
       , (<->)
       , makeReplace
       , composeFList
       , getVal
       ) where

import           Universum
import qualified Prelude as P

import           Data.Serialize (Serialize (..))


import           Snowdrop.Util (HasPrism (..), HasReview (..))


-- FAKE
-- Consider using `StaticPtr`, thus we obtain
--   serializable functions (on `Typeable`s)
instance Serialize v => Serialize (v -> Maybe v) where
  put _ = return ()
  get   = return Just

data ValueUpd v =
    Replace v
  -- The list of updates is a stack:
  --   the latest update is a head of the list
  | Modify [v -> Maybe v]
  deriving Generic

instance Serialize v => Serialize (ValueUpd v)

instance Hashable v => Hashable (ValueUpd v) where
   hashWithSalt n (Replace v) = hashWithSalt n v
   hashWithSalt n _           = n

-- 
composeFList :: [v -> Maybe v] -> v -> Maybe v
composeFList = foldr (<=<) Just

instance Functor ValueUpd where
  fmap f (Replace v) = Replace (f v)
  -- Can't do better
  fmap _ (Modify _) = Modify []

instance Show a => Show (ValueUpd a) where
  show (Replace v) = "Replace " ++ P.show v
  show (Modify _) = "Modify"

instance Eq a => Eq (ValueUpd a) where
  Replace u   == Replace v   = u == v
  _           == _           = False

instance (Eq a, Ord a) => Ord (ValueUpd a) where
  Replace u <= Replace v = u <= v
  _         <= _         = False

instance Foldable ValueUpd where
  foldr f z (Replace v) = f v z
  foldr _ z _           = z

instance Traversable ValueUpd where
  traverse f (Replace x) = pure Replace <*> f x
  traverse _ (Modify _)  = pure (Modify [])

-- | ValueOp datatype is an action which should be performed over an entry
-- stored in a key-value storage. Intended to be used in @ChangeSet@,
-- in Map Id (ValueOp Value).
data ValueOp v
    = New v
    -- ^ @New@ operation requires the following invariants (both of them):
    --   1. key which holds this ValueOp doesn't exist in a state,
    --   2. after aplication of entry (key, New v) to the state a value stored
    --      by @key@ is @v@.
    | Upd (ValueUpd v)
    -- ^ @Upd@ operation requires the following invariants (both of them):
    --   1. key which holds this ValueOp exists in a state,
    --   2. after aplication of entry (key, Upd v) to the state a value stored
    --      by @key@ is @v@.
    | Rem
    -- ^ @Rem@ operation requires the following invariants (both of them):
    --   1. key which holds this ValueOp exists in a state,
    --   2. after aplication of entry (key, Rem) to the state the @key@ doesn't
    -- exist in the state anymore.
    | NotExisted
    -- ^ @NotExisted@ operation requires the following invariant:
    -- key which holds this ValueOp doesn't exist in a state.
    -- This operation has no side effects with respect to the state.
    -- However, if invariant isn't hold an exception will be caused.
    deriving (Functor, Foldable, Traversable, Show, Eq, Ord, Generic)

instance Hashable v => Hashable (ValueOp v)

getVal :: ValueOp v -> v
getVal (New v) = v
getVal (Upd (Replace v)) = v
getVal _ = error "Can't get value"

instance HasPrism v v1 => HasReview (ValueOp v) (ValueOp v1) where
    inj (Upd (Replace x)) = Upd $ Replace (inj x)
    inj (Upd (Modify fs)) = Upd $ Modify $ map (\f -> fmap inj . f <=< proj) fs
    inj (New x)    = New (inj x)
    inj Rem        = Rem
    inj NotExisted = NotExisted

instance HasPrism v v1 => HasPrism (ValueOp v) (ValueOp v1) where
    proj Rem        = Just Rem
    proj NotExisted = Just NotExisted
    proj (New v)    = New <$> proj v
    proj (Upd (Replace v)) = (Upd . Replace) <$> proj v
    proj (Upd (Modify fs)) = Just $ Upd $ Modify $ map (\f -> proj <=< f . inj) fs

makeReplace :: v -> ValueOp v
makeReplace = Upd . Replace

-- | Auxiliary datatype to provide Semigroup instance for ValueOp.
-- ValueOp doesn't hold Semigroup laws, but it holds if there is
-- an additional constructor corresponding error during mappend.
data ValueOpEx v
    = Op (ValueOp v)
    | Err
    deriving (Functor, Show, Eq)

applyFList :: [v -> Maybe v] -> v -> ValueOpEx v
applyFList fs x = maybe Err (Op . New) (composeFList fs $ x)

instance Semigroup (ValueOpEx v) where
    Err <> _ = Err
    _ <> Err = Err

    Op NotExisted <> Op (Upd _) = Err
    Op NotExisted <> Op (New v) = Op (New v)
    Op NotExisted <> Op Rem     = Err

    Op (Upd _) <> Op NotExisted    = Err
    Op (New _) <> Op NotExisted    = Err
    Op Rem     <> Op NotExisted    = Op Rem
    Op NotExisted <> Op NotExisted = Op NotExisted

    Op Rem <> Op (New x) = Op $ Upd (Replace x)
    Op Rem <> Op Rem     = Err
    Op Rem <> Op (Upd _) = Err

    Op (New _) <> Op (New _) = Err
    Op (New _) <> Op Rem     = Op NotExisted
    Op (New _) <> Op (Upd (Replace y)) = Op $ New y
    -- This is important, any update after the New becomes a new New
    Op (New x) <> Op (Upd (Modify fs)) = applyFList fs x

    Op (Upd _) <> Op (New _) = Err
    Op (Upd _) <> Op Rem     = Op Rem

    Op (Upd _          ) <> Op (Upd y@(Replace _))  = Op $ Upd y
    -- This is important, any update after the Replace becomes a new New
    Op (Upd (Replace x)) <> Op (Upd (Modify fs))    = applyFList fs x
    Op (Upd (Modify  u)) <> Op (Upd (Modify     v)) = Op $ Upd $ Modify $ v ++ u

-- | Diff operation for value op:
-- @Op a <> Op b = Op c@ is true iff @c <-> a = DOp b@
(<->) :: ValueOp v -> ValueOp v -> Maybe (ValueOp v)

NotExisted <-> (New _)    = Just Rem        -- New _ <> Rem = NotExisted
NotExisted <-> NotExisted = Just NotExisted -- NotExisted <> NotExisted = NotExisted
NotExisted <-> _          = Nothing

(New v) <-> NotExisted    = Just (New v)    -- NotExisted <> New v = New v
(New y) <-> (New _)       = Just (Upd $ Replace y)    -- New x <> Upd y = New y
(New _) <-> _             = Nothing

Rem <-> Rem               = Just NotExisted -- Rem <> NotExisted = Rem
Rem <-> (Upd _)           = Just Rem        -- Upd x <> Rem = Rem
Rem <-> _                 = Nothing

(Upd (Replace v))   <-> Rem     = Just (New v) -- Rem <> New v = Upd v
(Upd y@(Replace _)) <-> (Upd _) = Just (Upd y) -- Upd _  <> (Upd y@(Replace _)) = Upd y
(Upd (Modify curr)) <-> (Upd (Modify prev)) = Just (Upd (Modify diff))
  where diff = take (length curr - length prev) curr
(Upd _)             <-> _       = Nothing
