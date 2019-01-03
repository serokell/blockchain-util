{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}

module Snowdrop.Core.MicroFramework (
    BaseM -- ABSTRACT!
  , StateQuery (..)
  , query
  , StateIterator (..)
  , iterator
  , PreExpander (..)
  , runSeqExpander
  , InOuts
  , PeType
  , PeFType
  , test
  )
  where

import           Universum hiding (Compose, getCompose, Const)
-- import           Prelude
-- import           Control.Monad.Reader

import           Data.Vinyl ( RMap (..), Rec (..), type RecSubset
                            , RecApplicative (..), RecordToList (..)
                            , rcast, rreplace
                            , type (∈), type (⊆) )
import           Data.Vinyl.Functor (Compose (..), (:.), Const (..))
import           Data.Vinyl.TypeLevel(RDelete, type RImage, type (++))

import           Snowdrop.Hetero (HKey, HMap, HSet, hsetFromSet, HMap, HVal, HKeyVal, ExnHKey)
import           Snowdrop.Core.ChangeSet ( HChangeSet, HChangeSetEl, CSMappendException, MappendHChSet
                                         -- Snowdrop.Core.ChangeSet.Type is patched to export this
                                         , mappendChangeSetEl )

import qualified Data.Set as S


-- FIXME: upgrade Vinyl (copied from fresh Vinyl) ------------
rdowncast :: (RecApplicative ss, RMap rs, rs ⊆ ss)
              => Rec f rs -> Rec (Maybe :. f) ss
rdowncast = flip rreplace (rpure (Compose Nothing)) . rmap (Compose . Just)
--------------------------------------------------------------

-- FIXEM: Move to Snowdrop.Core.ChangeSet --------------------
type HMbChangeSet ts = Rec (Maybe :. HChangeSetEl) ts

mappendMbChangeSetEl
    :: ExnHKey t
    => Maybe (HChangeSetEl t)
    -> Maybe (HChangeSetEl t)
    -> Either CSMappendException ((Maybe :. HChangeSetEl) t)
mappendMbChangeSetEl Nothing Nothing    = Right (Compose Nothing)
mappendMbChangeSetEl Nothing r@(Just _) = Right (Compose r)
mappendMbChangeSetEl l@(Just _) Nothing = Right (Compose l)
mappendMbChangeSetEl (Just l) (Just r)  = (Compose . Just) <$> mappendChangeSetEl l r

mappendMbChangeSet
    :: MappendHChSet ts
    => HMbChangeSet ts
    -> HMbChangeSet ts
    -> HMbChangeSet ts
mappendMbChangeSet RNil RNil = RNil
mappendMbChangeSet (x :& xs) (y :& ys) =
  case (getCompose x) `mappendMbChangeSetEl` (getCompose y) of
    Left _    -> error "FIXME: fix kind of throwLocalError and use it!"
    Right res -> res :& mappendMbChangeSet xs ys
--------------------------------------------------------------

type DBIO db = ReaderT db IO

newtype BaseM db a = BaseM { unBaseM :: DBIO db a }
    deriving Functor

instance Applicative (BaseM db) where
  pure a = BaseM $ pure a
  BaseM a <*> BaseM b = BaseM $ a <*> b

instance Monad (BaseM db) where
  a >>= b = BaseM $ unBaseM a >>= unBaseM . b

class StateQuery db uni where
  sQuery :: xs ⊆ uni => db -> HSet xs -> DBIO db (HMap xs)

query :: forall uni xs db . (StateQuery db uni, xs ⊆ uni) => HSet xs -> BaseM db (HMap xs)
query req = BaseM (do db <- ask; sQuery @_ @uni db req)

type KV t = (HKey t, HVal t)

class StateIterator db uni where
  sIterator :: t ∈ uni => db -> KV t -> (KV t -> KV t -> KV t) -> DBIO db (KV t)

iterator :: forall uni t db . (StateIterator db uni, t ∈ uni) => KV t -> (KV t -> KV t -> KV t) -> BaseM db (KV t)
iterator ini dofold = BaseM (do db <- ask; sIterator @db @uni @t db ini dofold)

-- Deduce uni
type family Nub (xs :: [k]) where
  Nub '[] = '[]
  Nub (x ': xs) = x ': Nub (RDelete x xs)

data ExpanderTypes t ins outs = ExpanderTypes t ins outs
type family Ins    u where Ins    ('ExpanderTypes t ins outs) = t ': ins
type family Outs   u where Outs   ('ExpanderTypes t ins outs) = outs
type family InOuts u where InOuts ('ExpanderTypes t ins outs) = Nub (t ': ins ++ outs)

-- Optimize (early Nub)
type family AllOfElist xs where
  AllOfElist '[] = '[]
  AllOfElist (r ': rest) = InOuts r ++ AllOfElist rest
type family Uni u where Uni ets = Nub (AllOfElist ets)

-- Obviously trivially satisfied
-- instance (uni ~ Uni xs) => RecSubset Rec uni uni (RImage uni uni) where
type Trivial xs =
  RecSubset Rec (Uni xs) (Uni xs) (RImage (Uni xs) (Uni xs))

type Good uni et = (
    Ins et ⊆ uni
  , Outs et ⊆ uni
  , RecApplicative uni
  , RMap (Outs et)
  )

type LiftPEs xs = (RecordToList xs, RMap xs)
type SeqE xs = (MappendHChSet (Uni xs), RecApplicative (Uni xs))

newtype HTransElTy (t :: u) = HTransEl {unHTransEl :: HKey t}
type HTrans = Rec HTransElTy

data PreExpander uni db et where
  PE :: (
      Good uni et
    , StateQuery db uni )
    => {runPreExpander :: HTrans (Ins et) -> BaseM db (HChangeSet (Outs et))} -> PreExpander uni db et
type Expander db uni = HTrans uni -> BaseM db (HMbChangeSet uni)

type SeqExpander uni db = Rec (PreExpander uni db)

liftPreExpander :: PreExpander uni db et -> Expander db uni
liftPreExpander (PE f) ts = rdowncast <$> f (rcast ts)

seqPreExpanders :: (LiftPEs xs, SeqE xs) => SeqExpander (Uni xs) db xs -> Expander db (Uni xs)
seqPreExpanders = seqE . liftPEs
  where
    liftPEs xs = recordToList (rmap (Const . liftPreExpander) xs)
    seqE es r = flatten <$> sequence (map (\f -> f r) es)
    flatten [] = rdowncast RNil
    flatten css = foldl1 mappendMbChangeSet css

runSeqExpander :: forall db xs .
  ( SeqE xs
  , LiftPEs xs
  , Trivial xs )
  =>
  SeqExpander (Uni xs) db xs
  -> HTrans (Uni xs)
  -> DBIO db (HMbChangeSet (Uni xs))
runSeqExpander se txs = unBaseM $ seqPreExpanders se txs

type PeType db et = (Good (InOuts et) et, StateQuery db (InOuts et)) => PreExpander (InOuts et) db et
type PeFType db et = HTrans (Ins et) -> BaseM db (HChangeSet (Outs et))

-- test
data T; type instance HKeyVal T = '(Int, ())
data Comp1; type instance HKeyVal Comp1 = '(String, Int)
data Comp2; type instance HKeyVal Comp2 = '(Double, String)

type Test = 'ExpanderTypes T '[Comp1, Comp2] '[]

test :: forall db . PeType db Test
test = PE fun
  where
    fun :: PeFType db Test
    fun (_ :& sd :& _) = do
      _m <- query @(InOuts Test) @('[Comp1]) (hsetFromSet $ S.singleton $ unHTransEl sd)
      return RNil
