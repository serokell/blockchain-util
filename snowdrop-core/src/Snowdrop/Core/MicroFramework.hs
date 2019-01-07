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

import           Data.Vinyl ( RMap (..), Rec (..)
                            , RecApplicative (..), RecordToList (..)
                            , rcast, rget, rreplace
                            , type (∈), type (⊆) )
import           Data.Vinyl.Functor (Compose (..), (:.), Const (..))
import           Data.Vinyl.TypeLevel(RDelete, type (++))

import           Snowdrop.Hetero (HKey, HMap, HSet, HSetEl (..), hsetFromSet, HMap, HVal, HKeyVal, ExnHKey)
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

class StateQuery db xs where
  sQuery :: db -> HSet xs -> DBIO db (HMap xs)

query :: forall uni xs db . (xs ⊆ uni, RecApplicative uni, StateQuery db uni) => HSet xs -> BaseM db (HMap xs)
query req = BaseM $ do
  db <- ask
  m <- sQuery db $ rreplace req (rpure @uni (HSetEl S.empty))
  return (rcast m)

type KVTy t = (HKey t, HVal t)

class StateIterator db t where
  sIterator :: db -> KVTy t -> (KVTy t -> KVTy t -> KVTy t) -> DBIO db (KVTy t)

iterator :: forall db t . StateIterator db t => KVTy t -> (KVTy t -> KVTy t -> KVTy t) -> BaseM db (KVTy t)
iterator ini dofold = BaseM (do db <- ask; sIterator @_ @t db ini dofold)

-- Deduce uni
type family Nub (xs :: [k]) where
  Nub '[] = '[]
  Nub (x ': xs) = x ': Nub (RDelete x xs)

data ExpanderTypes t ins outs = ExpanderTypes t ins outs
type family T      u where T      ('ExpanderTypes t ins outs) = t
type family Ins    u where Ins    ('ExpanderTypes t ins outs) = ins
type family Outs   u where Outs   ('ExpanderTypes t ins outs) = outs
type family InOuts u where InOuts ('ExpanderTypes t ins outs) = Nub (t ': ins ++ outs)

-- Optimize (early Nub)
type family AllOfElist xs where
  AllOfElist '[] = '[]
  AllOfElist (r ': rest) = InOuts r ++ AllOfElist rest
type family Uni u where Uni ets = Nub (AllOfElist ets)

type Good uni et = (
    T et ∈ uni
  , Outs et ⊆ uni
  , RecApplicative uni
  , RMap (Outs et)
  )

type LiftPEs xs = (RecordToList xs, RMap xs)
type SeqE xs = (MappendHChSet (Uni xs), RecApplicative (Uni xs))

newtype HTransElTy (t :: u) = HTransEl {unHTransEl :: HKey t}
type HTrans = Rec HTransElTy

data PreExpander uni db et where
  PE :: Good uni et
    => {runPreExpander :: HTransElTy (T et) -> BaseM db (HChangeSet (Outs et))} -> PreExpander uni db et
type Expander db uni = HTrans uni -> BaseM db (HMbChangeSet uni)

type SeqExpander uni db = Rec (PreExpander uni db)

liftPreExpander :: PreExpander uni db et -> Expander db uni
liftPreExpander (PE f) ts = rdowncast <$> f (rget ts)

seqPreExpanders :: (LiftPEs xs, SeqE xs) => SeqExpander (Uni xs) db xs -> Expander db (Uni xs)
seqPreExpanders = seqE . liftPEs
  where
    liftPEs xs = recordToList (rmap (Const . liftPreExpander) xs)
    seqE es r = flatten <$> sequence (map (\f -> f r) es)
    flatten [] = rdowncast RNil
    flatten css = foldl1 mappendMbChangeSet css

runSeqExpander :: forall db xs .
  ( SeqE xs
  , LiftPEs xs )
  =>
  SeqExpander (Uni xs) db xs
  -> HTrans (Uni xs)
  -> DBIO db (HMbChangeSet (Uni xs))
runSeqExpander se txs = unBaseM $ seqPreExpanders se txs

type PeType db et = (Good (InOuts et) et, StateQuery db (Ins et)) => PreExpander (InOuts et) db et
type PeFType db et = HTransElTy (T et) -> BaseM db (HChangeSet (Outs et))

-- test
data Tr; type instance HKeyVal Tr = '(Int, ())
data Comp1; type instance HKeyVal Comp1 = '(String, Int)
data Comp2; type instance HKeyVal Comp2 = '(Double, String)

type Test = 'ExpanderTypes Tr '[Comp1, Comp2] '[]

test :: forall db . PeType db Test
test = PE fun
  where
    fun :: PeFType db Test
    fun (HTransEl n) = do
      _m <- query @(Ins Test) @('[Comp1]) (hsetFromSet $ S.singleton $ "gago" ++ show n)
      return RNil
