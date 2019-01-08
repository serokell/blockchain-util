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

import qualified Data.Set as S
import qualified Data.Map as M

import           Data.Vinyl ( RMap (..), Rec (..)
                            , RecApplicative (..), RecordToList (..)
                            , RecMapMethod (..)
                            , rcast, rget, rreplace
                            , type (∈), type (⊆) )
import           Data.Vinyl.Functor (Compose (..), (:.), Const (..))
import           Data.Vinyl.TypeLevel(RDelete, type (++))

import           Snowdrop.Hetero ( HKey, HMap, HSet, HSetEl (..), HMapEl (..), hsetFromSet, HMap, HVal, HKeyVal
                                 , OrdHKey, ExnHKey)
import           Snowdrop.Core.ChangeSet ( HChangeSet, HChangeSetEl, CSMappendException, MappendHChSet
                                         -- Snowdrop.Core.ChangeSet.Type is patched to export this
                                         , mappendChangeSetEl )

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

type DBM db m = ReaderT db m

-- We may parametrize BaseM by whatever monad we want (STM anyone?),
-- Since the client hasn't access to BaseM data constructor,
--   she never may inject anything into it.
newtype BaseM db m a = BaseM { unBaseM :: DBM db m a }
    deriving Functor

instance Applicative m => Applicative (BaseM db m) where
  pure a = BaseM $ pure a
  BaseM a <*> BaseM b = BaseM $ a <*> b

instance Monad m => Monad (BaseM db m) where
  a >>= b = BaseM $ unBaseM a >>= unBaseM . b

class StateQuery db m xs where
  sQuery :: db -> HSet xs -> DBM db m (HMap xs)

query :: forall uni xs db m . (xs ⊆ uni, Monad m, RecApplicative uni, StateQuery db m uni) => HSet xs -> BaseM db m (HMap xs)
query req = BaseM $ do
  db <- ask
  m <- sQuery db $ rreplace req (rpure @uni (HSetEl S.empty))
  return (rcast m)

type KVTy t = (HKey t, HVal t)

class StateIterator db m t where
  sIterator :: db -> KVTy t -> (KVTy t -> KVTy t -> KVTy t) -> DBM db m (KVTy t)

iterator :: forall db m t . (Monad m, StateIterator db m t) => KVTy t -> (KVTy t -> KVTy t -> KVTy t) -> BaseM db m (KVTy t)
iterator ini dofold = BaseM (do db <- ask; sIterator @_ @_ @t db ini dofold)

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

data PreExpander uni db m et where
  PE :: Good uni et
    => {runPreExpander :: HTransElTy (T et) -> BaseM db m (HChangeSet (Outs et))} -> PreExpander uni db m et
type Expander db m uni = HTrans uni -> BaseM db m (HMbChangeSet uni)

type SeqExpander uni db m = Rec (PreExpander uni db m)

liftPreExpander :: Functor m => PreExpander uni db m et -> Expander db m uni
liftPreExpander (PE f) ts = rdowncast <$> f (rget ts)

seqPreExpanders :: (Monad m, LiftPEs xs, SeqE xs) => SeqExpander (Uni xs) db m xs -> Expander db m (Uni xs)
seqPreExpanders = seqE . liftPEs
  where
    liftPEs xs = recordToList (rmap (Const . liftPreExpander) xs)
    seqE es r = flatten <$> sequence (map (\f -> f r) es)
    flatten [] = rdowncast RNil
    flatten css = foldl1 mappendMbChangeSet css

runSeqExpander :: forall db m xs .
  ( SeqE xs
  , LiftPEs xs
  , Monad m )
  =>
  SeqExpander (Uni xs) db m xs
  -> HTrans (Uni xs)
  -> DBM db m (HMbChangeSet (Uni xs))
runSeqExpander se txs = unBaseM $ seqPreExpanders se txs

type PeType db m et = (Monad m, Good (InOuts et) et, StateQuery db m (Ins et)) => PreExpander (InOuts et) db m et
type PeFType db m et = HTransElTy (T et) -> BaseM db m (HChangeSet (Outs et))

-- test
data Tr; type instance HKeyVal Tr = '(Int, ())
data Comp1; type instance HKeyVal Comp1 = '(String, Int)
data Comp2; type instance HKeyVal Comp2 = '(Double, String)

type Test = 'ExpanderTypes Tr '[Comp1, Comp2] '[]

test :: forall db m . PeType db m Test
test = PE fun
  where
    fun :: PeFType db m Test
    fun (HTransEl n) = do
      _m <- query @(Ins Test) @('[Comp1]) (hsetFromSet $ S.singleton $ "gago" ++ show n)
      return RNil

-- SimpleDB -----------------
newtype SimpleDBTy comps = SimpleDB {unSimpleDB :: IORef (HMap comps)}

query1 :: forall t xs . (t ∈ xs, Ord (HKey t)) => HMap xs -> HSetEl t -> HMapEl t
query1 hmap (HSetEl req) = HMapEl $ M.restrictKeys (unHMapEl $ rget @t hmap) req 

class (t ∈ xs, OrdHKey t) => Q xs t
instance (t ∈ xs, OrdHKey t) => Q xs t

queryAll :: forall (xs :: [*]) . RecMapMethod (Q xs) HSetEl xs => HMap xs -> HSet xs -> HMap xs
queryAll hmap = rmapMethod @(Q xs) (query1 hmap)

instance RecMapMethod (Q xs) HSetEl xs => StateQuery (SimpleDBTy xs) IO xs where
  sQuery (SimpleDB hmapRef) req =
    flip queryAll req <$> lift (readIORef hmapRef)

instance t ∈ comps => StateIterator (SimpleDBTy comps) IO t where
  sIterator (SimpleDB hmapRef) ini f = do
    hmap <- lift $ readIORef hmapRef
    return $ foldl f ini $ M.toList (unHMapEl $ rget @t hmap)
