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
  , chgAccumQuery
  , chgAccumIter
  )
  where

import           Universum hiding (Compose, getCompose, Product, Const)
-- import           Prelude
-- import           Control.Monad.Reader

import qualified Data.Set as S
import qualified Data.Map as M
import           Data.Functor.Product (Product(..))

import           Data.Vinyl ( RMap (..), Rec (..)
                            , RecApplicative (..), RecordToList (..)
                            , RZipWith (..), RecMapMethod (..)
                            , rcast, rget, rreplace
                            , type (∈), type (⊆) )
import           Data.Vinyl.Functor (Compose (..), (:.), Const (..))
import           Data.Vinyl.TypeLevel(RDelete, type (++))

import           Snowdrop.Hetero ( HKey, HMap, HSet, HSetEl (..), HMapEl (..), hsetFromSet, HMap, HVal, HKeyVal
                                 , OrdHKey, ExnHKey)
import           Snowdrop.Core.ChangeSet ( HChangeSet, HChangeSetEl(..), CSMappendException, MappendHChSet
                                         , ValueOp(..), csNew
                                         , hChangeSetToHMap
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

type QueryType db m xs = HSet xs -> DBM db m (HMap xs)

class StateQuery db m xs where
  sQuery :: db -> QueryType db m xs

query :: forall uni xs db m . (xs ⊆ uni, Monad m, RecApplicative uni, StateQuery db m uni) => HSet xs -> BaseM db m (HMap xs)
query req = BaseM $ do
  db <- ask
  m <- sQuery db $ rreplace req (rpure @uni (HSetEl S.empty))
  return (rcast m)

type IterType db m t b = b -> (b -> (HKey t, HVal t) -> b) -> DBM db m b

class StateIterator db m t where
  sIterator :: db -> IterType db m t b

iterator :: forall db m t b . (Monad m, StateIterator db m t) => b -> (b -> (HKey t, HVal t) -> b) -> BaseM db m b
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

-- Don't quite understand if we need this, but implement just in case
chgAccumQuery :: forall db m xs .
  ( Monad m
  , RecMapMethod OrdHKey (Product HMapEl HMapEl) xs
  , RMap xs, RZipWith xs)
  => QueryType db m xs -> HChangeSet xs -> QueryType db m xs
chgAccumQuery q chs = \ks ->
    -- We have no rzipWithMethod, hence this
    rmapMethod @OrdHKey recombine . rzipWith Pair (hChangeSetToHMap chs) <$> q ks
  where
    recombine (Pair (HMapEl m1) (HMapEl m2)) = HMapEl (M.difference m2 m1 <> M.intersection m1 m2)

-- Also don't quite understand if we need this, but still
-- Copypasted from existing SD and edited slightly
newtype IterAction db m t = IterAction {runIterAction :: forall b . IterType db m t b }
type DIter db m xs = Rec (IterAction db m) xs

chgAccumIter :: forall db m xs .
  ( Monad m
  , RecMapMethod OrdHKey (Product (IterAction db m) HChangeSetEl) xs
  , RZipWith xs)
  => DIter db m xs -> HChangeSet xs -> DIter db m xs
chgAccumIter iter =
    rmapMethod @OrdHKey chgAccumIter' . rzipWith Pair iter
  where
    chgAccumIter' (Pair (IterAction iter') ac'@(HChangeSetEl accum)) =
      IterAction $ \initB foldF -> do
        let extractMin i m = do
                (k, v) <- M.lookupMin m
                (k, v) <$ guard (k <= i)
            newFoldF (b, newKeys) (i, val) =
                case (M.lookup i accum, extractMin i newKeys) of
                    (_,   Just (i', v')) -> newFoldF (foldF b (i', v'), M.deleteMin newKeys) (i, val)
                    (Nothing,         _) -> (foldF b (i, val) ,newKeys)
                    (Just Rem,        _) -> (b, newKeys)
                    (Just (Upd newV), _) -> (foldF b (i, newV), newKeys)

                    (Just NotExisted, _) -> (b, newKeys) -- TODO shall we throw error here ?
                    (Just (New _),    _) -> (b, newKeys) -- TODO shall we throw error here?
        (b, remainedNewKeys) <- iter' (initB, csNew ac') newFoldF
        pure $ M.foldlWithKey' (curry . foldF) b remainedNewKeys
