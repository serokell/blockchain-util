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
  , chgAccumGetter
  , chgAccumIter
  , computeHChSetUndo
  , applyAll
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
import           Data.Vinyl.Class.Method ( PayloadType )
import           Data.Vinyl.Functor (Const (..))
import           Data.Vinyl.TypeLevel(RDelete, type (++))

import           Snowdrop.Hetero ( HKey, HMap, HSet, HSetEl (..), HMapEl (..), hsetFromSet, HMap, HVal, HKeyVal
                                 , OrdHKey )
import           Snowdrop.Core.ChangeSet ( HChangeSet, HChangeSetEl(..), CSMappendException, MappendHChSet
                                         , ValueOp(..), ValueOpEx(..), csNew
                                         , hChangeSetToHMap, hChangeSetToHSet
                                         , mappendChangeSet )

--------------------------------------------------------------
-- Vinyl has no somthing like this. Consider adding to Vinyl?
rzipWithMethod :: forall c f g h xs .
  ( RecMapMethod c (Product f g) xs
  , RZipWith xs )
  => (forall x . c (PayloadType (Product f g) x) => Product f g x -> h x) -> Rec f xs -> Rec g xs -> Rec h xs
rzipWithMethod zipper r1 r2 = rmapMethod @c zipper $ rzipWith Pair r1 r2

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

type family TxRaw (t :: u) :: *
newtype HTransElTy (t :: u) = HTransEl {unHTransEl :: TxRaw t}
type HTrans = Rec HTransElTy

data PreExpander uni db m et where
  PE :: Good uni et
    => {runPreExpander :: HTransElTy (T et) -> BaseM db m (HChangeSet (Outs et))} -> PreExpander uni db m et

type SeqExpander uni db m = Rec (PreExpander uni db m)

emptyChSet :: forall xs . RecApplicative xs => HChangeSet xs
emptyChSet = rpure @xs $ HChangeSetEl M.empty

type Expander db m uni = HTrans uni -> BaseM db m (HChangeSet uni)

liftPreExpander :: Functor m => PreExpander uni db m et -> Expander db m uni
liftPreExpander (PE f) ts = flip rreplace emptyChSet <$> f (rget ts)

type ExpanderX m uni = HTrans uni -> m (Either CSMappendException (HChangeSet uni))

seqPreExpanders :: (Monad m, LiftPEs xs, SeqE xs) => SeqExpander (Uni xs) db m xs -> ExpanderX (BaseM db m) (Uni xs)
seqPreExpanders = seqE . liftPEs
  where
    liftPEs xs = recordToList (rmap (Const . liftPreExpander) xs)
    seqE es r = foldM mappendChangeSet emptyChSet <$> sequence (map (\f -> f r) es)

runSeqExpander :: forall db m xs .
  ( SeqE xs
  , LiftPEs xs
  , Monad m )
  =>
  SeqExpander (Uni xs) db m xs
  -> ExpanderX (DBM db m) (Uni xs)
runSeqExpander se txs = unBaseM $ seqPreExpanders se txs

type PeType db m et = (Monad m, Good (InOuts et) et, StateQuery db m (Ins et)) => PreExpander (InOuts et) db m et
type PeFType db m et = HTransElTy (T et) -> BaseM db m (HChangeSet (Outs et))

-- test
data Tr; type instance TxRaw Tr = Int
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
type GetterType db m xs = HChangeSet xs -> QueryType db m xs

chgAccumGetter :: forall db m xs .
  ( Monad m
  , RecMapMethod OrdHKey (Product HMapEl HMapEl) xs
  , RZipWith xs )
  => GetterType db m xs -> HChangeSet xs -> GetterType db m xs
chgAccumGetter q chs = \acc ks ->
    rzipWithMethod @OrdHKey recombine (hChangeSetToHMap chs) <$> q acc ks
  where
    recombine (Pair (HMapEl m1) (HMapEl m2)) = HMapEl (M.difference m2 m1 <> M.intersection m1 m2)

-- Also don't quite understand if we need this, but still
-- Copypasted from existing SD and edited slightly
newtype IterAction db m t = IterAction {runIterAction :: forall b . IterType db m t b }
type DIter db m xs = Rec (IterAction db m) xs

chgAccumIter :: forall db m xs .
  ( Monad m
  , RecMapMethod OrdHKey (Product (IterAction db m) HChangeSetEl) xs
  , RZipWith xs )
  => DIter db m xs -> HChangeSet xs -> DIter db m xs
chgAccumIter iter =
    rzipWithMethod @OrdHKey chgAccumIter' iter
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

-- Reuse ValueOpEx
-- FIXME? Move to Snowdrop.Core.ChangeSet.ValueOp?
undo :: Maybe v -> ValueOp v -> ValueOpEx v
undo Nothing  NotExisted = Op NotExisted
undo Nothing  (New _)    = Op Rem
undo (Just v) Rem        = Op (New v)
undo (Just v) (Upd _)    = Op (Upd v)
undo _        _          = Err

computeHChSetUndo ::
  ( Monad m
  , RecMapMethod OrdHKey (Product HChangeSetEl HMapEl) xs
  , RZipWith xs )
  => GetterType db m xs
  -> HChangeSet xs
  -> HChangeSet xs
  -> DBM db m (HChangeSet xs)
computeHChSetUndo getter sch chs =
    computeHChSetUndo' <$> getter sch (hChangeSetToHSet chs)
  where
    computeHChSetUndo' = rzipWithMethod @OrdHKey computeUndo chs
    computeUndo (Pair (HChangeSetEl cs) (HMapEl vals)) =
      let processOne m (k, valueop) = case (undo (k `M.lookup` vals) valueop) of
            Op op -> M.insert k op m
            Err   -> error "CSMappendException"
      in HChangeSetEl $ foldl processOne M.empty (M.toList cs)

-- The simplest and dumbest applier. Use no fancy chgAccums. PoC.
applyAll ::
    ( RecMapMethod OrdHKey (Product HChangeSetEl HMapEl) uni
    , RZipWith uni)
    => HTrans uni -> ExpanderX (DBM (SimpleDBTy uni) IO) uni -> DBM (SimpleDBTy uni) IO ()
applyAll ts f = do
    chs <- doOrThrow <$> f ts
    SimpleDB hmapRef <- ask
    lift $ atomicModifyIORef hmapRef $ \hmap -> (rzipWithMethod @OrdHKey apply chs hmap, ())
  where
    doOrThrow (Left _) = error "FIXME: use proper exception"
    doOrThrow (Right cs) = cs
    apply (Pair (HChangeSetEl cs) (HMapEl vals)) = HMapEl $ M.foldlWithKey app vals cs
    app vals k c = case (c, k `M.lookup` vals) of
      (NotExisted, Nothing) -> vals
      (New v     , Nothing) -> M.insert k v vals
      (Rem,         Just _) -> M.delete k   vals
      (Upd v     ,  Just _) -> M.insert k v vals
      (_,                _) -> error "FIXME: use proper exception? or simply ignore?"
