{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}

module MicroFramework (
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
  , chgQuery
  , chgIter
  , computeHChSetUndo
  , addChange
  , commit
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
import           Data.Vinyl.Functor (Compose (..), Const (..))
import           Data.Vinyl.TypeLevel(RDelete, type (++))
import           Data.Vinyl.CoRec hiding (Op)

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
type DBM db m = StateT db m

-- We may parametrize BaseM by whatever monad we want (STM anyone?),
-- Since the client hasn't access to BaseM data constructor,
--   she never may inject anything into it.
newtype BaseM db m a = BaseM { unBaseM :: DBM db m a }
    deriving Functor

instance Monad m => Applicative (BaseM db m) where
  pure a = BaseM $ pure a
  BaseM a <*> BaseM b = BaseM $ a <*> b

instance Monad m => Monad (BaseM db m) where
  a >>= b = BaseM $ unBaseM a >>= unBaseM . b

type QueryType db m xs = HSet xs -> DBM db m (HMap xs)

class StateQuery db m xs | db -> xs where
  sQuery :: QueryType db m xs

query :: forall uni xs db m . (xs ⊆ uni, Monad m, RecApplicative uni, StateQuery db m uni) => HSet xs -> BaseM db m (HMap xs)
query req = BaseM $ rcast <$> (sQuery $ rreplace req (rpure @uni (HSetEl S.empty)))

type IterType db m t = forall b . b -> (b -> (HKey t, HVal t) -> b) -> DBM db m b

class StateIterator db m t where
  sIterator :: IterType db m t

iterator :: forall db m t b . (Monad m, StateIterator db m t) => b -> (b -> (HKey t, HVal t) -> b) -> BaseM db m b
iterator ini dofold = BaseM (sIterator @_ @_ @t ini dofold)

-- Deduce uni
type family Nub (xs :: [k]) where
  Nub '[] = '[]
  Nub (x ': xs) = x ': Nub (RDelete x xs)

data ExpanderTypes t ins outs = ExpanderTypes t ins outs
type family T      u where T      ('ExpanderTypes t ins outs) = t
type family Ins    u where Ins    ('ExpanderTypes t ins outs) = ins
type family Outs   u where Outs   ('ExpanderTypes t ins outs) = outs
type family InOuts u where InOuts ('ExpanderTypes t ins outs) = Nub (ins ++ outs)

type family AllTs xs where
  AllTs '[] = '[]
  AllTs (r ': rest) = T r ++ AllTs rest
type family Ts ts where Ts ts = Nub (AllTs ts)

-- Optimize (early Nub)
type family AllOfElist xs where
  AllOfElist '[] = '[]
  AllOfElist (r ': rest) = InOuts r ++ AllOfElist rest
type family Uni u where Uni ets = Nub (AllOfElist ets)

type Good uni ts et = (
    T et ∈ ts
  , Outs et ⊆ uni
  , RecApplicative uni
  , RMap (Outs et)
  )

type LiftPEs xs = (RecordToList xs, RMap xs)
type SeqE xs = (MappendHChSet (Uni xs), RecApplicative (Uni xs))

type family TxRaw (t :: u) :: *
newtype HTransElTy (t :: u) = HTransEl {unHTransEl :: TxRaw t}
type HTransUnion = CoRec HTransElTy

data PreExpander uni ts db m et where
  PE :: Good uni ts et
    => {runPreExpander :: HTransElTy (T et) -> BaseM db m (HChangeSet (Outs et))} -> PreExpander uni ts db m et

type SeqExpander uni ts db m = Rec (PreExpander uni ts db m)

emptyChSet :: forall xs . RecApplicative xs => HChangeSet xs
emptyChSet = rpure @xs $ HChangeSetEl M.empty

type Expander db m uni ts = HTransUnion ts -> BaseM db m (HChangeSet uni)

liftPreExpander ::
  ( RecApplicative ts
  , Monad m)
  => PreExpander uni ts db m et -> Expander db m uni ts
liftPreExpander (PE f) ts = f' (rget $ coRecToRec ts)
  where
    f' (Compose Nothing) = return emptyChSet
    f' (Compose (Just csm)) = flip rreplace emptyChSet <$> f csm

type ExpanderX m uni ts = HTransUnion ts -> m (Either CSMappendException (HChangeSet uni))

seqPreExpanders ::
  ( RecApplicative (Ts ts)
  , LiftPEs xs
  , SeqE xs
  , Monad m )
  => SeqExpander (Uni xs) (Ts ts) db m xs -> ExpanderX (BaseM db m) (Uni xs) (Ts ts)
seqPreExpanders = seqE . liftPEs
  where
    liftPEs xs = recordToList (rmap (Const . liftPreExpander) xs)
    seqE es r = foldM mappendChangeSet emptyChSet <$> sequence (map (\f -> f r) es)

runSeqExpander :: forall db m xs ts .
  ( RecApplicative (Ts ts)
  , LiftPEs xs
  , SeqE xs
  , Monad m )
  =>
  SeqExpander (Uni xs) (Ts ts) db m xs
  -> ExpanderX (DBM db m) (Uni xs) (Ts ts)
runSeqExpander se txs = unBaseM $ seqPreExpanders @ts se txs

type PeType db m et = (Monad m, Good (InOuts et) '[T et] et, StateQuery db m (Ins et)) => PreExpander (InOuts et) '[T et] db m et
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
      _m <- query (hsetFromSet @Comp1 $ S.singleton $ "gago" ++ show n)
      return RNil

-- SimpleDB -----------------
data SimpleDBTy comps = SimpleDB {
    sdbCommitted :: IORef (HMap comps)
  -- We will, perhaps, have lists of changesets, and cached undos, and somthing else, perhaps.
  -- The question is what should be handled by DBMS, and what by the application logic.
  -- We chose the simplest possible implementation ATM, will add extra things if necessary.
  , sdbPending :: Maybe (HChangeSet comps)
  }

query1 :: forall t xs . (t ∈ xs, Ord (HKey t)) => HMap xs -> HSetEl t -> HMapEl t
query1 hmap (HSetEl req) = HMapEl $ M.restrictKeys (unHMapEl $ rget @t hmap) req 

class (t ∈ xs, OrdHKey t) => Q xs t
instance (t ∈ xs, OrdHKey t) => Q xs t

queryAll :: forall (xs :: [*]) . RecMapMethod (Q xs) HSetEl xs => HMap xs -> HSet xs -> HMap xs
queryAll hmap = rmapMethod @(Q xs) (query1 hmap)

type GoodChgQuery xs =
  ( RecMapMethod OrdHKey (Product HMapEl HMapEl) xs
  , RZipWith xs )

chgQuery :: forall db m xs . (Monad m, GoodChgQuery xs)
  => QueryType db m xs -> HChangeSet xs -> QueryType db m xs
chgQuery q chs = \ks ->
    rzipWithMethod @OrdHKey recombine (hChangeSetToHMap chs) <$> q ks
  where
    recombine (Pair (HMapEl m1) (HMapEl m2)) = HMapEl (M.difference m2 m1 <> M.intersection m1 m2)

instance
  ( RecMapMethod (Q xs) HSetEl xs
  , GoodChgQuery xs )
  => StateQuery (SimpleDBTy xs) IO xs where
  sQuery req = do
    SimpleDB {..} <- get
    db <- lift $ readIORef sdbCommitted
    let qry = return . queryAll db
    (maybe qry (chgQuery qry) sdbPending) req

-- Copypasted from SD, edited
chgIter :: (OrdHKey t, Monad m) => IterType db m t -> HChangeSetEl t -> IterType db m t
chgIter iter ac'@(HChangeSetEl accum) = \initB foldF -> do
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
    (b, remainedNewKeys) <- iter (initB, csNew ac') newFoldF
    pure $ M.foldlWithKey' (curry . foldF) b remainedNewKeys

instance (t ∈ comps, OrdHKey t) => StateIterator (SimpleDBTy comps) IO t where
  sIterator ini f = do
    SimpleDB {..} <- get
    db <- lift $ readIORef sdbCommitted
    let
      iter :: forall b . b -> (b -> (HKey t, HVal t) -> b) -> DBM (SimpleDBTy comps) IO b
      iter ini' f' = return $ foldl f' ini' $ M.toList (unHMapEl $ rget @t db)
    case sdbPending of
      Nothing -> iter ini f
      Just hcs -> (chgIter iter (rget @t hcs)) ini f

-- Seems don't need this, but I keep it around for the time being just in case
newtype IterAction db m t = IterAction {runIterAction :: IterType db m t }
type DIter db m xs = Rec (IterAction db m) xs

chgIterH :: forall db m xs .
  ( Monad m
  , RecMapMethod OrdHKey (Product (IterAction db m) HChangeSetEl) xs
  , RZipWith xs )
  => DIter db m xs -> HChangeSet xs -> DIter db m xs
chgIterH iter = rzipWithMethod @OrdHKey chgIter' iter
  where
    chgIter' (Pair iter' ac) = IterAction (chgIter (runIterAction iter') ac)

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
  => QueryType db m xs
  -> HChangeSet xs
  -> DBM db m (HChangeSet xs)
computeHChSetUndo qry chs =
    computeHChSetUndo' <$> qry (hChangeSetToHSet chs)
  where
    computeHChSetUndo' = rzipWithMethod @OrdHKey computeUndo chs
    computeUndo (Pair (HChangeSetEl cs) (HMapEl vals)) =
      let processOne m (k, valueop) = case (undo (k `M.lookup` vals) valueop) of
            Op op -> M.insert k op m
            Err   -> error "FIXME: use proper exception"
      in HChangeSetEl $ foldl processOne M.empty (M.toList cs)

applyOne :: OrdHKey t => Product HChangeSetEl HMapEl t -> HMapEl t
applyOne (Pair (HChangeSetEl cs) (HMapEl thevals)) = HMapEl $ M.foldlWithKey app thevals cs
  where
    app vals k c = case (c, k `M.lookup` vals) of
      (NotExisted, Nothing) -> vals
      (New v     , Nothing) -> M.insert k v vals
      (Rem,         Just _) -> M.delete k   vals
      (Upd v     ,  Just _) -> M.insert k v vals
      (_,                _) -> error "FIXME: use proper exception? or simply ignore?"

type GoodApplyChs uni = 
  ( RecMapMethod OrdHKey (Product HChangeSetEl HMapEl) uni
  , RZipWith uni )

-- flip because of "oldest first"
addChange ::
  ( GoodApplyChs uni
  , MappendHChSet uni )
  => HChangeSet uni -> DBM (SimpleDBTy uni) IO ()
addChange cs = modify (\sdb -> sdb {sdbPending = Just $ maybe cs (doOrThrow . flip (mappendChangeSet) cs) (sdbPending sdb)})
  where
    doOrThrow (Left _) = error "FIXME: use proper exception"
    doOrThrow (Right v) = v

commit :: GoodApplyChs uni => DBM (SimpleDBTy uni) IO ()
commit = do
    SimpleDB {..} <- get
    let doCommit = maybe id (\pending -> rzipWithMethod @OrdHKey applyOne pending) sdbPending
    lift $ atomicModifyIORef sdbCommitted (embed doCommit)
  where embed f = \a -> (f a, ())
