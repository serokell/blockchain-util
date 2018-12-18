{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE GADTs #-}

module Snowdrop.Core.Expand.TypeNew where

import           Universum hiding (Compose, getCompose, Const, Nat)

import           Data.Vinyl (Rec (..), RMap (..))
import           Data.Vinyl.Core (RecApplicative, rpure, recordToList, RecordToList, rzipWith, RZipWith)
import           Data.Vinyl.Lens (type (⊆), rcast, rreplace)
import           Data.Vinyl.Functor ((:.), Compose (..), Const (..))
import           Data.Vinyl.TypeLevel

import           Snowdrop.Core.ChangeSet (HChangeSetEl (..), HChangeSet, CSMappendException, MappendHChSet, mappendChangeSetEl)
import           Snowdrop.Core.ERoComp (ERoComp)

import           Snowdrop.Hetero (ExnHKey)

-- FIXME: upgrade Vinyl (copied from fresh Vinyl)
rdowncast :: (RecApplicative ss, RMap rs, rs ⊆ ss)
              => Rec f rs -> Rec (Maybe :. f) ss
rdowncast = flip rreplace (rpure (Compose Nothing)) . rmap (Compose . Just)
--------------
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

-----------
data PreExpander conf rawTx f uni zs where
  PE :: (
      RMap (Snd zs)
    , Fst zs ⊆ uni
    , Snd zs ⊆ uni
    ) => {runExpander :: rawTx -> Rec f (Fst zs) -> ERoComp conf (HChangeSet (Snd zs))}
           -> PreExpander conf rawTx f uni zs

newtype PreExpanderU conf rawTx f uni = PEU {runExpanderU :: rawTx -> Rec f uni -> ERoComp conf (HMbChangeSet uni)}

liftPE :: forall uni zs conf rawTx f . (RecApplicative uni) => PreExpander conf rawTx f uni zs -> PreExpanderU conf rawTx f uni
liftPE (PE act) = PEU act' where act' t r = rdowncast <$> act t (rcast r)

seqPEs :: (
    RecApplicative uni
  , MappendHChSet uni
  , RMap zss
  , RecordToList zss
  ) => Rec (PreExpander conf rawTx f uni) zss -> PreExpanderU conf rawTx f uni
seqPEs = seqPEU . liftPEs
  where
    liftPEs zss = recordToList (rmap (Const . liftPE) zss)
    seqPEU peus = PEU $ \t r -> flatten <$> sequence (map (\(PEU f) -> f t r) peus)
    flatten [] = rdowncast RNil
    flatten css = foldl1 mappendMbChangeSet css

-- Deduce uni
type family Nub (xs :: [k]) where
  Nub '[] = '[]
  Nub (x ': xs) = x ': Nub (RDelete x xs)

type family Concat (ls :: [[k]]) :: [k] where
  Concat '[] = '[]
  Concat (x ': xs) = x ++ Concat xs

-- Optimize (early Nub)
-- All kinds are successfully inferred: ( xs :: [([*], [*])] ) :: [*]
type family AllTypsOfPre xs where
  AllTypsOfPre '[] = '[]
  AllTypsOfPre ('(ins, outs) ': rest) = Nub (ins ++ outs) ++ AllTypsOfPre rest

type FFlattenUni zss = Nub (AllTypsOfPre zss)

-----
data ExpTypes t ioss = ExpTypes t ioss
type family T u where T ('ExpTypes t ioss) = t
type family IOSS u where IOSS ('ExpTypes t ioss) = ioss
type family IOSSUni u where IOSSUni ets = FFlattenUni (IOSS ets)

type U = ExpTypes * [([*],[*])]

-- The kind of `uni` is deduced but we document it
data SeqExpander conf f (uni :: U) where
  SE :: (
      MappendHChSet (IOSSUni uni)
    , RecApplicative (IOSSUni uni)
    , RMap (IOSS uni)
    , RecordToList (IOSS uni)
    ) => {unSE :: Rec (PreExpander conf (T uni) f (IOSSUni uni)) (IOSS uni)}
      -> SeqExpander conf f uni

-- type family Ts biguni where Ts '[] = '[]; Ts (t ': ts) = T t ': Ts ts
type family IOSSUnis biguni where
  IOSSUnis '[] = '[]
  IOSSUnis (t ': ts) = IOSSUni t ': IOSSUnis ts

type UltraFlat biguni = Nub (Concat (IOSSUnis biguni))

data TxAndState f uni = TAS {
    tasTx        :: T uni
  , tasFromState :: Rec f ((IOSSUni uni))
  }

newtype ERCUni conf uni = ERCU { unERCU :: ERoComp conf (HMbChangeSet (IOSSUni uni)) }

runE :: TxAndState f uni -> SeqExpander conf f uni -> ERCUni conf uni
runE (TAS tx fs) (SE pe) = ERCU $ runExpanderU (seqPEs pe) tx fs

-- The kind of `biguni` is deduced but we document it
runSeqExpanders ::
    RZipWith (biguni :: [U])
    => Rec (TxAndState f) biguni
    -> Rec (SeqExpander conf f) biguni
    -> Rec (ERCUni conf) biguni
runSeqExpanders ts xs = rzipWith runE ts xs

-- To flatten this out we shall repeat the things from the lower level.
-- Looks cumbersome.
-- FIXME!!! SIMPLIFY

type HMbMbChangeSet conf biguni = Rec ( Maybe :. ERCUni conf ) biguni

-- NOTE:
-- If we would have ExpTypes * [*] [*], i.e, a triple (transaction type, ins type list, outs type list),
--   we won't have to write an extra flatten step, but much more important thing is that the client's code
--   would be simpler, more natural and easier to write!
-- We might flatten out existing `ExpTypes` the other way around, like this:
--  OldExpTypes * [([*],[*])] -> [NewExpTypes * [*] [*]), replicating the first (transaction type) parameter
--  for each element of insouts list
-- But! It's much easier (less parens and grouping) when a client write the client code right in terms of 
--   NewExpTypes * [*] [*]
