{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType          #-}

{-# LANGUAGE GADTs #-}

module Snowdrop.Core.Expand.Type
       ( PrePreExpander (..)
       , PreExpander (..)
       , SeqExpander (..)
       , contramapSeqExpander
       , contramapPreExpander
       , runPreExpander
       , applyPreExpander
       , runSeqExpander
       , HChangeSetEl
       , test
       ) where

import           Universum hiding (Compose, getCompose, Const, Nat)

import           Data.Vinyl (Rec (..), RMap (..))
import           Data.Vinyl.Core (rtraverse, RecApplicative, rpure, recordToList, RecordToList)
import           Data.Vinyl.Lens (type (⊆), rcast, rreplace, RecSubset)
import           Data.Vinyl.Functor ((:.), Compose (..), Const (..))
import           Data.Vinyl.TypeLevel

import           Snowdrop.Core.ChangeSet (HChangeSetEl (..), HChangeSet, CSMappendException, MappendHChSet, mappendChangeSetEl)
import           Snowdrop.Core.ERoComp (ERoComp)

import           Snowdrop.Hetero (ExnHKey, HKeyVal)

import qualified Data.Map as M

-- FIXME: upgrade Vinyl (copied from fresh Vinyl)
rdowncast :: (RecApplicative ss, RMap rs, rs ⊆ ss)
              => Rec f rs -> Rec (Maybe :. f) ss
rdowncast = flip rreplace (rpure (Compose Nothing)) . rmap (Compose . Just)
--------------

-- Intersection
type family RIn r rs where
   RIn r (r ': rs) = 'Just r
   RIn r (s ': rs) = RIn r rs
   RIn r '[] = 'Nothing

type family RMbIntersect ls rs where
   RMbIntersect '[]       rs = '[]
   RMbIntersect (l ': ls) rs = RIn l rs ': RMbIntersect ls rs

type family RCatMaybes rs where
   RCatMaybes '[]              = '[]
   RCatMaybes ('Nothing ': rs) = RCatMaybes rs
   RCatMaybes ('Just r  ': rs) = r ': RCatMaybes rs

type RIntersect ls rs = RCatMaybes (RMbIntersect ls rs)

{- Another variant of intersection
type family NatLess (l :: Nat) (r :: Nat) :: Bool where
  NatLess 'Z     ('S m) = 'True
  NatLess ('S n)  'Z    = 'False
  NatLess ('S n) ('S m) = NatLess n m

type family RElemIn r rs where
   RElemIn r rs = NatLess (RIndex r rs) (RLength rs)

type family IfTf t r1 r2 where
   IfTf 'True  r1 r2 = r1
   IfTf 'False r1 r2 = r2

-- GHC choked on recursive If
type family PrependIf b x xs where
    PrependIf 'True  x xs = x ': xs
    PrependIf 'False x xs = xs

type family RIntersect ls rs where
   RIntersect '[]        rs = '[]
   RIntersect (l  ': ls) rs = PrependIf (RElemIn l rs) l (RIntersect ls rs)
-}

-- Should be automatically satisfied after all tyfams reductions at the call site
class (
    RecSubset Rec (RIntersect ts is) is (RImage (RIntersect ts is) is)
  , RecSubset Rec (RIntersect ts is) ts (RImage (RIntersect ts is) ts)
  , RMap (RIntersect ts is)
  ) => Sat ts is

instance (
    RecSubset Rec (RIntersect ts is) is (RImage (RIntersect ts is) is)
  , RecSubset Rec (RIntersect ts is) ts (RImage (RIntersect ts is) ts)
  , RMap (RIntersect ts is)
  ) => Sat ts is


-- Expanders
newtype PrePreExpander conf rawTx i
  = PPE { runPpeExpander :: rawTx -> ERoComp conf (HChangeSetEl i) }
--------------------------------------------------

-- We can put RMap constraint inside because
--   all the client code will always have explicit type lists which automatically give
--   us the requred instance!
data PreExpander conf rawTx ts is where
  PE :: (RMap is, Sat ts is) => Rec (PrePreExpander conf rawTx) is -> PreExpander conf rawTx ts is

-----------------
type HMbChangeSet ts = Rec (Maybe :. HChangeSetEl) ts

runPreExpander :: forall conf rawTx ts is . RecApplicative ts => rawTx -> PreExpander conf rawTx ts is -> ERoComp conf (HMbChangeSet ts)
runPreExpander tx (PE ppes) = (downcast . upcast) <$> rtraverse ( \(PPE rpe) -> rpe tx ) ppes
  where
    upcast :: Rec HChangeSetEl is -> HChangeSet (RIntersect ts is)
    upcast = rcast
    downcast :: RMap (RIntersect ts is) => HChangeSet (RIntersect ts is) -> HMbChangeSet ts
    downcast = rdowncast

contramapPreExpander :: forall a b conf ts is . (a -> b) -> PreExpander conf b ts is -> PreExpander conf a ts is
contramapPreExpander f (PE pe) = PE (rmap cmap pe)
  where
    cmap :: PrePreExpander conf b i -> PrePreExpander conf a i
    cmap (PPE act) = PPE (act . f)

data SeqExpander conf rawTx ts iss where
  SE :: (RecordToList iss, RMap iss, RecApplicative ts) => Rec (PreExpander conf rawTx ts) iss -> SeqExpander conf rawTx ts iss

runSeqExpander :: forall ts iss conf rawTx . SeqExpander conf rawTx ts iss -> rawTx -> [ERoComp conf (HMbChangeSet ts)]
runSeqExpander (SE pes) tx = recordToList $ rmap (\pe -> Const $ runPreExpander tx pe) pes

contramapSeqExpander :: (a -> b) -> SeqExpander conf b ts iss -> SeqExpander conf a ts iss
contramapSeqExpander f (SE pes) = SE (rmap (contramapPreExpander f) pes)

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

-- Example from 'Sequential'
applyPreExpander
    :: forall conf rawTx ts is .
    ( RecApplicative ts
    , MappendHChSet ts)
    => rawTx
    -> PreExpander conf rawTx ts is
    -> HMbChangeSet ts
    -> ERoComp conf (HMbChangeSet ts)
applyPreExpander tx ex sumCS = flip mappendMbChangeSet sumCS <$> runPreExpander tx ex

{- Universes. Might be flattened.
  uni1 :: [*]
  ...
  uniN :: [*]
  --
  type family FlattenUni :: [[*]] :: [*]
  type uni = FlattenUni [uni1, ..., uniN]
-}

data PreExpanderZ conf rawTx f uni zs where
  PEZ :: (RMap (Snd zs), Fst zs ⊆ uni, Snd zs ⊆ uni) => {runExpanderZ :: rawTx -> Rec f (Fst zs) -> ERoComp conf (HChangeSet (Snd zs))} -> PreExpanderZ conf rawTx f uni zs

newtype PreExpanderU conf rawTx f uni = PEU {runExpanderU :: rawTx -> Rec f uni -> ERoComp conf (HMbChangeSet uni)}

liftPEU :: forall uni zs conf rawTx f . (RecApplicative uni) => PreExpanderZ conf rawTx f uni zs -> PreExpanderU conf rawTx f uni
liftPEU (PEZ act) = PEU act' where act' t r = rdowncast <$> act t (rcast r)

liftPEZ2U :: forall uni zss conf rawTx f .
  ( RecApplicative uni
  , RMap zss
  , RecordToList zss
  ) => Rec (PreExpanderZ conf rawTx f uni) zss -> [PreExpanderU conf rawTx f uni]
liftPEZ2U zss = recordToList (rmap (Const . liftPEU) zss)

-- Deduce uni?
type family Concat (ls :: [[k]]) :: [k] where
  Concat '[] = '[]
  Concat (x ': xs) = x ++ Concat xs

type family Nub (xs :: [k]) where
  Nub '[] = '[]
  Nub (x ': xs) = x ': Nub (RDelete x xs)

type family Fsts (xs :: [(k,v)]) :: [k] where
  Fsts '[] = '[]
  Fsts (x ': xs) = Fst x ': Fsts xs

type family Snds (xs :: [(k,v)]) :: [v] where
  Snds '[] = '[]
  Snds (x ': xs) = Snd x ': Snds xs

type FFlattenUni zss = Nub (Concat (Fsts zss ++ Snds zss))

-- Is this usable?
liftPEZ2Ud ::
  ( RecApplicative (FFlattenUni zss)
  , RMap zss
  , RecordToList zss
  ) => Rec (PreExpanderZ conf rawTx f (FFlattenUni zss)) zss -> [PreExpanderU conf rawTx f (FFlattenUni zss)]
liftPEZ2Ud zss = recordToList (rmap (Const . liftPEU) zss)
--------------


-- Test if can write things in this style
data Fields = Name | Age | Sleeping | Master deriving Show
type TS = ['Name, 'Age, 'Sleeping]
type XS = ['Sleeping, 'Master]

type instance HKeyVal 'Name = '(Int, String)
type instance HKeyVal 'Age = '(Int, Double)
type instance HKeyVal 'Sleeping = '(Bool, Int)
type instance HKeyVal 'Master = '(String, String)

data Cfg = Cfg
data Tx = Tx

sleep :: PrePreExpander Cfg Tx 'Sleeping
sleep = PPE (\_ -> (return $ HChangeSetEl M.empty) )

master :: PrePreExpander Cfg Tx 'Master
master = PPE (\_ -> (return $ HChangeSetEl M.empty) )

s_n_m :: PreExpander Cfg Tx TS XS
s_n_m = PE $ sleep :& master :& RNil

se :: SeqExpander Cfg Tx TS '[XS]
se = SE $ s_n_m :& RNil

test = runSeqExpander se Tx

-- We can inline all this
se_inline :: SeqExpander Cfg Tx TS '[XS]
se_inline = SE $ PE
  (  PPE (const $ return $ HChangeSetEl M.empty)
  :& PPE (const $ return $ HChangeSetEl M.empty)
  :& RNil ) :& RNil

test_inline = runSeqExpander se_inline Tx
