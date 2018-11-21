{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveFunctor           #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Snowdrop.Core.Expand.Sequential
       ( ExpandRawTxsMode
       , ExpandOneTxMode
       , expandOneTx
       , runSeqExpandersSequentially
       , ProofNExp (..)
       , ExpandableTx
       , UnionSeqExpandersInps
       , UnionExpandersInps
       , UnionSeqExpanders
       ) where

import           Universum

import           Data.Default (Default (def))
import           Data.Vinyl (Rec (..), rget)
import           Data.Vinyl.TypeLevel (AllConstrained)

import           Snowdrop.Core.ChangeSet (CSMappendException (..), HChangeSet, HChangeSetEl,
                                          HUpCastableChSet, MappendHChSet, SumChangeSet (..),
                                          SumChangeSet (..), mappendChangeSet)
import           Snowdrop.Core.ERoComp (ChgAccum, ChgAccumCtx (..), Ctx, ERoCompM, HasBException,
                                        UpCastableERoM, convertEffect, modifyAccum,
                                        upcastEffERoCompM, withModifiedAccumCtxOne)
import           Snowdrop.Core.Expand.Type (DiffChangeSet (..), ExpInpComps, ExpOutComps,
                                            ExpRestriction (..), PreExpander (..), ProofNExp (..),
                                            SeqExpander, SeqExpanderComponents)
import           Snowdrop.Core.Transaction (SomeTx, StateTx (..), TxComponents, TxRaw)
import           Snowdrop.Hetero (Both, HCastable, HElem, SomeData (..), UnionTypes, applySomeData,
                                  castStrip, hupcast)
import           Snowdrop.Util (HasLens (..), OldestFirst (..), throwLocalError)

type ExpandRawTxsMode conf txtypes =
    ( HasBException conf CSMappendException
    , HasLens (Ctx conf) (ChgAccumCtx conf)
    , MappendHChSet (UnionSeqExpandersInps txtypes)

    , Default (ChgAccum conf)
    , Default (SumChangeSet (UnionSeqExpandersInps txtypes))
    )

type ExpandOneTxMode txtype =
    ( Default (SumChangeSet (TxComponents txtype))
    , MappendHChSet (TxComponents txtype)
    , RestrictTx (TxComponents txtype) txtype
    )

expandOneTx
    :: forall txtype conf .
    ( HasBException conf CSMappendException
    , ExpandOneTxMode txtype
    , HasLens (Ctx conf) (ChgAccumCtx conf)
    , Default (ChgAccum conf)
    )
    => ProofNExp conf txtype
    -> TxRaw txtype
    -> ERoCompM conf (TxComponents txtype) (StateTx txtype)
expandOneTx (ProofNExp (prf, sexp)) tx = do
    hcs <- unSumCS <$> runSeqExpanderForTx @txtype tx sexp
    pure $ StateTx (prf tx) hcs

runSeqExpandersSequentially
    :: forall txtypes (c :: * -> Constraint) conf .
    ( HasBException conf CSMappendException
    , HasLens (Ctx conf) (ChgAccumCtx conf)
    , MappendHChSet (UnionSeqExpandersInps txtypes)
    , Default (ChgAccum conf)
    , Default (HChangeSet (UnionSeqExpandersInps txtypes))
    )
    => Rec (ProofNExp conf) txtypes
    -> [SomeData TxRaw (Both (ExpandableTx txtypes) c)]
    -> ERoCompM conf (UnionSeqExpandersInps txtypes) (OldestFirst [] (SomeTx c, ChgAccum conf))
runSeqExpandersSequentially proofNExps allTxs =  do
      (someTxs, _) <- foldM runExps ([], def) allTxs
      let (fch, hch) = unzip someTxs
      modAccums :: OldestFirst [] (ChgAccum conf) <- modifyAccum (OldestFirst hch)
      pure $ OldestFirst (zip fch (unOldestFirst modAccums))

  where
    runExps
        :: ([(SomeTx c, HChangeSet (UnionSeqExpandersInps txtypes))], HChangeSet (UnionSeqExpandersInps txtypes))
        -> SomeData TxRaw (Both (ExpandableTx txtypes) c)
        -> ERoCompM conf (UnionSeqExpandersInps txtypes) ([(SomeTx c, HChangeSet (UnionSeqExpandersInps txtypes))], HChangeSet (UnionSeqExpandersInps txtypes))
    runExps (res, currentHcs) rtx = do
        (someTxAndChs, nextHcs) <- withModifiedAccumCtxOne currentHcs (applySomeData (fmap (second castStrip) . constructStateTx) rtx)
        pure (((someTxAndChs, nextHcs): res), nextHcs)

    constructStateTx
        :: forall txtype . (ExpandableTx txtypes txtype, c txtype)
        => TxRaw txtype
        -> ERoCompM conf (UnionSeqExpandersInps txtypes) (SomeTx c, HChangeSet (TxComponents txtype))
    constructStateTx tx = do
        let ProofNExp (prf, sexp) = rget @txtype proofNExps
        hcs <- unSumCS <$> runSeqExpanderForTx @txtype tx sexp
        pure (SomeData (StateTx @txtype (prf tx) hcs), hcs)

runSeqExpanderForTx
    :: forall txtype components conf .
    ( HasBException conf CSMappendException
    , Default (SumChangeSet (TxComponents txtype))
    , MappendHChSet (TxComponents txtype)
    , RestrictTx components txtype
    , HasLens (Ctx conf) (ChgAccumCtx conf)
    )
    => TxRaw txtype
    -> SeqExpander conf txtype
    -> ERoCompM conf components (SumChangeSet (TxComponents txtype))
runSeqExpanderForTx tx exps = runSeqExpanderForTxAll exps
  where
    runSeqExpanderForTxAll
        :: forall (rs :: [ExpRestriction [*] [*]]) .
           ( AllConstrained (RestrictIo components txtype) rs
           , MappendHChSet (TxComponents txtype)
           )
        => Rec (PreExpander conf (TxRaw txtype)) rs
        -> ERoCompM conf components (SumChangeSet (TxComponents txtype))
    runSeqExpanderForTxAll RNil           = pure def
    runSeqExpanderForTxAll exps'@(_ :& _) = runSeqExpanderForTx' exps'

    runSeqExpanderForTx'
        :: forall (rs :: [ExpRestriction [*] [*]]) r rs'.
        ( rs ~ (r ': rs')
        , AllConstrained (RestrictIo components txtype) rs
        , MappendHChSet (TxComponents txtype)
        )
        => Rec (PreExpander conf (TxRaw txtype)) rs
        -> ERoCompM conf components (SumChangeSet (TxComponents txtype))
    runSeqExpanderForTx' (ex :& rest) = do
        sm <- runSeqExpanderForTxAll rest
        upcastEffERoCompM @(ExpInpComps r) $ applyPreExpander @txtype tx ex sm

applyPreExpander
    :: forall txtype rawtx conf ioRestr .
    ( HasBException conf CSMappendException
    , HUpCastableChSet (ExpOutComps ioRestr) (TxComponents txtype)
    , MappendHChSet (TxComponents txtype)
    )
    => rawtx
    -> PreExpander conf rawtx ioRestr
    -> SumChangeSet (TxComponents txtype)
    -> ERoCompM conf (ExpInpComps ioRestr) (SumChangeSet (TxComponents txtype))
applyPreExpander tx ex sumCS = do
    DiffChangeSet diffCS <- convertEffect @conf $ runExpander ex tx
    case hupcast diffCS `mappendChangeSet` unSumCS sumCS of
        Left e      -> throwLocalError e
        Right newCS -> pure $ SumChangeSet newCS

-- These typeclass should be satisfied automatically if everything is correct
type family UnionExpandersInps (restrictions :: [ExpRestriction [*] [*]]) where
    UnionExpandersInps '[] = '[]
    UnionExpandersInps ('ExRestriction i _ ': xs) = UnionTypes i (UnionExpandersInps xs)

type family UnionSeqExpanders (txtypes :: [*]) where
    UnionSeqExpanders '[] = '[]
    UnionSeqExpanders (a ': xs) = UnionTypes (SeqExpanderComponents a) (UnionSeqExpanders xs)

type UnionSeqExpandersInps txtypes = UnionExpandersInps (UnionSeqExpanders txtypes)

class (
        HCastable HChangeSetEl (UnionSeqExpandersInps txtypes) (TxComponents txtype)
      , Default (SumChangeSet (TxComponents txtype))
      , MappendHChSet (TxComponents txtype)
      , RestrictTx (UnionSeqExpandersInps txtypes) txtype
      , HElem txtype txtypes
      )
      => ExpandableTx (txtypes :: [*]) (txtype :: *)
instance (
          HCastable HChangeSetEl (UnionSeqExpandersInps txtypes) (TxComponents txtype)
        , Default (SumChangeSet (TxComponents txtype))
        , MappendHChSet (TxComponents txtype)
        , RestrictTx (UnionSeqExpandersInps txtypes) txtype
        , HElem txtype txtypes
        )
        => ExpandableTx (txtypes :: [*]) (txtype :: *)

type RestrictTx xs txtype = AllConstrained (RestrictIo xs txtype) (SeqExpanderComponents txtype)

class ( HUpCastableChSet (ExpOutComps ioRestr) (TxComponents txtype)
      , UpCastableERoM (ExpInpComps ioRestr) xs
      ) => RestrictIo (xs :: [*]) (txtype :: *) (ioRestr :: ExpRestriction [*] [*])
instance ( HUpCastableChSet (ExpOutComps ioRestr) (TxComponents txtype)
      , UpCastableERoM (ExpInpComps ioRestr) xs
      ) => RestrictIo (xs :: [*]) (txtype :: *) (ioRestr :: ExpRestriction [*] [*])

-- Auxiliary stuff for expanders

-- -- RawTxStates holds all needed states to compute state for the next expander.
-- data RawTxState id value = RawTxState
--     { rtsSum      :: SumChangeSet id value -- mappends of all previous expanders for this tx
--     , rtsExpState :: SumChangeSet id value -- mappends of all previous expanders for all previous txs
--     }

-- mappendExpander
--     :: (HasBException conf (CSMappendException id), Ord id)
--     => PreExpander e id proof valuconf rawTx
--     -> PreExpander e id proof valuconf rawTx
--     -> PreExpander e id proof valuconf rawTx
-- mappendExpander a b = PreExpander
--     (inpSet a `S.union` inpSet b)
--     (outSet a `S.union` outSet b)
--     act
--   where
--     act rtx = do
--         resList :: [DiffChangeSet id value] <- fmap one (expanderAct a rtx) <> fmap one (expanderAct b rtx)
--         case resList of
--             (DiffChangeSet l):(DiffChangeSet r):[] -> eitherThrow $ DiffChangeSet <$> first inj (l `mappendChangeSet` r)
--             _      -> error "impossibro"

-- squashSeqExpanders
--     :: (Ord id, HasBException conf (CSMappendException id))
--     => SeqExpander' e id txtype valuconf rawTx -> SeqExpander' e id txtype valuconf rawTx
-- squashSeqExpanders (x:|[]) = one x
-- squashSeqExpanders (a:|b:xs) =
--     if inpOutIntersection then NE.cons a (squashSeqExpanders (b :| xs))
--     else squashSeqExpanders (a `mappendExpander` b :| xs)
--   where
--     inpOutIntersection = not $ S.null $ S.intersection (outSet a) (inpSet b)

-- | We have severals txs to expand using sequence of expanders.
-- We may run them *sequentually*: sequence of expanders is run on each tx.
-- So if we have 3 transactions (tx1, tx2, tx3) and 2 expanders (E1, E2), it will look like:
-- run E1 on tx1, run E2 on tx1,
-- run E1 on tx2, run E2 on tx2,
-- run E1 on tx3, run E2 on tx3.
-- Output of each expander is applied to state which will be used by the next expander.

-- We may run them *in parallel*: each expander is run in parallel on all transactions.
-- So if we have 3 transactions and 2 expanders, it will look like:
-- run E1 on tx1, run E1 on tx2, run E1 on tx3 in parallel
-- then
-- run E2 on tx1, run E2 on tx2, run E2 on tx3 in parallel
-- Running in parallel isn't always possible.
-- It's possible only when expanding of a transaction doesn't
-- depend on application expanded StateTx of previous transactions.
-- (*) Formally:
-- when (inpSet E_i) ∩ (outSet E_j) = ∅ for j >= i, for 1 <= i, j <= numbers of expanders.
-- If it isn't satisfied then running E_j on tx_k may produce changes
-- which affect expanding of E_i on tx_{k+1}. Basically we get data dependency
-- and we aren't able to run expanders in parallel.

-- Interesting point in parallel expanding is
-- how we compute changes which should be used by next expander.
-- Let's assume we have n expanders.
-- Let's denote expanding changeset by expander E_i on tx tx_k as E_i t_k.
-- Let's look at expanding of tx_k by E_i.
-- Obviously that state on which expanding of E_i tk_k should be
--   (E1 tx1 `mappend` E2 tx1 ... `mappend` En tx1) `mappend`
--   (E1 tx2 `mappend` E2 tx2 ... `mappend` En tx2) `mappend` ...
--   (E1 tx_{k-1}             ... `mappend` E_n tx_{k-1}).
-- How it looks when expanding is performing sequentially.
-- But for expanders holds (*) so we just can remove from consideration
-- expanding of E_j where j >= i.
-- So we get
--   (E1 tx1 `mappend` E2 tx1 ... `mappend` E_{i-1} tx1) `mappend`
--   (E1 tx2 `mappend` E2 tx2 ... `mappend` E_{i-1} tx2) `mappend` ...
--   (E1 tx_{k-1}             ... `mappend` E_{i-1} tx_{k-1}).
-- How can it be recomputed for E_{i+1} tx_k?
-- We can just mappend E_i tx1 to the first parentheses, E_i tx2 to the second and so on and so forth.
-- TODO Must be generialized for several expanders
-- expandRawTxs
--   :: forall rawTx e id txtype valuconf .
--     ( Ord id
--     , IdSumPrefixed id
--     , HasExceptions e [CSMappendException id, RestrictionInOutException]
--     , HasLens ctx (ChgAccumCtx conf)
--     , Default (ChgAccum conf)
--     , HasLens ctx RestrictCtx
--     )
--     => [(rawtx, SomeData (ProofNExp conf rawtx) c)]
--     -> ERoCompM conf id valuconf [SomeTx id value c]
-- expandRawTxs mkProof rawTxs (getSeqExpanders -> exps) =
--     -- Check whether expanding can be run in parallel.
--     if checkParallelization expander then
--         runExpandersInParallel (squashSeqExpanders expander) $ take (length rawTxs) $ repeat (RawTxState def def)
--     else
--         runSeqExpandersSequentially mkProof (zip rawTxs $ repeat expander)
--   where
--     intersectsInpOut :: PreExpander e id txtype valuconf rawTx -> PreExpander e id txtype valuconf rawTx -> Bool
--     intersectsInpOut e1 e2 = not $ S.null $ S.intersection (inpSet e1) (outSet e2)

--     -- Check that (inpSet E_i) ∩ (outSet E_j) = ∅ for j >= i, for 0 <= i, j < numbers of expanders.
--     checkParallelization :: SeqExpander' e id txtype valuconf rawTx -> Bool
--     checkParallelization (x :| []) = intersectsInpOut x x
--     checkParallelization (x :| xs) =
--         if any (intersectsInpOut x) (x:xs) then False
--         else checkParallelization $ NE.fromList xs

--     runExpandersInParallel
--         :: SeqExpander' e id txtype valuconf rawTx
--         -> [RawTxState id value]
--         -> ERoCompM conf id valuconf [StateTx id value txtype]
--     runExpandersInParallel (ex:|xs) prevRawTxStates =
--       runExpanderOnAllTxsParallel >>= \stxSums ->
--             -- We should apply portion of expanded data for next expander.
--             -- So basically for each tx we accumulate all previous expanded changesets
--             -- to be able run the next expander correctly.
--             case xs of
--               (ex1:xs1) ->
--                   case computeAccumulatedCSs stxSums of
--                     Left csex    -> throwLocalError csex
--                     Right accCSs -> runExpandersInParallel (ex1 :| xs1) accCSs
--               _ -> pure $ map (\(rtx, stxCS) -> StateTx (mkProof rtx) (accumToDiff stxCS)) . safeZip rawTxs $ stxSums

--       where
--         -- Accumulate changeset for previous step with changes from previous txs expanded by current expander.
--         computeAccumulatedCSs
--             :: [SumChangeSet id value]
--             -> Either (CSMappendException id) [RawTxState id value]
--         computeAccumulatedCSs = fmap reverse . foldM accM []

--         accM :: [RawTxState id value]
--              -> SumChangeSet id value
--              -> Either (CSMappendException id) [RawTxState id value]
--         accM [] sumCS
--             = Right $ [RawTxState sumCS def] -- TODO in case when init state not empty, pass this init state to second argument
--         accM a@(sumSt:_) newRtsSum = do
--             newRtsExpState <- rtsExpState sumSt `modifySumChgSet` accumToDiff (rtsSum sumSt)
--             pure $ RawTxState newRtsSum newRtsExpState : a

--         runExpanderOnAllTxsParallel :: ERoCompM conf id valuconf [SumChangeSet id value]
--         runExpanderOnAllTxsParallel =
--             mconcat $
--               map (fmap one . applyExpWithState) $
--             safeZip prevRawTxStates rawTxs

--         applyExpWithState
--             :: (RawTxState id value, rawTx)
--             -> ERoCompM conf id valuconf (SumChangeSet id value)
--         applyExpWithState (RawTxState{..}, rawTx) =
--             withModifiedAccumCtx (accumToDiff rtsExpState) $ applyExpander rawTx rtsSum ex

--         safeZip :: [a] -> [b] -> [(a, b)]
--         safeZip as bs =
--             if length as /= length bs then error "safeZip: length of list isn't same"
--             else zip as bs
