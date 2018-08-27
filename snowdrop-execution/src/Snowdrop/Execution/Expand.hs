{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Execution.Expand
       ( expandRawTxs
       , expandUnionRawTxs
       ) where

import           Universum

import           Data.Default (Default (def))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

import           Snowdrop.Core (CSMappendException (..), ChgAccum, ChgAccumCtx (..),
                                DiffChangeSet (..), ERoComp, Expander (..), IdSumPrefixed (..),
                                SeqExpanders (..), SeqExpanders', StateTx (..),
                                TxProof, TxProof, mappendChangeSet, withModifiedAccumCtx)
import           Snowdrop.Execution.DbActions (SumChangeSet, accumToDiff, mappendStOrThrow,
                                               modifySumChgSet)
import           Snowdrop.Execution.Restrict (RestrictCtx, RestrictionInOutException, restrictCS,
                                              restrictDbAccess)
import           Snowdrop.Util

-- RawTxStates holds all needed states to compute state for the next expander.
data RawTxState id value = RawTxState
    { rtsSum      :: SumChangeSet id value -- mappends of all previous expanders for this tx
    , rtsExpState :: SumChangeSet id value -- mappends of all previous expanders for all previous txs
    }

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
-- (*) Formally: when (inpSet E_i) ∩ (outSet E_j) = ∅ for j >= i, for 1 <= i, j <= numbers of expanders.
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
-- But for expanders holds (*) so we just can remove from consideration expanding of E_j where j >= i.
-- So we get
--   (E1 tx1 `mappend` E2 tx1 ... `mappend` E_{i-1} tx1) `mappend`
--   (E1 tx2 `mappend` E2 tx2 ... `mappend` E_{i-1} tx2) `mappend` ...
--   (E1 tx_{k-1}             ... `mappend` E_{i-1} tx_{k-1}).
-- How can it be recomputed for E_{i+1} tx_k?
-- We can just mappend E_i tx1 to the first parentheses, E_i tx2 to the second and so on and so forth.
expandRawTxs
  :: forall rawTx e id txtype value ctx .
    ( Ord id
    , IdSumPrefixed id
    , HasExceptions e [CSMappendException id, RestrictionInOutException]
    , HasLens ctx (ChgAccumCtx ctx)
    , Default (ChgAccum ctx)
    , HasLens ctx RestrictCtx
    )
    => (rawTx -> TxProof txtype)
    -> [rawTx]
    -> SeqExpanders e id txtype value ctx rawTx
    -> ERoComp e id value ctx [StateTx id value txtype]
expandRawTxs mkProof rawTxs (getSeqExpanders -> exps) =
    -- Check whether expanding can be run in parallel.
    if checkParallelization exps then
        runExpandersInParallel (squashSeqExpanders exps) $ take (length rawTxs) $ repeat (RawTxState def def)
    else
        runSeqExpandersSequentially mkProof (zip rawTxs $ repeat exps)
  where
    intersectsInpOut :: Expander e id txtype value ctx rawTx -> Expander e id txtype value ctx rawTx -> Bool
    intersectsInpOut e1 e2 = not $ S.null $ S.intersection (inpSet e1) (outSet e2)

    -- Check that (inpSet E_i) ∩ (outSet E_j) = ∅ for j >= i, for 0 <= i, j < numbers of expanders.
    checkParallelization :: SeqExpanders' e id txtype value ctx rawTx -> Bool
    checkParallelization (x :| []) = intersectsInpOut x x
    checkParallelization (x :| xs) =
        if any (intersectsInpOut x) (x:xs) then False
        else checkParallelization $ NE.fromList xs

    runExpandersInParallel
        :: SeqExpanders' e id txtype value ctx rawTx
        -> [RawTxState id value]
        -> ERoComp e id value ctx [StateTx id value txtype]
    runExpandersInParallel (ex:|xs) prevRawTxStates =
      runExpanderOnAllTxsParallel >>= \stxSums ->
            -- We should apply portion of expanded data for next expander.
            -- So basically for each tx we accumulate all previous expanded changesets
            -- to be able run the next expander correctly.
            case xs of
              (ex1:xs1) ->
                  case computeAccumulatedCSs stxSums of
                    Left csex    -> throwLocalError csex
                    Right accCSs -> runExpandersInParallel (ex1 :| xs1) accCSs
              _ -> pure $ map (\(rtx, stxCS) -> StateTx (mkProof rtx) (accumToDiff stxCS)) . safeZip rawTxs $ stxSums

      where
        -- Accumulate changeset for previous step with changes from previous txs expanded by current expander.
        computeAccumulatedCSs
            :: [SumChangeSet id value]
            -> Either (CSMappendException id) [RawTxState id value]
        computeAccumulatedCSs = fmap reverse . foldM accM []

        accM :: [RawTxState id value]
             -> SumChangeSet id value
             -> Either (CSMappendException id) [RawTxState id value]
        accM [] sumCS
            = Right $ [RawTxState sumCS def] -- TODO in case when init state not empty, pass this init state to second argument
        accM a@(sumSt:_) newRtsSum = do
            newRtsExpState <- rtsExpState sumSt `modifySumChgSet` accumToDiff (rtsSum sumSt)
            pure $ RawTxState newRtsSum newRtsExpState : a

        runExpanderOnAllTxsParallel :: ERoComp e id value ctx [SumChangeSet id value]
        runExpanderOnAllTxsParallel =
            mconcat $
              map (fmap one . applyExpWithState) $
            safeZip prevRawTxStates rawTxs

        applyExpWithState
            :: (RawTxState id value, rawTx)
            -> ERoComp e id value ctx (SumChangeSet id value)
        applyExpWithState (RawTxState{..}, rawTx) =
            withModifiedAccumCtx (accumToDiff rtsExpState) $ applyExpander rawTx rtsSum ex

        safeZip :: [a] -> [b] -> [(a, b)]
        safeZip as bs =
            if length as /= length bs then error "safeZip: length of list isn't same"
            else zip as bs

expandUnionRawTxs
  :: forall rawTx e id txtype value ctx .
    ( Ord id
    , IdSumPrefixed id
    , HasExceptions e [CSMappendException id, RestrictionInOutException]
    , HasLens ctx (ChgAccumCtx ctx)
    , HasLens ctx RestrictCtx
    , Default (ChgAccum ctx)
    )
    => (rawTx -> (TxProof txtype, SeqExpanders e id txtype value ctx rawTx))
    -> [rawTx]
    -> ERoComp e id value ctx [StateTx id value txtype]
expandUnionRawTxs f txs = do
    let seqExps = zip txs (map (getSeqExpanders . view _2 . f) txs)
    let mkProof = view _1 . f
    runSeqExpandersSequentially mkProof seqExps

runSeqExpandersSequentially
  :: forall rawTx e id txtype value ctx .
    ( IdSumPrefixed id
    , Ord id
    , HasExceptions e [CSMappendException id, RestrictionInOutException]
    , HasLens ctx (ChgAccumCtx ctx)
    , HasLens ctx RestrictCtx
    , Default (ChgAccum ctx)
    )
    => (rawTx -> TxProof txtype)
    -> [(rawTx, SeqExpanders' e id txtype value ctx rawTx)]
    -> ERoComp e id value ctx [StateTx id value txtype]
runSeqExpandersSequentially mkProof txWithExp =
    reverse <$> evalStateT (foldM runExps [] txWithExp) def
  where
    runExps :: [StateTx id value txtype]
            -> (rawTx, SeqExpanders' e id txtype value ctx rawTx)
            -> StateT (SumChangeSet id value) (ERoComp e id value ctx) [StateTx id value txtype]
    runExps txs rtx = do
        chgAcc <- get
        stx <- lift $ withModifiedAccumCtx (accumToDiff chgAcc) (runSeqExpandersForTx rtx)
        mappendStOrThrow @e (txBody stx) $> (stx : txs)

    runSeqExpandersForTx :: (rawTx, SeqExpanders' e id txtype value ctx rawTx) -> ERoComp e id value ctx (StateTx id value txtype)
    runSeqExpandersForTx (tx, sexp) = StateTx (mkProof tx) . accumToDiff <$> foldM (applyExpander tx) def sexp

applyExpander
    :: ( IdSumPrefixed id
       , Ord id
       , HasExceptions e [CSMappendException id, RestrictionInOutException]
       , HasLens ctx RestrictCtx
       )
    => rawTx
    -> SumChangeSet id value
    -> Expander e id txtype value ctx rawTx
    -> ERoComp e id value ctx (SumChangeSet id value)
applyExpander tx sumCS ex = do
    DiffChangeSet diffCS <- restrictDbAccess (inpSet ex) $ expanderAct ex tx
    restrictCS (outSet ex) diffCS
    case sumCS `modifySumChgSet` diffCS of
        Left e      -> throwLocalError e
        Right newCS -> pure newCS

-- Auxiliary stuff for expanders

mappendExpander
    :: (HasException e (CSMappendException id), Ord id)
    => Expander e id txtype value ctx rawTx
    -> Expander e id txtype value ctx rawTx
    -> Expander e id txtype value ctx rawTx
mappendExpander a b = Expander
    (inpSet a `S.union` inpSet b)
    (outSet a `S.union` outSet b)
    act
  where
    act rtx = do
        resList :: [DiffChangeSet id value] <- fmap one (expanderAct a rtx) <> fmap one (expanderAct b rtx)
        case resList of
            (DiffChangeSet l):(DiffChangeSet r):[] -> eitherThrow $ DiffChangeSet <$> first inj (l `mappendChangeSet` r)
            _      -> error "impossibro"

squashSeqExpanders
    :: (Ord id, HasException e (CSMappendException id))
    => SeqExpanders' e id txtype value ctx rawTx -> SeqExpanders' e id txtype value ctx rawTx
squashSeqExpanders (x:|[]) = one x
squashSeqExpanders (a:|b:xs) =
    if inpOutIntersection then NE.cons a (squashSeqExpanders (b :| xs))
    else squashSeqExpanders (a `mappendExpander` b :| xs)
  where
    inpOutIntersection = not $ S.null $ S.intersection (outSet a) (inpSet b)
