{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}

module Snowdrop.Block.Application
       ( applyBlock
       , applyFork
       , processFork
       , BlockApplicationException (..)
       , OpenBlockRawTx (..)
       , CloseBlockRawTx (..)
       ) where

import           Universum

import           Data.Default (Default)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))

import           Snowdrop.Block.Configuration (BlkConfiguration (..), unBIV)
import           Snowdrop.Block.Fork (ForkVerResult (..), ForkVerificationException (..),
                                      verifyFork)
import           Snowdrop.Block.State (BlkProcConstr, BlundComponent, TipComponent, TipKey (..),
                                       TipValue (..))
import           Snowdrop.Block.StateConfiguration (BlkStateConfiguration (..))
import           Snowdrop.Block.Types (Block (..), BlockHeader, BlockRef, BlockUndo, Blund (..),
                                       CurrentBlockRef (..), OSParams, Payload, PrevBlockRef (..),
                                       RawBlk, RawBlund, RawPayload, Tx)
import           Snowdrop.Core (CSMappendException, ChgAccum, ChgAccumCtx, Ctx, DbAccessU, ERoComp,
                                ERwComp, HChangeSet, HUpCastableChSet, HasBExceptions,
                                MappendHChSet, QueryERo, SomeTx, StatePException (..), StateTx (..),
                                Undo, ValueOp (..), applySomeTx, computeUndo, hChangeSetFromMap,
                                liftERoComp, mconcatChangeSets, modifyAccum, modifyAccumOne,
                                modifyAccumUndo, queryOne)
import           Snowdrop.Util (DBuildable, HasReview (inj), HasReviews, OldestFirst (..), maybeF,
                                throwLocalError, unNewestFirst)

-- | Exception type for block application.
data BlockApplicationException blockRef
    = TipMismatched (Maybe blockRef) (Maybe blockRef)
    -- ^ Expected one tip value, but encountered another.
    | BlockIntegrityVerifierFailed
    -- ^ Block integrity verification produced negative result.
    deriving (Show)

instance Buildable blockRef => Buildable (BlockApplicationException blockRef) where
    build = \case
        TipMismatched given expected ->
            bprint ("Wrong reference to previous block, \
                    \given "%maybeF build%", expected (tip) "%maybeF build)
              given expected
        BlockIntegrityVerifierFailed ->
            "Block integrity verification failed"

data ForkApplyAction chgAccum blkType = ForkApplyAction
    { faaRollbackTo  :: BlockRef blkType
    , faaApplyBlocks :: OldestFirst [] (Block (BlockHeader blkType) (Payload blkType), chgAccum)
    }

-- | Applies an individual block if it's a direct continuation of currently adopted "best" chain.
-- Current implementation checks only block integrity,
-- no comparison with `bcIsBetterThan` is performed (which is better to be changed).
applyBlock
    :: forall blkType conf m txtypes xs
    . ( Eq (BlockRef blkType)
      , HasBExceptions conf
        [ BlockApplicationException (BlockRef blkType)
        , StatePException
        , CSMappendException
        ]
      , HasGetter (RawBlk blkType) (RawPayload blkType)
      , HasGetter (Undo conf) (BlockUndo blkType)
      , HasLens (Ctx conf) (ChgAccumCtx conf)
      , HUpCastableChSet '[TipComponent blkType] xs
      , HUpCastableChSet '[BlundComponent blkType] xs
      , QueryERo xs (TipComponent blkType)
      , MappendHChSet xs
      , Default (HChangeSet xs)
      , Tx blkType ~ SomeTx (BlkProcConstr txtypes xs)
      , m ~ ERwComp conf (DbAccessU conf xs) (ChgAccum conf)
      )
    => OSParams blkType
    -> BlkStateConfiguration (ChgAccum conf) blkType m
    -> RawBlk blkType
    -> m ()
-- TODO: compare old chain with new one via `bcIsBetterThan`
applyBlock = expandAndApplyBlock True

newtype OpenBlockRawTx blkType  = OpenBlockRawTx  { unOpenBlockRawTx :: BlockHeader blkType }
newtype CloseBlockRawTx blkType = CloseBlockRawTx { unCloseBlockRawTx :: BlockHeader blkType }

instance Buildable (CloseBlockRawTx h) where
    build _ = "Block close tx"
instance DBuildable (CloseBlockRawTx h)

instance Buildable (OpenBlockRawTx h) where
    build _ = "Block open tx"
instance DBuildable (OpenBlockRawTx h)

deriving instance Hashable (BlockHeader blkType) => Hashable (OpenBlockRawTx blkType)
deriving instance Hashable (BlockHeader blkType) => Hashable (CloseBlockRawTx blkType)

expandAndApplyBlock
    :: forall blkType conf m txtypes xs
    . ( Eq (BlockRef blkType)
      , HasBExceptions conf
        [ BlockApplicationException (BlockRef blkType)
        , StatePException
        , CSMappendException
        ]
      , HasReview (BlockRawTx blkType) (OpenBlockRawTx blkType)
      , HasReview (BlockRawTx blkType) (CloseBlockRawTx blkType)
      , HasGetter (RawBlk blkType) (RawPayload blkType)
      , HasGetter (Undo conf) (BlockUndo blkType)
      , HasLens (Ctx conf) (ChgAccumCtx conf)
      , HUpCastableChSet '[TipComponent blkType] xs
      , HUpCastableChSet '[BlundComponent blkType] xs
      , QueryERo xs (TipComponent blkType)
      , MappendHChSet xs
      , Default (HChangeSet xs)
      , Tx blkType ~ SomeTx (BlkProcConstr txtypes xs)
      , m ~ ERwComp conf (DbAccessU conf xs) (ChgAccum conf)
      )
    => Bool
    -> OSParams blkType
    -> BlkStateConfiguration (ChgAccum conf) blkType m
    -> RawBlk blkType
    -> m ()
--        bscExpand bsc
--          ( [inj $ OpenBlockRawTx @blkType blkHeader]
--              ++ blkPayload
--              ++ [inj $ CloseBlockRawTx @blkType blkHeader]
--          )
expandAndApplyBlock checkBIV osParams bsc rawBlk = do
    chgAccum :: (ChgAccum conf) <- get
    headers <- bscExpandHeaders bsc chgAccum [rawBlk]
    payloads <- bscExpandPayloads bsc chgAccum [rawBlk]
    let blks = uncurry Block <$> zip (unOldestFirst headers) (fst <<$>> unOldestFirst payloads)
    case blks of
      blk:[] -> applyBlockImpl checkBIV osParams bsc (gett rawBlk) blk
      _      -> error "expandAndApplyBlock: expected exactly one block"

applyBlockImpl
    :: forall blkType conf m txtypes xs
    . ( Eq (BlockRef blkType)
      , HasBExceptions conf
        [ BlockApplicationException (BlockRef blkType)
        , StatePException
        , CSMappendException
        ]
      , HasGetter (Undo conf) (BlockUndo blkType)
      , HasLens (ChgAccum conf) (ChgAccum conf)
      , HasLens (Ctx conf) (ChgAccumCtx conf)
      , HUpCastableChSet '[TipComponent blkType] xs
      , HUpCastableChSet '[BlundComponent blkType] xs
      , QueryERo xs (TipComponent blkType)
      , MappendHChSet xs
      , Default (HChangeSet xs)
      , Tx blkType ~ SomeTx (BlkProcConstr txtypes xs)
      , m ~ ERwComp conf (DbAccessU conf xs) (ChgAccum conf)
      )
    => Bool
    -> OSParams blkType
    -> BlkStateConfiguration (ChgAccum conf) blkType m
    -> RawPayload blkType
    -> Block (BlockHeader blkType) (Payload blkType)
    -> m ()
applyBlockImpl checkBIV osParams BlkStateConfiguration{..} rawPayload blk@Block{..} = do
    tip <- bscGetTip
    when (checkBIV && any not [ unBIV (bcBlkVerify bscVerifyConfig) blk
                              , bcValidateFork bscVerifyConfig osParams (OldestFirst [blkHeader])]) $
        throwLocalError $ BlockIntegrityVerifierFailed @(BlockRef blkType)
    let prev = unPrevBlockRef $ bcPrevBlockRef bscVerifyConfig blkHeader
    if prev == tip then do
        chgAccum <- txToApply
        undo <- liftERoComp $ computeUndo @xs @conf chgAccum
        let rawBlund = Blund (Block blkHeader rawPayload) (gett undo)
        storeRawBlund bscVerifyConfig rawBlund
        setTip @blkType $ unCurrentBlockRef $ bcBlockRef bscVerifyConfig blkHeader
    else
        throwLocalError $ TipMismatched prev tip

  where
    txToApply :: m (ChgAccum conf)
    txToApply = cs >>= liftERoComp . (modifyAccumOne @xs @conf)
       where
         css = applySomeTx (hupcast . txBody) <$> unOldestFirst blkPayload
         cs = either throwLocalError pure (mconcatChangeSets css)


-- | Function `tryApplyFork` uses `verifyFork` from `Snowdrop.Block.Fork` in order to verify
-- block sequence and decide on whether proposed chain is better
-- than currently adopted "best" chain.
--
-- If `verifyFork` returns `ApplyFork`:
--
-- 1. rollback for a series of blocks is performed;
-- 2. tip is set to reference of block, which preceedes the first block in the fork;
-- 3. for each block in the fork: payload is applied, blund is stored and tip updated.
-- tryApplyFork
--     -- TODO `undo` is not Monoid, even for ChangeSet
--     :: forall chgAccum blkType e m
--     . ( HasGetter (RawBlk blkType) (BlockHeader blkType)
--       -- pva701: TODO ^ this constraint should be eliminated and
--       -- either expanding of headers should be made separately from blocks
--       -- or fork should be verified using scheme:
--       -- 1. rollback
--       -- 2. expand alt chain
--       -- 3. compare chains
--       -- 4. apply appropriate chain
--       , HasGetter (RawBlk blkType) (RawPayload blkType)
--       , Eq (BlockRef blkType)
--       , HasReviews e [ ForkVerificationException (BlockRef blkType)
--                         , BlockApplicationException (BlockRef blkType)]
--       , MonadError e m
--       )
--     => BlkStateConfiguration chgAccum blkType m
--     -> OSParams blkType
--     -> OldestFirst NonEmpty (RawBlk blkType)
--     -> m Bool
-- tryApplyFork bcs@(BlkStateConfiguration {..}) osParams (OldestFirst rawBlocks) = do
--     -- fork <- traverse toFork (unOldestFirst rawBlocks)
--     verifyFork bcs osParams (OldestFirst $ NE.map gett rawBlocks) >>= \case
--         RejectFork     -> pure False
--         ApplyFork{..} -> do
--             forM_ (unNewestFirst fvrToRollback) $ \blund -> do
--                 bscApplyUndo (buUndo blund)
--                 bscRemoveBlund $ unCurrentBlockRef $
--                     bcBlockRef bscConfig (blkHeader $ buBlock blund)
--             bscSetTip fvrLCA
--             mapM_ (applyBlock osParams bcs) $ NE.toList rawBlocks
--             pure True

applyFork :: forall blkType m conf txtypes xs
    . ( HasBExceptions conf
        [ StatePException
        , CSMappendException
        ]
      , HasGetter (RawBlk blkType) (RawPayload blkType)
      , HasGetter (Undo conf) (BlockUndo blkType)
      , HasLens (ChgAccum conf) (ChgAccum conf)
      , HasLens (Ctx conf) (ChgAccumCtx conf)
      , HUpCastableChSet '[TipComponent blkType] xs
      , HUpCastableChSet '[BlundComponent blkType] xs
      , QueryERo xs (TipComponent blkType)
      , Tx blkType ~ SomeTx (BlkProcConstr txtypes xs)
      , m ~ ERwComp conf (DbAccessU conf xs) (ChgAccum conf)
      )
    => BlkConfiguration blkType
    -> OldestFirst NonEmpty (RawBlk blkType)
    -> ForkApplyAction (ChgAccum conf) blkType
    -> m (OldestFirst [] (ChgAccum conf))
applyFork cfg fork ForkApplyAction{..} = do
    toApply <- txsToApply -- Apply tx1, ..., tx_k to state
    forkUndos <- liftERoComp $ traverse (computeUndo @xs @conf) chgAccums
    let forkBlockUndos = gett <$> forkUndos
    (liftERoComp $ modifyAccumUndo @xs @conf (NewestFirst forkUndos)) >>= modify . flip sett

    storeRawBlunds forkBlockUndos -- save blunds for fork
    setTip @blkType faaRollbackTo -- and update tip
    pure toApply
  where
    (blocks, chgAccums) = unzip $ unOldestFirst faaApplyBlocks

    txsToApply = liftERoComp $ modifyAccum @xs @conf (OldestFirst css)
       where
         payloads = join $ unOldestFirst $ traverse blkPayload blocks
         css = map (applySomeTx $ hupcast . txBody) payloads

    storeRawBlunds rBlunds = void $ traverse (storeRawBlund cfg) (rawBlunds rBlunds)
       where

         rawBlunds :: [BlockUndo blkType] -> [RawBlund blkType]
         rawBlunds blockUndo = mkRawBlund <$> rawBlundData blockUndo
            where
              headers = blkHeader <$> blocks

              mkRawBlund :: (BlockHeader blkType, RawPayload blkType, BlockUndo blkType) -> RawBlund blkType
              mkRawBlund (header, cs, undo) = Blund (Block header cs) undo

              fork' = toList $ unOldestFirst fork
              rawPayloads = gett <$> drop (length fork' - length headers) fork'
              rawBlundData = zip3 headers rawPayloads

storeRawBlund :: forall blkType m conf xs
    . ( HasBExceptions conf
        [ StatePException
        , CSMappendException
        ]
      , HasLens (Ctx conf) (ChgAccumCtx conf)
      , HUpCastableChSet '[BlundComponent blkType] xs
      , m ~ ERwComp conf (DbAccessU conf xs) (ChgAccum conf)
      )
      => BlkConfiguration blkType
      -> RawBlund blkType -> m ()
storeRawBlund cfg blund = do
   let blockRef = unCurrentBlockRef $ bcBlockRef cfg (blkHeader $ buBlock blund)
   let chg = hChangeSetFromMap @(BlundComponent blkType) $ M.singleton blockRef (New blund)
   applyBlkChg @xs chg

setTip :: forall blkType m conf xs
    . ( HasBExceptions conf
        [ StatePException
        , CSMappendException
        ]
      , HasLens (ChgAccum conf) (ChgAccum conf)
      , HUpCastableChSet '[TipComponent blkType] xs
      , QueryERo xs (TipComponent blkType)
      , HasLens (Ctx conf) (ChgAccumCtx conf)
      , m ~ ERwComp conf (DbAccessU conf xs) (ChgAccum conf)
      )
    => BlockRef blkType
    -> m ()
setTip newTip = do
    oldTipMb <- unTipValue <<$>> liftERoComp (queryOne @(TipComponent blkType) @xs @conf TipKey)
    let applyChg tipMod = applyBlkChg $ hChangeSetFromMap @(TipComponent blkType) $ M.singleton TipKey tipMod
    applyChg $ maybe New (const Upd) oldTipMb (TipValue newTip)

applyBlkChg :: forall xs t m conf .
  ( HasBExceptions conf
        [ StatePException
        , CSMappendException
        ]
  , HasLens (ChgAccum conf) (ChgAccum conf)
  , HasLens (Ctx conf) (ChgAccumCtx conf)
  , HUpCastableChSet '[t] xs
  , m ~ ERwComp conf (DbAccessU conf xs) (ChgAccum conf)
  ) => HChangeSet '[t] -> m ()
applyBlkChg chg =
    liftERoComp (modifyAccumOne @xs @conf $ hupcast @_ @'[t] @xs chg)
        >>= modify . flip sett

processFork
    :: forall blkType m conf xs
    . ( HasBExceptions conf
          [ ForkVerificationException (BlockRef blkType)
          , StatePException
          , CSMappendException
          ]
      , Eq (BlockRef blkType)
      , HasGetter (BlockHeader blkType) (BlockRef blkType)
      , HasGetter (ChgAccum conf) (HChangeSet xs)
      , HasGetter (HChangeSet xs) (ChgAccum conf)
      , MappendHChSet xs
      , Default (HChangeSet xs)
      , m ~ ERoComp conf xs
      ) => BlkStateConfiguration (ChgAccum conf) blkType m
    -> OSParams blkType
    -> OldestFirst NonEmpty (RawBlk blkType)
    -> ChgAccum conf
    -> m (ForkApplyAction (ChgAccum conf) blkType)
processFork bcs@(BlkStateConfiguration {..}) osParams fork chgAccum = do

  let forkList = NE.toList $ unOldestFirst fork

  headers <- bscExpandHeaders chgAccum forkList

  case headers of
    OldestFirst []     -> throwLocalError @(ForkVerificationException (BlockRef blkType)) InvalidForkOrigin
    OldestFirst (x:xs) -> verifyFork @_ @(ChgAccum conf) bcs osParams (OldestFirst (x :| xs)) >>= \case
      ApplyFork -> pure ()
      RejectFork -> throwLocalError @(ForkVerificationException (BlockRef blkType)) InvalidForkOrigin

  tip <- bscGetTip >>= maybe (throwLocalError @StatePException BlockRefNotFound) pure
  tipHeader <- bscGetHeader tip >>= maybe (throwLocalError @StatePException BlockRefNotFound) pure

  let depth = bcMaxForkDepth bscVerifyConfig

  (rollbackTo, acc0) <- findLCA depth (gett <$> headers) tipHeader chgAccum

  -- | 4. Expand raw block bodies of fork, get sequence of transactions tx1, tx2, ..., tx_k.
  txsWithChgAccum <- bscExpandPayloads acc0 forkList

  -- | 5. Compute series of accumulators acc1, .., acc_{k-1}: acc_i := applyChangeSet acc_{i-1} (changeSet tx_i).
  -- Step 5 done as part of runSeqExpandersSequentially

  -- | 6. Perform validation of tx_i using acc_{i-1} for all i: 1..k.
  void $ getCompose <$> traverse (uncurry bscValidateTx . swap) (Compose txsWithChgAccum)

  let headerAndPayloads = zip (unOldestFirst headers) (unOldestFirst txsWithChgAccum)

  let mkBlockAndChgAccum :: (BlockHeader blkType, OldestFirst [] (Tx blkType, ChgAccum conf)) -> m (Block (BlockHeader blkType) (Payload blkType), ChgAccum conf)
      mkBlockAndChgAccum (h, OldestFirst t) = let
        (txs, chgAccums) = unzip t
        blockTxChangeSets = gett @_ @(HChangeSet xs) <$> chgAccums
        eitherBlockChangeSet = gett <$> mconcatChangeSets blockTxChangeSets
        in do
        blockChangeSet <- either throwLocalError pure eitherBlockChangeSet
        pure (Block h (OldestFirst txs), blockChangeSet)

  applyBlocks <- OldestFirst <$> traverse mkBlockAndChgAccum headerAndPayloads

  pure $ ForkApplyAction rollbackTo applyBlocks

    where
      findLCA
          :: Int
          -> OldestFirst [] (BlockRef blkType)
          -> BlockHeader blkType
          -> ChgAccum conf
          -> m (BlockRef blkType, ChgAccum conf)
      findLCA depth hashes currentHeader acc
        | depth < 0 = throwLocalError @(ForkVerificationException (BlockRef blkType)) TooDeepFork
        | otherwise = case maybePrev of
            Nothing -> throwLocalError @(ForkVerificationException (BlockRef blkType)) OriginOfBlockchainReached
            Just prev ->
              if elem prev (unOldestFirst hashes)
                then pure $ (cur, acc)
                else do
                  prevHeader <- bscGetHeader prev
                  prevChgAccum <- bscInmemRollback prev
                  case prevHeader of
                    Just p -> findLCA (depth - 1) hashes p prevChgAccum
                    _      -> throwLocalError @(ForkVerificationException (BlockRef blkType)) (BlockDoesntExistInChain prev)
       where
         CurrentBlockRef cur = bcBlockRef bscVerifyConfig currentHeader
         PrevBlockRef maybePrev = bcPrevBlockRef bscVerifyConfig currentHeader
