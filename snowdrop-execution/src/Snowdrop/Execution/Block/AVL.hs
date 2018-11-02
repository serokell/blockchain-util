{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Execution.Block.AVL
       ( createAVLActionsPair
       ) where

import           Universum

import           Data.Tree.AVL (Serialisable (..))

import qualified Data.Map.Strict as M
import           Data.Vinyl (Rec (..))
import           Data.Vinyl.TypeLevel (type (++))
import           Fmt (format)

import           Snowdrop.Block (BlockRef)
import           Snowdrop.Core (ChgAccum, Undo)
import           Snowdrop.Execution.Block.Storage (BlundComponent, TipComponent,
                                                   simpleBlockDbActions)
import           Snowdrop.Execution.DbActions (AMSRequested, AllWholeTree, AvlClientConf,
                                               AvlServerConf, ClientMode, CompositeChgAccum,
                                               CompositeUndo, DbApplyProof, DbComponents,
                                               DbModifyActions, IsAvlEntry, RememberForProof,
                                               SimpleConf, amsRootHashes, amsState,
                                               avlClientDbActions, avlServerDbActions,
                                               constructCompositeDma, initAVLPureStorage,
                                               unAVLPureStorage)
import           Snowdrop.Util (ExecM, HDownCastable, HMap, NotIntersect, RecAll', fmapH, logInfo)


type ServerDma conf = RememberForProof -> DbModifyActions conf ExecM
type ClientDma proof conf = ClientMode proof -> DbModifyActions conf ExecM

createAVLActionsPair
  :: forall avlHash xs blkTypeS blkTypeC confS confC blkConfS blkConfC stateConfS stateConfC.
       ( Ord (BlockRef blkTypeC)
       , Show (BlockRef blkTypeC)
       , Buildable (BlockRef blkTypeC)
       , Typeable (BlockRef blkTypeC)
       , Ord (BlockRef blkTypeS)
       , Show (BlockRef blkTypeS)
       , Buildable (BlockRef blkTypeS)
       , Typeable (BlockRef blkTypeS)
       , DbComponents confS ~ (DbComponents blkConfS ++ xs)
       , DbComponents confC ~ (DbComponents blkConfC ++ xs)
       , HDownCastable (DbComponents confS) xs
       , HDownCastable (DbComponents confC) xs
       , NotIntersect (DbComponents blkConfS) xs
       , NotIntersect (DbComponents blkConfC) xs
       , RecAll' xs (IsAvlEntry avlHash)
       , Ord avlHash
       , Show avlHash
       , Typeable avlHash
       , Serialisable avlHash
       , AllWholeTree xs
       , Monoid (Rec AMSRequested xs)
       , blkConfC ~ SimpleConf '[TipComponent blkTypeC, BlundComponent blkTypeC]
       , blkConfS ~ SimpleConf '[TipComponent blkTypeS, BlundComponent blkTypeS]
       , DbApplyProof confC ~ ((), DbApplyProof stateConfC)
       , DbApplyProof confS ~ ((), DbApplyProof stateConfS)
       , ChgAccum confC ~ CompositeChgAccum blkConfC stateConfC
       , ChgAccum confS ~ CompositeChgAccum blkConfS stateConfS
       , Undo confC ~ CompositeUndo blkConfC stateConfC
       , Undo confS ~ CompositeUndo blkConfS stateConfS
       , stateConfC ~ AvlClientConf avlHash xs
       , stateConfS ~ AvlServerConf avlHash xs
       )
    => HMap xs
    -> ExecM (ServerDma confS, ClientDma (DbApplyProof stateConfS) confC)
createAVLActionsPair initStorage = do
    avlInitState <- initAVLPureStorage initStorage
    let initState = unAVLPureStorage $ amsState avlInitState
    logInfo $ format "Initialized pure AVL+ storage ({} entries)" (M.size initState)
    (serverStDba, serverLookupHash) <- avlServerDbActions avlInitState
    let retrieveF :: avlHash -> ExecM (Maybe ByteString)
        retrieveF h = serverLookupHash h >>= \resp -> do
           -- TODO uncomment this stuff in future
           -- whenJust resp $ \resp' -> do
           --     (respV :: AVL.MapLayer Hash Ids Values Hash) <- deserialiseM resp'
           --     let respProof = AVL.Proof . Free $ pure <$> respV
           --     when (not $ AVL.checkProof h respProof) $ throwM BrokenProofError
           pure resp
    clientStDba <- avlClientDbActions retrieveF (amsRootHashes avlInitState)
    liftA2 (,) (constructDma serverStDba) (constructDma clientStDba)
  where
    constructDma stateDma = do
        blkDma <- fmapH atomically <$> atomically simpleBlockDbActions
        pure (constructCompositeDma blkDma . stateDma)
