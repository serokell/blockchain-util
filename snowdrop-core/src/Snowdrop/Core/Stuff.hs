{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Core.Stuff
       ( StateModificationException(..)

       , UnitedTxType
       , Prf (..)
       ) where

import           Universum

import           Control.Lens (to)
import qualified Data.Text.Buildable
import           Data.Vinyl (Rec (..), type (∈), rget)
import           Formatting (bprint, build, stext, (%))

import           Snowdrop.Core.Transaction (TxComponents, TxProof)
import           Snowdrop.Util (HasGetter (..), UnionTypes)

------------------------
-- Compute undo
------------------------

data StateModificationException
    = forall id . (Buildable id, Show id) => UnexpectedKeyExists id
    | forall id . (Buildable id, Show id) => UnexpectedKeyAbsent id

deriving instance Show StateModificationException

instance Buildable StateModificationException where
    build = \case
        UnexpectedKeyExists i -> problemWithKey "exist" i
        UnexpectedKeyAbsent i -> problemWithKey "be absent" i
      where
        problemWithKey desc key =
            bprint ("Key "%build%" was not expected to "%stext%
                " during performed modification") key desc

-- type ComputeUndoC e components xs =
--      ( HasException e StateModificationException
--      , RecAll' xs (QueryERo components)
--      , RecAll' xs GKeyC
--      )

-- | Compute undo and verify change is valid for being applied to state
-- computeUndo
--   :: forall e components xs ctx . ComputeUndoC e components xs
--   => GChangeSet xs
--   -> ERoComp e components ctx (Undo xs)
-- computeUndo GNil        = pure GNil
-- computeUndo gs'@(_ :> _) = computeUndo' gs'
--   where
--     -- Nonempty list case
--     computeUndo'
--       :: forall t xs' . ComputeUndoC e components (t ': xs')
--       => GChangeSet (t ': xs')
--       -> ERoComp e components ctx (Undo (t ': xs'))
--     computeUndo' (cs :> gs) = liftA2 (:>) (checkNew <> checkUpdate <> checkRemove) (computeUndo gs)
--       where
--         checkNew :: QueryERo components t => ERoComp e components ctx (MapKV ValueOp t)
--         checkNew = query @t (M.keysSet $ csNew cs) >>= \vals ->
--             case M.toList vals of
--                 (i,_):_ -> throwLocalError $ UnexpectedKeyExists i
--                 _       -> pure $ Rem <$ csNew cs

--         checkUpdate :: QueryERo components t => ERoComp e components ctx (MapKV ValueOp t)
--         checkUpdate = query @t (M.keysSet $ csUpdate cs) >>= \vals ->
--             case M.toList (csUpdate cs M.\\ vals) of
--                 (i,_):_ -> throwLocalError $ UnexpectedKeyAbsent i
--                 _       -> pure $ Upd <$> vals

--         checkRemove :: QueryERo components t => ERoComp e components ctx (MapKV ValueOp t)
--         checkRemove = query @t (csRemove cs) >>= \vals ->
--             case S.toList (csRemove cs S.\\ M.keysSet vals) of
--                 i:_ -> throwLocalError $ UnexpectedKeyAbsent i
--                 _   -> pure $ New <$> vals


------------------------
-- UnitedTxType
------------------------

data UnitedTxType (types :: [*])

type instance TxComponents (UnitedTxType '[x]) = TxComponents x
type instance TxComponents (UnitedTxType (x ': y ': xs)) = UnionTypes (TxComponents x) (TxComponents (UnitedTxType (y ': xs)))
type instance TxProof (UnitedTxType '[x]) = TxProof x

newtype Prf x = Prf {unPrf :: TxProof x} -- Gospodi kak je ya ustal ot etoi parashi
type instance TxProof (UnitedTxType (x ': y ': xs)) = Rec Prf (x ': y ': xs)

instance (prftype ~ TxProof x) => HasGetter (Prf x) prftype where
    gett = unPrf
instance (prftype ~ TxProof x, x ∈ xs) => HasGetter (Rec Prf xs) prftype where
    gett = gett . rget (Proxy @x)
    getterOf = to (gett . rget (Proxy @x)) -- to avoid some troubles with @x@, dunno
