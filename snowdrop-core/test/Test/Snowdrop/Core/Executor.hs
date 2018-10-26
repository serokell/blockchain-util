{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Snowdrop.Core.Executor
       ( TestExecutorT
       , TestCtx
       , runERoComp
       , countERoComp
       , Counter (..)
       ) where

import           Universum

import           Control.Lens (makeLenses)
import           Control.Monad.Except (MonadError)
import           Data.Default (Default (def))
import qualified Data.Map.Strict as M
import           Data.Vinyl.Core (Rec (..))
import           Data.Vinyl.TypeLevel (AllConstrained)

import           Loot.Log (NameSelector (..))
import           Snowdrop.Core (BException, CSMappendException (..), ChgAccum, ChgAccumCtx (..),
                                Ctx, DbAccess (..), ERoComp, Effectful (..), FoldF (..), HChangeSet,
                                HChangeSetEl, ValueOp (..), getCAOrDefault, hChangeSetElToList,
                                unBaseM)
import           Snowdrop.Util

-- | SumChangeSet holds some change set which is sum of several ChangeSet
-- Copy-pasted from snowdrop-execution in order to avoid bad dependency between packages
newtype SumChangeSet xs = SumChangeSet (HChangeSet xs)

deriving instance Show (HChangeSet xs) => Show (SumChangeSet xs)

instance Default (HChangeSet xs) => Default (SumChangeSet xs) where
    def = SumChangeSet def

simpleStateAccessor
    :: HIntersectable xs xs
    => HMap xs
    -> DbAccess xs res
    -> res
simpleStateAccessor st (DbQuery q cont) = cont (st `hintersect` q)
simpleStateAccessor st (DbIterator getComp (FoldF (e, foldf, applier))) = applier $
    foldl
      foldf
      e
      (M.toList $ unHMapEl $ getComp st)

data TestCtx conf = TestCtx
    { tctxChgAccum :: ChgAccumCtx conf
    }

instance HasLens (TestCtx conf) (ChgAccumCtx conf) where
    sett ctx val = ctx { tctxChgAccum = val }

instance HasGetter (TestCtx conf) (ChgAccumCtx conf) where
    gett = tctxChgAccum

data Counter = Counter
    { _cntQuery :: Int
    , _cntIter  :: Int
    }

makeLenses ''Counter

instance Default Counter where
    def = Counter 0 0

newtype TestExecutorT e conf m a = TestExecutorT
    { runTestExecutorT :: ReaderT (TestCtx conf) (StateT Counter (ExceptT e m)) a }
    deriving (Functor, Applicative, Monad, MonadError e, MonadReader (TestCtx conf))

-- Dummy logging
instance Monad m => MonadLogging (TestExecutorT e conf m) where
    log _ _ _ = pure ()
    logName = pure (GivenName "")

instance Monad m => ModifyLogName (TestExecutorT e conf m) where
    modifyLogNameSel _ = id

applyDiff
    :: forall e m xs .
    ( HasReview e CSMappendException
    , MonadError e m
    , AllConstrained ExnHKey xs
    )
    => HChangeSet xs
    -> HMap xs
    -> m (HMap xs)
applyDiff = applyDiffDo
  where
    applyDiffDo :: AllConstrained ExnHKey rs => HChangeSet rs  -> HMap rs -> m (HMap rs)
    applyDiffDo RNil RNil                = pure RNil
    applyDiffDo (cs :& xs) (initM :& ys) = (:&) <$> applyDiffF cs initM <*> applyDiffDo xs ys

    applyDiffF :: ExnHKey t => HChangeSetEl t -> HMapEl t -> m (HMapEl t)
    applyDiffF cs initM = HMapEl <$> foldM applyDiffOne (unHMapEl initM) (hChangeSetElToList cs)
      where
        maybeLookup k m act1 act2 = maybe act1 act2 $ M.lookup k m
        applyDiffOne m (k, New v) =
            maybeLookup k m (pure $ M.insert k v m) (\_ -> throwLocalError $ CSMappendException k)
        applyDiffOne m (k, Upd v) =
            maybeLookup k m (throwLocalError $ CSMappendException k) (\_ -> pure $ M.insert k v m)
        applyDiffOne m (k, Rem)   =
            maybeLookup k m (throwLocalError $ CSMappendException k) (\_ -> pure $ M.delete k m)
        applyDiffOne m (k, NotExisted) =
            maybeLookup k m (pure m) (\_ -> throwLocalError $ CSMappendException k)

instance ( MonadReader (HMap xs) m
         , Default (HChangeSet xs)
         , HasReview e CSMappendException
         , HIntersectable xs xs
         , AllConstrained ExnHKey xs
         , ChgAccum conf ~ SumChangeSet xs
         ) =>
    Effectful (DbAccess xs) (TestExecutorT e conf m) where
        effect dbAccess = do
            SumChangeSet acc <- getCAOrDefault . tctxChgAccum <$> ask
            storage <- TestExecutorT $ lift $ lift ask
            storage' <- acc `applyDiff` storage
            case dbAccess of
                DbQuery _ _    -> TestExecutorT $ lift $ modify $ cntQuery %~ (+1)
                DbIterator _ _ -> TestExecutorT $ lift $ modify $ cntIter %~ (+1)
            pure $ simpleStateAccessor storage' dbAccess

data TestConf e (xs :: [*])

type instance BException (TestConf e xs) = e
type instance Ctx (TestConf e xs) = TestCtx (TestConf e xs)
type instance ChgAccum (TestConf e xs) = SumChangeSet xs


countERoComp
    :: forall e xs m a .
       ( MonadReader (HMap xs) m
       , HasReview e CSMappendException
       , Default (HChangeSet xs)
       , HIntersectable xs xs
       , AllConstrained ExnHKey xs
       )
    => ERoComp (TestConf e xs) xs a
    -> m (Either e Counter)
countERoComp comp =
    runExceptT $ flip execStateT def $
        runReaderT (runTestExecutorT (unBaseM comp)) $ TestCtx CANotInitialized

runERoComp
    :: forall e xs m a .
       ( MonadReader (HMap xs) m
       , HasReview e CSMappendException
       , Default (HChangeSet xs)
       , HIntersectable xs xs
       , AllConstrained ExnHKey xs
       )
    => ERoComp (TestConf e xs) xs a
    -> m (Either e a)
runERoComp comp =
    runExceptT $ flip evalStateT def $
        runReaderT (runTestExecutorT (unBaseM comp)) $ TestCtx CANotInitialized
