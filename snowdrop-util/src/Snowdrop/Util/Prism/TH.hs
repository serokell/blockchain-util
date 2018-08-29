module Snowdrop.Util.Prism.TH
       (
         withInj
       , withInjProj
       , deriveView
       , deriveIdView
       ) where

import           Universum hiding (head, init, last)

import qualified Language.Haskell.TH.Lib    as TH
import qualified Language.Haskell.TH.Syntax as TH

import           Snowdrop.Util.Prism.Class

-- | Defines which of 'HasReview' and 'HasPrism' instances should be declared.
data DeriveViewParams
    = DeriveInj
    | DeriveInjProj
    -- There seem to be no use case for bare DeriveProj.

withInj, withInjProj :: DeriveViewParams
withInj = DeriveInj
withInjProj = DeriveInjProj

-- | Helper to fetch only those entries which are requested via 'DeriveViewParams'.
selectFromInjProj :: Semigroup m => DeriveViewParams -> m -> m -> m
selectFromInjProj DeriveInj     injPart _        = injPart
selectFromInjProj DeriveInjProj injPart projPart = injPart <> projPart

-- | For @data A = B Int | C Double@, splice @deriveView param ''A@ defines
-- @instance HasX A Int@ and @instance HasX A Double@, where @HasX@ is
-- @HasReview@, @HasPrism@ or both.
deriveView :: DeriveViewParams -> TH.Name -> TH.Q [TH.Dec]
deriveView params dty = do
    tyInfo <- TH.reify dty
    ctors <- case tyInfo of
        TH.TyConI (TH.DataD _ _ _ _ ctors _)   -> pure ctors
        TH.TyConI (TH.NewtypeD _ _ _ _ ctor _) -> pure [ctor]
        _                                      -> fail "Can derive only for plain datatypes"
    concatMapM (deriveForCtor <=< getCtorParams) ctors
  where
    getCtorParams :: TH.Con -> TH.Q (Maybe (TH.Name, TH.Type))
    getCtorParams = \case
        TH.NormalC name [(_, ty)] -> pure $ Just (name, ty)
        TH.NormalC _ [] -> pure Nothing -- Ignoring empty constructors
        TH.NormalC _ _ -> fail "Expected exactly one entry in each constructor"
        TH.RecC name [(_, _, ty)] -> pure $ Just (name, ty)
        TH.RecC _ [] -> pure Nothing -- Ignoring empty constructors
        TH.RecC _ _ -> fail "Expected exactly one entry in each constructor"
        TH.InfixC _ _ _ -> fail "Can't derive when constructor is operator"
        TH.ForallC _ _ _ -> fail "Can not yet derive for existential quantification"
        TH.GadtC _ _ _ -> fail "GADT is unsupported yet"
        TH.RecGadtC _ _ _ -> fail "GADT is unsupported yet"
    deriveForCtor (Just p) =
        selectFromInjProj params <$> deriveInjForCtor p <*> deriveProjForCtor p
    deriveForCtor _ = pure []
    deriveInjForCtor (ctorName, innerTy) =
        [d| instance HasReview $(TH.conT dty) $(pure innerTy) where
                inj = $(TH.conE ctorName)
            |]
    deriveProjForCtor (ctorName, innerTy) = do
        let var = TH.mkName "x"
        [d| instance HasPrism $(TH.conT dty) $(pure innerTy) where
                proj $(pure $ TH.ConP ctorName [TH.VarP var]) = Just $(TH.varE var)
                proj _                                        = Nothing
            |]

-- | Defines @instance HasX A A@, where @HasX@ is @HasReview@, @HasPrism@ or
-- both.
deriveIdView :: DeriveViewParams -> TH.Name -> TH.Q [TH.Dec]
deriveIdView params ty = selectFromInjProj params <$> deriveInj <*> deriveProj
  where
    deriveInj =
        [d| instance HasReview $(TH.conT ty) $(TH.conT ty) where
                reviewOf = identity
            |]
    deriveProj =
        [d| instance HasPrism $(TH.conT ty) $(TH.conT ty) where
                proj = Just
            |]
