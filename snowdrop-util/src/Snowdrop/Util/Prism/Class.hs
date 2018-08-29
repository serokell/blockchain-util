module Snowdrop.Util.Prism.Class
       (
         HasReview (..)
       , HasPrism (..)
       , fPair
       ) where

import           Universum hiding (head, init, last)

import           Control.Lens (Prism', Review, prism', re, unto)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- TODO probably using lens package is overkill and we have to use manually defined
-- Prism and Getter in future

class HasReview s a where
    {-# MINIMAL reviewOf | inj #-}
    reviewOf :: Review s a
    reviewOf = unto inj

    inj :: HasReview s a => a -> s
    inj = (^. re reviewOf)


class HasReview s a => HasPrism s a where
    {-# MINIMAL prismOf | proj #-}
    prismOf :: Prism' s a
    prismOf = prism' inj proj

    proj :: HasPrism s a => s -> Maybe a
    proj = (^? prismOf)

------------------------------------------------------
-- Instances HasPrism
------------------------------------------------------

instance HasReview String String where
    reviewOf = identity

instance HasPrism String String where
    proj = Just

instance HasReview Int Int where
    reviewOf = identity

instance HasPrism Int Int where
    proj = Just

instance (HasReview a a1, HasReview b b1) => HasReview (a, b) (a1, b1) where
    inj = bimap inj inj

instance (HasPrism a a1, HasPrism b b1) => HasPrism (a, b) (a1, b1) where
    proj = fPair . bimap proj proj

instance HasReview (Set a) a where
    inj = S.singleton

instance HasPrism (Set a) a where
    proj s = if S.null s then Nothing
             else Just $ S.elemAt 0 s

instance HasReview (M.Map a b) (a, b) where
    inj = uncurry M.singleton

instance HasPrism (M.Map a b) (a, b) where
    proj m = if M.null m then Nothing
             else Just $ M.findMin m

instance (Ord id, HasReview id id1) => HasReview (Set id) (Set id1) where
    inj sid1 = S.fromList $ inj <$> S.toList sid1

instance (Ord id, Ord id1, HasPrism id id1) => HasPrism (Set id) (Set id1) where
    proj vals
        | isJust (find isNothing valsM)
            = Nothing -- TODO after we have more sensitive errors,
                      -- have one error for "unexpected entry {..} in set"
        | otherwise
            = Just $ S.fromList $ catMaybes valsM
      where
        valsM = proj <$> S.toList vals

instance (Ord id, HasReview id id1, HasReview value value1)
        => HasReview (M.Map id value) (M.Map id1 value1) where
    inj mid1 = M.fromList $ bimap inj inj <$> M.toList mid1

instance (Ord id, Ord id1, HasPrism id id1, HasPrism value value1)
        => HasPrism (M.Map id value) (M.Map id1 value1) where
    proj vals
        | isJust (find isNothing valsM)
            = Nothing -- TODO after we have more sensitive errors,
                      -- have one error for "id returned unexpected value"
                      -- and one for "unexpected id"
        | otherwise
            = Just $ M.fromList $ catMaybes valsM
      where
        valsM = fPair . bimap proj proj <$> M.toList vals

fPair :: Applicative f => (f a, f b) -> f (a, b)
fPair (fa, fb) = (,) <$> fa <*> fb
