module Snowdrop.Util.Helpers
       (
         VerRes (..)
       , VerifySign (..)
       , IdStorage
       , getId
       , eitherToVerRes
       , verResToEither
       , runExceptTV
       , propagateSecondF
       , PublicKey
       , Signature
       , Signed (..)
       ) where

import           Universum hiding (head, init, last)

import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))

import           Snowdrop.Util.Prism.Class

-- | Data family to specify a public key type for a given signature scheme type
data family PublicKey sigScheme :: *
-- | Data family to specify a signature type for a given signature scheme type
data family Signature sigScheme a :: *

-- | Type class, providing capability to verify signatures
-- (made within some public-key signature scheme, represented by type @sigScheme@)
class VerifySign sigScheme a where
    -- | Verify signature of the supplied data which is assumed to be made by a given public key
    verifySignature :: PublicKey sigScheme -> a -> Signature sigScheme a -> Bool

-- | Helper data type to hold data along with its signature
-- and a public key corresponding to the signature.
data Signed sigScheme msg = Signed
    { message   :: msg
    , publicKey :: PublicKey sigScheme
    , signature :: Signature sigScheme msg
    } deriving (Generic)

deriving instance (Show (PublicKey sigScheme),
                   Show (Signature sigScheme msg),
                   Show msg) => Show (Signed sigScheme msg)
deriving instance (Eq (PublicKey sigScheme),
                   Eq (Signature sigScheme msg),
                   Eq msg) => Eq (Signed sigScheme msg)
deriving instance (Ord (PublicKey sigScheme),
                   Ord (Signature sigScheme msg),
                   Ord msg) => Ord (Signed sigScheme msg)

instance (Hashable (PublicKey sigScheme), Hashable (Signature sigScheme msg),
          Hashable msg) => Hashable (Signed sigScheme msg)

instance (Buildable (PublicKey sigScheme),
          Buildable (Signature sigScheme msg)) => Buildable (Signed sigScheme msg) where
    build (Signed _ pk sig) =
        -- We omit 'message' for purpose. Currently 'Signed' is used
        -- in witnesses, and witness is always coupled with related transaction.
        bprint ("signature (pubkey="%(build)%", signature="%(build)%")") pk sig

-- | Type class which allows to get an instance of some enum data type @s@,
-- specified for a given value @a@.
--
-- Usage pattern is as follows:
--
-- @
-- data MyId = MyId1 | MyId2
-- data HerId = HerId
--
-- data AllIds = AMyId MyId | AHerId HerId
--
-- instance Enum AllIds where
--     toEnum x
--         | x < fromEnum (maxBound @MyId) = AMyId $ toEnum x
--         | rest < fromEnum (maxBound @HerId) = AHerId $ toEnum rest
--         | otherwise  = error "invalid int"
--       where
--         rest = x - fromEnum (maxBound @MyId)
--     fromEnum (AMyId s)      = fromEnum s
--     fromEnum (AHerId HerId) = maxBound @MyId
--
-- instance IdStorage AllIds MyId
-- instance IdStorage AllIds HerId
--
-- deriveView withInjProj ''AllIds
--
-- -- Integer corresponding to MyId2:
-- myId2 :: Int
-- myId2 = getId (Proxy @AllIds) MyId2
-- @
--
-- The type class allows to get an unique integer for any concrete id,
-- which can be further used for instance for indexing components of database.
class (HasPrism s a, Enum s) => IdStorage s a

-- | Get integer index for a given id
getId :: forall ids i . IdStorage ids i => Proxy ids -> i -> Int
getId _ i = fromEnum (inj i :: ids)

-- | Data type, similar to `Either` which provides instances of $Semigroup and $Monoid,
-- well suited for error handling.
data VerRes e a = VErr e | VRes a
    deriving (Show, Eq)

instance Semigroup a => Semigroup (VerRes e a) where
    VRes a <> VRes b = VRes $ a <> b
    x@(VErr _) <> _ = x
    _ <> x@(VErr _) = x

instance (Monoid a, Semigroup a) => Monoid (VerRes e a) where
    mempty = VRes mempty
    mappend = (<>)

-- | Convert value of type $Either to type $VerRes
eitherToVerRes :: Either e a -> VerRes e a
eitherToVerRes (Right x) = VRes x
eitherToVerRes (Left e)  = VErr e

-- | Convert value of type $VerRes to type $Either
verResToEither :: VerRes e a -> Either e a
verResToEither (VRes x) = Right x
verResToEither (VErr e) = Left e

-- | Helper to run $ExceptT in order to obtain $VerRes
runExceptTV :: Monad m => ExceptT e m a -> m (VerRes e a)
runExceptTV = fmap eitherToVerRes . runExceptT

-- | Simple helper which is equivallent to expression @\(fa, b) -> (,b) <$> fa@
propagateSecondF :: Functor f => (f a, b) -> f (a, b)
propagateSecondF (fa, b) = (,b) <$> fa
