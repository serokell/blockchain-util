{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types    #-}

module Snowdrop.Util.Helpers
       (
         VerRes (..)
       , VerifySigned (..)
       , eitherToVerRes
       , verResToEither
       , runExceptTV
       , propagateSecondF
       , PublicKey
       , Signature
       , Signed (..)
       , HFunctor (..)
       , Sign (..)
       , SecretKey
       , SignKeyPair (..)
       , SecureHash (..)
       , SignedNoMsg (..)
       ) where

import           Universum hiding (head, init, last)

import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))

-- | Data family to specify a public key type for a given signature scheme type
data family PublicKey sigScheme :: *
-- | Data family to specify a signature type for a given signature scheme type
data family Signature sigScheme a :: *
-- | Data family to specify a secret key type for a given signature scheme type
data family SecretKey sigScheme :: *

class SignKeyPair sigScheme where
    toPublicKey :: SecretKey sigScheme -> PublicKey sigScheme

-- | Type class, providing capability to verify signatures
-- (made within some public-key signature scheme, represented by type @sigScheme@)
class SignKeyPair sigScheme => Sign sigScheme a where
    sign :: SecretKey sigScheme -> a -> Signature sigScheme a

    mkSignedNoMsg :: SecretKey sigScheme -> a -> SignedNoMsg sigScheme a
    mkSignedNoMsg sk msg = SignedNoMsg (toPublicKey sk) (sign sk msg)

    mkSigned :: SecretKey sigScheme -> a -> Signed sigScheme a
    mkSigned sk msg = Signed msg (toPublicKey sk) (sign sk msg)

-- | Type class, providing capability to verify signatures
-- (made within some public-key signature scheme, represented by type @sigScheme@)
class VerifySigned sigScheme a where
    -- | Verify signature of the supplied data which is assumed to be made by a given public key
    verifySigned :: Signed sigScheme a -> Bool

    -- | Verify signature of the supplied data which is assumed to be made by a given public key
    verifySignature :: PublicKey sigScheme -> a -> Signature sigScheme a -> Bool
    verifySignature pk msg = verifySigned . Signed msg pk

    -- | Verify signature of the supplied data which is assumed to be made by a given public key
    verifySignedNoMsg :: SignedNoMsg sigScheme a -> a -> Bool
    verifySignedNoMsg (SignedNoMsg pk sig) msg = verifySignature pk msg sig

-- | Helper data type to hold data along with its signature
-- and a public key corresponding to the signature.
data SignedNoMsg sigScheme msg = SignedNoMsg
    { snPublicKey :: PublicKey sigScheme
    , snSignature :: Signature sigScheme msg
    } deriving (Generic)

deriving instance (Show (PublicKey sigScheme),
                   Show (Signature sigScheme msg)) => Show (SignedNoMsg sigScheme msg)
deriving instance (Eq (PublicKey sigScheme),
                   Eq (Signature sigScheme msg)) => Eq (SignedNoMsg sigScheme msg)
deriving instance (Ord (PublicKey sigScheme),
                   Ord (Signature sigScheme msg)) => Ord (SignedNoMsg sigScheme msg)

instance (Hashable (PublicKey sigScheme), Hashable (Signature sigScheme msg)) => Hashable (SignedNoMsg sigScheme msg)

instance (Buildable (PublicKey sigScheme),
          Buildable (Signature sigScheme msg)) => Buildable (SignedNoMsg sigScheme msg) where
    build (SignedNoMsg pk sig) =
        bprint ("signature (pubkey="%(build)%", signature="%(build)%")") pk sig

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
          Buildable (Signature sigScheme msg),
          Buildable msg) => Buildable (Signed sigScheme msg) where
    build (Signed msg pk sig) =
        bprint ("signature (pubkey="%(build)%", signature="%(build)%", message="%(build)%", )") pk sig msg

-- | Data type, similar to `Either` which provides instances of 'Semigroup' and 'Monoid',
-- well suited for error handling.
data VerRes e a = VErr e | VRes a
  deriving (Show, Eq, Functor)

instance Applicative (VerRes e) where
  pure          = VRes
  VErr  e <*> _ = VErr e
  VRes f <*> r = fmap f r

instance Semigroup a => Semigroup (VerRes e a) where
    VRes a <> VRes b = VRes $ a <> b
    x@(VErr _) <> _ = x
    _ <> x@(VErr _) = x

instance (Monoid a, Semigroup a) => Monoid (VerRes e a) where
    mempty = VRes mempty
    mappend = (<>)

-- | Convert value of type 'Either' to type 'VerRes'
eitherToVerRes :: Either e a -> VerRes e a
eitherToVerRes (Right x) = VRes x
eitherToVerRes (Left e)  = VErr e

-- | Convert value of type 'VerRes' to type 'Either'
verResToEither :: VerRes e a -> Either e a
verResToEither (VRes x) = Right x
verResToEither (VErr e) = Left e

-- | Helper to run 'ExceptT' in order to obtain 'VerRes'
runExceptTV :: Monad m => ExceptT e m a -> m (VerRes e a)
runExceptTV = fmap eitherToVerRes . runExceptT

-- | Simple helper which is equivallent to expression @\(fa, b) -> (,b) <$> fa@
propagateSecondF :: Functor f => (f a, b) -> f (a, b)
propagateSecondF (fa, b) = (,b) <$> fa

-- | Higher-order version of Functor class.
class HFunctor t where
    fmapH :: Functor a => (forall x . a x -> b x) -> t a -> t b

class SecureHash hash msg where
    secureHash :: msg -> hash
