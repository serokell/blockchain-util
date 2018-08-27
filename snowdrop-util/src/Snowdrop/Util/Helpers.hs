module Snowdrop.Util.Helpers
       (
         VerRes (..)
       , VerifySign (..)
       , IdStorage
       , getId
       , eitherToVerRes
       , verResToEither
       , runExceptTV
       , WithSignature (..)
       , propagateSecondF
       , PublicKey
       , Signature
       , Signed (..)
       ) where

import           Universum hiding (head, init, last)

import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))

import           Snowdrop.Util.Prism.Class

data family PublicKey sigScheme :: *
data family Signature sigScheme a :: *

class VerifySign sigScheme a where
    verifySignature :: PublicKey sigScheme -> a -> Signature sigScheme a -> Bool

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

class (HasPrism s a, Enum s) => IdStorage s a

getId :: forall ids i . IdStorage ids i => Proxy ids -> i -> Int
getId _ i = fromEnum (inj i :: ids)

-- to avoid orphan instance for Either and change definition of Monoid for Either

data VerRes e a = VErr e | VRes a
    deriving (Show, Eq)

instance Semigroup a => Semigroup (VerRes e a) where
    VRes a <> VRes b = VRes $ a <> b
    x@(VErr _) <> _ = x
    _ <> x@(VErr _) = x

instance (Monoid a, Semigroup a) => Monoid (VerRes e a) where
    mempty = VRes mempty
    mappend = (<>)

eitherToVerRes :: Either e a -> VerRes e a
eitherToVerRes (Right x) = VRes x
eitherToVerRes (Left e)  = VErr e

verResToEither :: VerRes e a -> Either e a
verResToEither (VRes x) = Right x
verResToEither (VErr e) = Left e

runExceptTV :: Monad m => ExceptT e m a -> m (VerRes e a)
runExceptTV = fmap eitherToVerRes . runExceptT

data WithSignature sigScheme a = WithSignature
    { wsSignature :: Signature sigScheme a
    , wsPublicKey :: PublicKey sigScheme
    , wsBody      :: a
    }

propagateSecondF :: Functor f => (f a, b) -> f (a, b)
propagateSecondF (fa, b) = (,b) <$> fa
