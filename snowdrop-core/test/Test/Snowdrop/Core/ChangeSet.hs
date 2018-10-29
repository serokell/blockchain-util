{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Test.Snowdrop.Core.ChangeSet
       ( changeSetTests
       ) where

import           Universum

import           Data.Default (def)
import           Hedgehog (MonadGen, Property, PropertyT, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)

import           Snowdrop.Core (HChangeSet, HChangeSetEl (..), ValueOp (..), ValueOpEx (..),
                                mappendChangeSet)
import           Snowdrop.Util

--  'Int' is used in types for simplicity.

changeSetTests :: [TestTree]
changeSetTests =
    [ testProperty "ValueOpEx Semigroup associativity law" prop_valueOpExAss
    , testProperty "ChangeSet monoid left identity law" prop_memptyCSL
    , testProperty "ChangeSet monoid right identity law" prop_memptyCSR
    , testProperty "ChangeSet mappend" prop_changeSetAss
    , testProperty "VerRes Semigroup associativity law" prop_verResAss
    , testProperty "VerRes monoid left identity law" prop_memptyVerResL
    , testProperty "VerRes monoid right identity law" prop_memptyVerResR
    ]

{- | The semigroup associativity axiom:

@
x <> (y <> z) ≡ (x <> y) <> z
@

-}
prop_valueOpExAss :: Property
prop_valueOpExAss = checkAssociativity genValueOpEx

{- | Identity law for monoid

@
x <> mempty = x
@

-}
prop_memptyCSL :: Property
prop_memptyCSL = property $ do
    x <- genChangeSet
    mappendChangeSet x def === Right x

{- | Identity law for monoid

@
mempty <> x = x
@

-}
prop_memptyCSR :: Property
prop_memptyCSR = property $ do
    x <- genChangeSet
    mappendChangeSet def x === Right x


-- | 'ChangeSet' custom mappend law.
prop_changeSetAss :: Property
prop_changeSetAss = property $ do
    x <- genChangeSet
    y <- genChangeSet
    z <- genChangeSet

    let xy = mappendChangeSet x y
    let yz = mappendChangeSet y z

    liftEithers (Right x) yz === liftEithers xy (Right z)
  where
    liftEithers a b = a >>= \x -> b >>= \y -> mappendChangeSet x y

prop_verResAss :: Property
prop_verResAss = checkAssociativity genVerRes

prop_memptyVerResL :: Property
prop_memptyVerResL = property $ do
    x <- genVerRes
    x <> mempty === x

prop_memptyVerResR :: Property
prop_memptyVerResR = property $ do
    x <- genVerRes
    mempty <> x === x

----------------------------------------------------------------------------
-- Generate values
----------------------------------------------------------------------------

-- | Generates random 'Int'.
genAnyInt :: MonadGen m => m Int
genAnyInt = Gen.int $ Range.constantBounded @Int

-- | Generates random 'ValueOp' with random 'Int'.
genValueOp :: MonadGen m => m (ValueOp Int)
genValueOp = do
    n <- genAnyInt
    Gen.element [ New n , Upd n , Rem, NotExisted ]

-- | Generates random 'ValueOpEx'.
genValueOpEx :: Monad m => PropertyT m (ValueOpEx Int)
genValueOpEx = forAll valChooser
  where
    valChooser :: MonadGen m => m (ValueOpEx Int)
    valChooser = do
        valOp <- genValueOp
        Gen.element
            [ Op valOp
            , Err
            ]

data Comp
type instance HKeyVal Comp = '(Int, Int)

-- | Generates random 'ChangeSet'.
genChangeSet :: Monad m => PropertyT m (HChangeSet '[Comp])
genChangeSet = forAll $ rone . HChangeSetEl <$> Gen.map (Range.constant 1 100) genPair
  where
    genPair :: MonadGen m => m (Int, ValueOp Int)
    genPair = do
        k <- genAnyInt
        v <- genValueOp
        pure (k, v)

genVerRes :: Monad m => PropertyT m (VerRes Int (Sum Int))
genVerRes = forAll $ do
    n <- genAnyInt
    Gen.element [ VErr n, VRes (Sum n) ]

----------------------------------------------------------------------------
-- Util
----------------------------------------------------------------------------

checkAssociativity :: (Eq a, Show a, Semigroup a) => PropertyT IO a -> Property
checkAssociativity genVal = property $ do
    x <- genVal
    y <- genVal
    z <- genVal

    (x <> (y <> z)) === ((x <> y) <> z)
