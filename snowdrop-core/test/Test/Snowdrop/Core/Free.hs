{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Test.Snowdrop.Core.Free
       ( freeTests
       ) where

import           Universum

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Hedgehog (MonadGen, Property, PropertyT, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)

import           Snowdrop.Core (CSMappendException (..), ERoComp, Prefix (..), StatePException (..))
import           Snowdrop.Util (eitherToVerRes, inj, verResToEither)

import           Test.Snowdrop.Core.Executor (Counter (..), TestCtx)
import           Test.Snowdrop.Core.Types (Command (..), Err, Id, IdValB, InterpetResult, T (..),
                                           Value, getOperNum, getRes, runCommands)

freeTests :: [TestTree]
freeTests =
    [ testProperty "length of two concatenated ERoComp" prop_ercLen
    , testProperty "ERoComp: result of execution = concatenation" prop_ercConcRes
    ]

{- | Check that length of two concatenated 'ERoComp' is satisfies
the following formula:

@
i1 + i2 + max(q1, q2) >= i + q
@

where
@q1@ — number of query operations in the first 'ERoComp',
@i1@ — number of iterator operations in the first 'ERoComp',
@q2@ — number of query operations in the second 'ERoComp',
@i2@ — number of iterator operations in the second 'ERoComp',
@q@ — number of query operations in the 'ERoComp's concatenation,
@i@ — number of iterator operations in the 'ERoComp's concatenation,
-}

prop_ercLen :: Property
prop_ercLen = property $ do
    finMap <- genIdValList
    erpc1 <- genEReaderComp finMap
    erpc2 <- genEReaderComp finMap
    let erpcRes  = erpc1 <> erpc2
        cnts     = liftA2 (,) (getOperNum erpc1) (getOperNum erpc2)
        cntJ     = getOperNum erpcRes
    case (cnts, cntJ) of
        (Left _, Left _) -> assert True
        (Right (Counter q1 i1 m1, Counter q2 i2 m2), Right (Counter q i m)) ->
            assert (i1 + i2 + m1 + m2 + q1 + q2 == i + q + m)
        _ -> assert False
    -- TODO adjust these tests for JoinExecT as it will be ready
    -- assert (i1 + i2 + max q1 q2 >= i + q)

-- | Check that the results of executions of several `ERoComp`
-- should be the same as the result of their concatenation.
prop_ercConcRes :: Property
prop_ercConcRes = property $ do
    finList <- genIdValList
    erpc1 <- genEReaderComp finList
    erpc2 <- genEReaderComp finList
    let erpcRes  = erpc1 <> erpc2
    let finalMap = Map.fromList finList
    let res1 = getRes finalMap erpc1
    let res2 = getRes finalMap erpc2
    let res  = getRes finalMap erpcRes
    let resVerRes = verResToEither $ eitherToVerRes res1 <> eitherToVerRes res2
    resVerRes === res

----------------------------------------------------------------------------
-- Generators
----------------------------------------------------------------------------

lenRange :: Range.Range Int
lenRange = Range.constant 0 10

-- | Generates random 'Int'.
genAnyInt :: MonadGen m => m Int
genAnyInt = Gen.int Range.constantBounded

-- | Generates random 'String'.
genString :: MonadGen m => m String
genString = Gen.string lenRange Gen.alphaNum

-- | Generates random 'T'. (It applies to 'Id', 'Value', 'B')
genT :: MonadGen m => m T
genT = Gen.choice [TI <$> genAnyInt, TS <$> genString]

-- | Generator for id - value pairs, used in `Map` generation.
genIdVal :: MonadGen m => m (Id, Value)
genIdVal = liftA2 (,) genT genT

-- | Generates the list of id - value.
genIdValList :: Monad m => PropertyT m [(Id, Value)]
genIdValList = forAll (Gen.list lenRange genIdVal)

-- | Generates 'QuerySet' command.
genQuerySet :: MonadGen m => [(Id, Value)] -> m Command
genQuerySet res = do
    setFromRes <- Map.keysSet . Map.fromList <$> Gen.subsequence res
    randSet    <- Gen.set lenRange genT
    pure $ QuerySet (Set.union randSet setFromRes)

-- | Generates 'QueryOne' command.
genQueryOne :: MonadGen m => m Command
genQueryOne = QueryOne <$> genT

-- | Generates random 'Err'.
genError :: MonadGen m => m Err
genError = Gen.element
    [ inj QueryProjectionFailed
    , inj IteratorProjectionFailed
    , inj (CSMappendException (TS "<generated exception>"))
    ]

-- | Generates 'ThrowError' command.
genThrowError :: MonadGen m => m Command
genThrowError = ThrowError <$> genError

-- | Generates random 'Prefix' (could be only @Prefix 0@, @Prefix 1@ or @Prefix 2@)
genPrefix :: MonadGen m => m Prefix
genPrefix = Prefix <$> Gen.int (Range.constant 0 3)

-- | Generates random 'IdValB'.
genIdValB :: MonadGen m => m IdValB
genIdValB = Gen.enumBounded

-- | Generates 'IteratorFor' command.
genIteratorFor :: MonadGen m => m Command
genIteratorFor = liftA3 IteratorFor genPrefix genT genIdValB

-- | Generates random operation.
-- Note, that the probability of 'ThrowError' is smaller that anything else
-- to make tests reasonable.
genCommand :: MonadGen m => [(Id, Value)] -> m Command
genCommand res = Gen.frequency
    [ (50, genQuerySet res)
    , (50, genQueryOne)
    , (50, genIteratorFor)
    , (1,  genThrowError)
    ]

-- | Generates the list of operations.
genCommands :: MonadGen m => [(Id, Value)] -> m [Command]
genCommands = Gen.list lenRange . genCommand

genEReaderComp
    :: Monad m
    => [(Id, Value)]
    -> PropertyT m (ERoComp Err Id Value (TestCtx Id Value) InterpetResult)
genEReaderComp l = runCommands <$> forAll (genCommands l)
