import           Universum

import           Test.Hspec (hspec)
import           Test.Tasty (defaultMain, testGroup)

import           Spec (spec)
import           Test.Snowdrop.Core.ChangeSet (changeSetTests)
-- import           Test.Snowdrop.Core.Free (freeTests)

main :: IO ()
main = do
    hspec spec
    defaultMain $ testGroup "Snowdrop property tests"
        [ testGroup "ChangeSet property tests" changeSetTests
        -- , testGroup "Free property tests" freeTests
        ]
