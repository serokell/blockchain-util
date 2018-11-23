import           Universum

import           Test.Tasty (defaultMain, testGroup)
import           Test.Snowdrop.Core.ChangeSet (changeSetTests)
-- import           Test.Snowdrop.Core.Free (freeTests)

main :: IO ()
main = do
    defaultMain $ testGroup "Snowdrop property tests"
        [ testGroup "ChangeSet property tests" changeSetTests
        -- , testGroup "Free property tests" freeTests
        ]
