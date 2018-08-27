module Snowdrop.Block.OSParams
       ( OSParams (..)
       , Time
       , OSParamsBuilder(..)
       ) where

import           Data.Time.Clock (UTCTime)

type Time = UTCTime

data OSParams = OSParams
    { currentTime :: Time
    , startTime   :: Time
    }

newtype OSParamsBuilder = OSParamsBuilder { unOSParamsBuilder :: Time -> OSParams }
