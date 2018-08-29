module Snowdrop.Block.OSParams
       ( Time
       , OSParamsBuilder(..)
       ) where

import           Data.Time.Clock (UTCTime)

type Time = UTCTime

newtype OSParamsBuilder osparams = OSParamsBuilder { unOSParamsBuilder :: Time -> osparams }
