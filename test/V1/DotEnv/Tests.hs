module DotEnv.Tests (spec) where

import qualified DotEnv.DotEnvSpec
import Test.Hspec

spec :: Spec
spec = do
  DotEnv.DotEnvSpec.spec
