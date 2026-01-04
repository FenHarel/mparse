module Format.Tests (spec) where

import qualified Format.FormatSpec
import Test.Hspec

spec :: Spec
spec = do
  Format.FormatSpec.spec
