module FormatSpec (spec) where

import Mparse.Format
import Test.Hspec

spec :: Spec
spec = do
  describe "fstring tests" $ do
    it "iFmt" $ shouldBe (iFmt "{0}: <{1}> {2} added to group {3}" ["2025-05-01", "USER", "name@email.com", "FTE"]) "2025-05-01: <USER> name@email.com added to group FTE"
