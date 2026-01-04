module TestSpec (spec) where

import qualified Characters
import qualified Combinators
import qualified Optionals
import qualified Primitives
import qualified Spaces
import Test.Hspec

spec :: Spec
spec = do
  describe "Parser builder primitives" $ do
    Primitives.spec
    Characters.spec
    Combinators.spec
    Spaces.spec
    Optionals.spec
