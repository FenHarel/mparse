{-# LANGUAGE QuasiQuotes #-}

module LibSpec where

import DotEnvSpec
import FormatSpec
import ParserSpec
import Test.Hspec

spec :: Spec
spec = do
  ParserSpec.spec
  DotEnvSpec.spec
  FormatSpec.spec
