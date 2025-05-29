module DotEnvSpec (spec) where

import Mparse.DotEnv
import Test.Hspec

spec :: Spec
spec = do
  describe "dotenv parser tests" $ do
    it "parsedFile" $ do
      result <- parsedDotEnvDebug "test/test.env"
      shouldBe
        result
        [ ("_some_var", "123This_is-my!#$VAR"),
          ("unquoted", "123this_is-my!"),
          ("another", "swag"),
          ("more", "idk999"),
          ("unquotedEscape", "sla\\nsh"),
          ("dquote", "hello\"world"),
          ("_uqSub", "swag@<<HOME>>${not_a_var}"),
          ("_uqSub", "swag@<docker compose exec web python main.py>$(not)"),
          ("_dqSub", "swag@<<HOME>>${not_a_var}"),
          ("_dqCom", "swag@<docker compose exec web python main.py>$(not a com)")
        ]
