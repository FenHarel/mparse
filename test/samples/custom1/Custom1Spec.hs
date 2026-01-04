module Custom1Spec (spec) where

import Mparse.EParser ((//))
import qualified Mparse.EParser as EP
import Test.Hspec

testFilePath :: String
testFilePath = "test/samples/custom1/test_1.txt"

data Name = Name String String deriving (Show, Eq)

data Phone = Phone Int Int Int deriving (Show, Eq)

data Profile = Profile Name Int String Phone String deriving (Show, Eq)

_dash :: EP.EParser Char
_dash = EP.char '-'

profileHeader :: EP.EParser String
profileHeader = EP.bracket _dashes _dashes _profile
  where
    _dashes :: EP.EParser String
    _dashes = EP.counted 10 _dash
    _profile :: EP.EParser String
    _profile = EP.exact "Profile"

nameField :: EP.EParser Name
nameField = _label >> _field <* EP.spaces
  where
    _label :: EP.EParser String
    _label = EP.exact "Name:" <* EP.spaces
    _field :: EP.EParser Name
    _field =
      Name
        <$> (EP.repeated1 EP.letter <* EP.spaces)
        <*> (EP.repeated1 EP.letter <* EP.spaces)

ageField :: EP.EParser Int
ageField = _label >> EP.nat <* EP.spaces
  where
    _label :: EP.EParser String
    _label = EP.exact "Age:" <* EP.spaces

addressField :: EP.EParser String
addressField = _label >> _field <* EP.spaces
  where
    _label :: EP.EParser String
    _label = EP.exact "Address:" <* EP.spaces
    _field :: EP.EParser String
    _field = EP.repeated1 EP.notNewLine

phoneField :: EP.EParser Phone
phoneField = _label >> _field <* EP.spaces
  where
    _label :: EP.EParser String
    _label = EP.exact "Phone Number:" <* EP.spaces
    _field :: EP.EParser Phone
    _field =
      Phone
        <$> (_numbers 3 <* _dash)
        <*> (_numbers 3 <* _dash)
        <*> _numbers 4
      where
        _numbers :: Int -> EP.EParser Int
        _numbers n = read <$> EP.counted n EP.digit

occupationField :: EP.EParser String
occupationField = _label >> _field <* EP.spaces
  where
    _label :: EP.EParser String
    _label = EP.exact "Occupation:" <* EP.spaces
    _field :: EP.EParser String
    _field = EP.repeated1 EP.notNewLine

profile :: EP.EParser Profile
profile =
  (profileHeader <* EP.newLine)
    >> Profile
      <$> (nameField <* EP.newLine)
      <*> (ageField <* EP.newLine)
      <*> (addressField <* EP.newLine)
      <*> (phoneField <* EP.newLine)
      <*> (occupationField <* EP.newLine)

profiles :: EP.EParser [Profile]
profiles = EP.spaces >> (profile // (EP.repeated EP.newLine))

spec :: Spec
spec = do
  describe "Basic parsers" $ do
    it "result returns a value without consuming input" $ do
      value <- readFile testFilePath
      let value' = EP.parseResults profiles value
      let p1 = Profile (Name "John" "Doe") 45 "123 Fake St, Vancouver, BC, VKX 459, Canada" (Phone 566 859 4151) "Ditch Digger"
      let p2 = Profile (Name "Anakin" "Skywalker") 23 "1 Republic Ave, First Level, First Sector, A8S 5W9, Coruscant" (Phone 998 462 1352) "Jedi Knight"
      let p3 = Profile (Name "Jane" "Doe") 34 "56 Somewhere Blvd, Toronto, ON, M8S 3A2, Canada" (Phone 234 758 3165) "Dentist"
      let input' = ("", (21, 0, 495))
      shouldBe value' [EP.Success ([p1, p2, p3], input')]
