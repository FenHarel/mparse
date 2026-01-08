module Custom1Spec (spec) where

import Mparse.MParser ((//))
import qualified Mparse.MParser as MP
import Test.Hspec

type Location = (Int, Int, Int)

data ParseLocation = ParseLocation Location deriving (Show, Eq)

instance MP.ParseState ParseLocation where
  initialState = ParseLocation (0, 0, 0)
  consumeCharacter '\n' (ParseLocation (lcp, _, acp)) = ParseLocation (lcp + 1, 0, acp + 1)
  consumeCharacter _ (ParseLocation (lcp, rcp, acp)) = ParseLocation (lcp, rcp + 1, acp + 1)

type CustomParser = MP.MParser ParseLocation

customparse :: CustomParser a -> String -> MP.RichParsedData a Location
customparse = MP.parsedWithFormat onlyData onlyError
  where
    onlyData :: (String, ParseLocation, a) -> (String, Location, a)
    onlyData (input, ParseLocation l, a) = (input, l, a)
    onlyError = map (\(input, ParseLocation l, e) -> (input, l, e))

testFilePath :: String
testFilePath = "test/samples/custom1/test_1.txt"

data Name = Name String String deriving (Show, Eq)

data Phone = Phone Int Int Int deriving (Show, Eq)

data Profile = Profile Name Int String Phone String deriving (Show, Eq)

_dash :: CustomParser Char
_dash = MP.char '-'

profileHeader :: CustomParser String
profileHeader = MP.between _dashes _dashes _profile
  where
    _dashes :: CustomParser String
    _dashes = MP.counted 10 _dash
    _profile :: CustomParser String
    _profile = MP.exact "Profile"

nameField :: CustomParser Name
nameField = _label >> _field <* MP.spaces
  where
    _label :: CustomParser String
    _label = MP.exact "Name:" <* MP.spaces
    _field :: CustomParser Name
    _field =
      Name
        <$> (MP.repeated1 MP.letter <* MP.spaces)
        <*> (MP.repeated1 MP.letter <* MP.spaces)

ageField :: CustomParser Int
ageField = _label >> MP.nat <* MP.spaces
  where
    _label :: CustomParser String
    _label = MP.exact "Age:" <* MP.spaces

addressField :: CustomParser String
addressField = _label >> _field <* MP.spaces
  where
    _label :: CustomParser String
    _label = MP.exact "Address:" <* MP.spaces
    _field :: CustomParser String
    _field = MP.repeated1 MP.notNewLine

phoneField :: CustomParser Phone
phoneField = _label >> _field <* MP.spaces
  where
    _label :: CustomParser String
    _label = MP.exact "Phone Number:" <* MP.spaces
    _field :: CustomParser Phone
    _field =
      Phone
        <$> (_numbers 3 <* _dash)
        <*> (_numbers 3 <* _dash)
        <*> _numbers 4
      where
        _numbers :: Int -> CustomParser Int
        _numbers n = read <$> MP.counted n MP.digit

occupationField :: CustomParser String
occupationField = _label >> _field <* MP.spaces
  where
    _label :: CustomParser String
    _label = MP.exact "Occupation:" <* MP.spaces
    _field :: CustomParser String
    _field = MP.repeated1 MP.notNewLine

profile :: CustomParser Profile
profile =
  (profileHeader <* MP.newLine)
    >> Profile
      <$> (nameField <* MP.newLine)
      <*> (ageField <* MP.newLine)
      <*> (addressField <* MP.newLine)
      <*> (phoneField <* MP.newLine)
      <*> (occupationField <* MP.newLine)

profiles :: CustomParser [Profile]
profiles = MP.spaces >> (profile // (MP.repeated MP.newLine))

spec :: Spec
spec = do
  describe "Basic parsers" $ do
    it "result returns a value without consuming input" $ do
      value <- readFile testFilePath
      let value' = customparse profiles value
      let p1 = Profile (Name "John" "Doe") 45 "123 Fake St, Vancouver, BC, VKX 459, Canada" (Phone 566 859 4151) "Ditch Digger"
      let p2 = Profile (Name "Anakin" "Skywalker") 23 "1 Republic Ave, First Level, First Sector, A8S 5W9, Coruscant" (Phone 998 462 1352) "Jedi Knight"
      let p3 = Profile (Name "Jane" "Doe") 34 "56 Somewhere Blvd, Toronto, ON, M8S 3A2, Canada" (Phone 234 758 3165) "Dentist"
      let expected' = ("", (21, 0, 495), [p1, p2, p3])
      shouldBe value' (MP.ParsedData expected')
