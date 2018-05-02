import           Test.Tasty

import           Course.OptionalTest   (test_Optional)
import           Course.ValidationTest (test_Validation)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Tests" [
    test_Optional
  , test_Validation
  ]
