module Gen
  (genWhitespace) where

import qualified Hedgehog.Gen as Gen (element, list)
import qualified Hedgehog.Range as Range (constant)

genWhitespace :: Monad m => Gen.Gen m String
genWhitespace = Gen.list (Range.constant 0 10) (Gen.element " \t\n\r")
