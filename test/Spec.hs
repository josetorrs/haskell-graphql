import Test.Hspec
import Test.QuickCheck

import Data.Either
import System.FilePath

import qualified Data.Text.IO as T

import Src.GraphQL

testParse file = do
  let fp = "test" </> "lang" </> file
  file <- T.readFile fp
  let doc = parseDocument fp file
  return doc

main :: IO ()
main = hspec $ parallel $ do
  describe "Data.GraphQL" $ do
    -- it "should parse basic queries correctly." $ do
    --   head [] `shouldThrow` anyException
    it "should parse mutation queries correctly" $ do
      result <- testParse "mutation.ql"
      result `shouldSatisfy` isRight
