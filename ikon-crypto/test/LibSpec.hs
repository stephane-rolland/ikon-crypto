module LibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Storage as S
import qualified CryptoRate as CR
import qualified Data.ByteString.Lazy.Char8 as DBLC
import qualified Data.ByteString.Lazy as DBL
import qualified Codec.Compression.BZip as CCBZ
import qualified Data.Aeson as DA
import qualified Data.Maybe as DM

main :: IO ()
main = hspec spec

oneCryptoRate = "/home/code/src/ikon-crypto/ikon-crypto/test/OneCryptoRate.json"
twoCryptoRates = "/home/code/src/ikon-crypto/ikon-crypto/test/TwoCryptoRates.json"

readCompressedFile :: String -> IO DBLC.ByteString
readCompressedFile fp = do
  content <- DBL.readFile $ fp
  return content

spec :: Spec
spec =
  describe "Lib" $ do
    it "works" $ do
      bs <- readCompressedFile oneCryptoRate
      let cr = DA.decode bs :: Maybe CR.CryptoRate
      
      DM.isJust cr `shouldBe` True
    --prop "ourAdd is commutative" $ \x y ->
    --  True `shouldBe` True
