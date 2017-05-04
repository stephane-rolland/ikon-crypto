module Request where

import Control.Lens
import qualified Data.ByteString.Lazy as BL
import qualified Network.Wreq as NW

requestCryptoRates :: String
requestCryptoRates = "https://api.coinmarketcap.com/v1/ticker/?convert=EUR&"

request :: IO (String)
request = do
  answer <- NW.get requestCryptoRates
  return $ show answer
