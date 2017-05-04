module Request where

import Control.Lens
import qualified Data.ByteString.Lazy as BL
import Network.Wreq

requestCryptoRates :: String
requestCryptoRates = "https://api.coinmarketcap.com/v1/ticker/?convert=EUR&"

request :: IO (BL.ByteString)
request = do
  response <- get requestCryptoRates
  return $ view responseBody response
