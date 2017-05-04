{-# LANGUAGE OverloadedStrings  #-}
module Request where

import Control.Lens
import qualified Data.ByteString.Lazy as BL
import Network.Wreq

requestCryptoRates :: String
requestCryptoRates = "https://api.coinmarketcap.com/v1/ticker"

request :: IO (BL.ByteString)
request = do
  let opts = defaults & param "convert" .~ ["EUR"] 
  response <- getWith opts requestCryptoRates
  return $ view responseBody response
