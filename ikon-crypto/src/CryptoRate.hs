module CryptoRate where

import Prelude hiding (id)

type CryptoRates = [CryptoRate]

data CryptoRate = CryptoRate
                  {
                    id :: Maybe String,
                    name :: Maybe String,
                    symbol :: Maybe String,
                    rank :: Maybe Integer,
                    price_usd :: Maybe Double,
                    price_btc :: Maybe Double,
                    field_24h_volume_usd :: Maybe Double,
                    market_cap_usd :: Maybe Double,
                    available_supply :: Maybe Double,
                    total_supply :: Maybe Double,
                    percent_change_1h :: Maybe Double,
                    percent_change_24h :: Maybe Double,
                    percent_change_7d :: Maybe Double,
                    last_updated :: Maybe Integer,
                    price_eur :: Maybe Double,
                    field_24h_volume_eur :: Maybe Double,
                    market_cap_eur :: Maybe Double
                  }
                  deriving (Show)

selectBySymbol :: String -> CryptoRates -> CryptoRate
selectBySymbol symbolCr crs = head $ filter predicate crs
  where
    predicate cr = case s of
                      Just c -> c == symbolCr
                      Nothing -> False
      where
        s = symbol cr


selectByName :: String -> CryptoRates -> CryptoRate
selectByName nameCr crs = head $ filter predicate crs
  where
    predicate cr = case mn of
                      Just c -> c == nameCr
                      Nothing -> False
      where
        mn = name cr

selectByMarket :: Double -> CryptoRates -> CryptoRates
selectByMarket i crs = filter predicate crs
  where
    predicate cr = case mm of
                     Just c -> c >= i
                     Nothing -> False
      where
        mm = market_cap_usd cr 

selectByMarketRange :: Double -> Double -> CryptoRates -> CryptoRates
selectByMarketRange iMax iMin crs = filter predicate crs
  where
    predicate cr = case mm of
                     Just c -> c >= iMin && c <= iMax
                     Nothing -> False
      where
        mm = market_cap_usd cr 
