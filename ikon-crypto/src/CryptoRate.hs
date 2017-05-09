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
