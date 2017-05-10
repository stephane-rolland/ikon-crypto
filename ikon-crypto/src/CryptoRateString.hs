module CryptoRateString where

import Prelude hiding (id)
import qualified Data.ByteString.Lazy.Char8 as DBLC
import qualified Data.Aeson as DA
import qualified Data.Aeson.Types as DAT
import GHC.Generics
import qualified Data.Maybe as DM
import qualified CryptoRate as CR

type CryptoRateStrings = [CryptoRateString]
data CryptoRateString = CryptoRateString
                  {
                    id :: Maybe String,
                    name :: Maybe String,
                    symbol :: Maybe String,
                    rank :: Maybe String,
                    price_usd :: Maybe String,
                    price_btc :: Maybe String,
                    field_24h_volume_usd :: Maybe String,
                    market_cap_usd :: Maybe String,
                    available_supply :: Maybe String,
                    total_supply :: Maybe String,
                    percent_change_1h :: Maybe String,
                    percent_change_24h :: Maybe String,
                    percent_change_7d :: Maybe String,
                    last_updated :: Maybe String,
                    price_eur :: Maybe String,
                    field_24h_volume_eur :: Maybe String,
                    market_cap_eur :: Maybe String
                    }
                  deriving (Generic, Show)

instance DA.FromJSON CryptoRateString where
  parseJSON = DA.genericParseJSON DA.defaultOptions { DAT.fieldLabelModifier = addPrefix }

instance DA.ToJSON CryptoRateString where
  toJSON = DA.genericToJSON DA.defaultOptions { DAT.fieldLabelModifier = removePrefix }
  toEncoding = DA.genericToEncoding DA.defaultOptions { DAT.fieldLabelModifier = removePrefix }

mkCryptoRate :: CryptoRateString -> CR.CryptoRate
mkCryptoRate crstr = cr
  where
    csi = convertSelectorInteger
    csd = convertSelectorDouble
    cr = CR.CryptoRate
         {
             CR.id = id (crstr :: CryptoRateString)
           , CR.name = name (crstr :: CryptoRateString)
           , CR.symbol = symbol (crstr :: CryptoRateString)
           , CR.rank = csi rank crstr
           , CR.price_usd = csd price_usd crstr
           , CR.price_btc = csd price_btc crstr
           , CR.field_24h_volume_usd = csd field_24h_volume_usd crstr
           , CR.market_cap_usd = csd market_cap_usd crstr
           , CR.available_supply = csd available_supply crstr
           , CR.total_supply = csd total_supply crstr
           , CR.percent_change_1h = csd percent_change_1h crstr
           , CR.percent_change_24h = csd percent_change_24h crstr
           , CR.percent_change_7d = csd percent_change_7d crstr
           , CR.last_updated = csi last_updated crstr
           , CR.price_eur = csd price_eur crstr
           , CR.field_24h_volume_eur = csd field_24h_volume_eur crstr
           , CR.market_cap_eur = csd market_cap_eur crstr
         }

convertSelectorInteger :: ( CryptoRateString -> Maybe String ) -> CryptoRateString -> Maybe Integer
convertSelectorInteger f crstr = (\x -> read x :: Integer) <$> res
  where
    res = f crstr
    
convertSelectorDouble :: ( CryptoRateString -> Maybe String ) -> CryptoRateString -> Maybe Double
convertSelectorDouble f crstr = (\x -> read x :: Double) <$> res
  where
    res = f crstr

addPrefix :: String -> String
addPrefix s@('2':_) = "field_" ++ s
addPrefix s = s

removePrefix :: String -> String
removePrefix ('f':'i':'e':'l':'d':'_':s) = s
removePrefix s = s

getCryptoRates :: DBLC.ByteString -> CR.CryptoRates
getCryptoRates bs = case eitherCryptoRates of
                      Right crs -> mkCryptoRate <$> crs
                      Left msg -> error msg
  where
    eitherCryptoRates = DA.eitherDecode bs :: Either String CryptoRateStrings
    
    errorMsg = "crypto races not decoded from = " ++ jsondata
    jsondata = DBLC.unpack bs

