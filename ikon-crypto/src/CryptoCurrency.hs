module CryptoCurrency where

import qualified Storage as S
import qualified CryptoRate as CR
import qualified CryptoRateString as CRS
import qualified Data.Maybe as DM
import qualified Data.List as DL

data CryptoCurrency = CryptoCurrency
  {
      symbol :: String
    , eur :: Double
    , btc :: Double
    , market_usd :: Double
  }

type CryptoCurrencies = [CryptoCurrency]

instance Show CryptoCurrency where
  show c = show $ eur c

data CryptoCurrencyHistory = CryptoCurrencyHistory
  {
      code :: String
    , history :: CryptoCurrencies
  }

instance Show CryptoCurrencyHistory where
  show (CryptoCurrencyHistory c h) = separator ++ newLine ++ displayCode ++ " => " ++ histospaced 
    where
      separator = replicate 80 '-'
      newLine = "\n"
      displayCode = c
      histospaced ::  String
      histospaced = DL.intercalate " "  (show <$> h)


type CryptoCurrencyHistories = [CryptoCurrencyHistory]

getCryptoCurrencyHistories :: S.FileRates -> [String] -> CryptoCurrencyHistories
getCryptoCurrencyHistories rates lstCurrencies = histories
  where
    histories = getCryptoCurrencyHistory rates <$> lstCurrencies

getCryptoCurrencyHistory :: S.FileRates -> String -> CryptoCurrencyHistory
getCryptoCurrencyHistory rates c = CryptoCurrencyHistory c h
  where
    ratesCurrency = findCurrencyRate c <$> rates
    h = getHistory ratesCurrency

findCurrencyRate :: String -> S.FileRate -> CR.CryptoRate
findCurrencyRate c (S.FileRate fp bs) = cr
  where
    crs = CRS.getCryptoRates bs
    cr =  CR.selectBySymbol c crs 
  
getHistory :: CR.CryptoRates -> CryptoCurrencies
getHistory crs = mkCryptoCurrency <$> selectedCrs
  where
    indexes = getIndexes $ length crs
    selectedCrs = (\x -> crs !! (x-1)) <$> (reverse indexes)

getIndexes :: Int -> [Int]
getIndexes l = i ++ i' ++ i'' ++ i''' ++ i''''
  where
    i     = [1..9]
    i'    = [10,13..19]
    i''   = [20,25..49]
    i'''  = [50,60..99]
    i'''' = [100,125..l]

mkCryptoCurrency :: CR.CryptoRate -> CryptoCurrency
mkCryptoCurrency cr = c
  where
    c = CryptoCurrency s e b m
    s = DM.fromJust $ CR.symbol cr
    e = DM.fromJust $ CR.price_eur cr
    b = DM.fromJust $ CR.price_btc cr
    m = DM.fromJust $ CR.market_cap_usd cr
