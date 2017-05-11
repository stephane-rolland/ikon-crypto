module Global where

import qualified Storage as S
import qualified Data.ByteString.Lazy.Char8 as DBLC
import qualified CryptoRateString as CRS
import qualified CryptoRate as CR
import qualified Data.Maybe as DM
import qualified Data.List as DL

data Global = Global
              {
                  day :: String
                , hour :: String
                , marketBitcoin :: Double
                , market5 :: Double
                , market10 :: Double
                , market1M :: Double
                , market100K :: Double
                , market50K :: Double
                , market20K :: Double
                , market10K :: Double
              }

instance Show Global where
  show (Global d hr mB m5 m10 m1M m100K m50K m20K m10K) =
    showMarkt mB    ++ " " ++
    showMarkt m5    ++ " " ++
    showMarkt m10   ++ " " ++
    showMarkt m1M   ++ " " ++
    showMarkt m100K ++ " " ++
    showMarkt m50K  ++ " " ++
    showMarkt m20K  ++ " " ++
    showMarkt m10K

showMarkt :: Double -> String
showMarkt d = padded
  where
    divided = d / 10000
    d' = ceiling divided
    stringDisplay = show d'
    l = length stringDisplay
    maxLength = 9
    spaceToAdd = maxLength - l
    pad = replicate spaceToAdd ' '
    padded = pad ++ stringDisplay

type Globals = [Global]

data GlobalVariation = GlobalVariation
                       {
                         from :: String,
                         to :: String,
                         gbl :: Global
                       }

instance Show GlobalVariation where
  show (GlobalVariation f t g) =
    f ++ " -> " ++ t ++ " = " ++ (show g)

type GlobalVariations = [GlobalVariation]

getIndexes :: Int -> [Int]
getIndexes l = i ++ i' ++ i'' ++ i''' 
  where
    i     = [1..9]
    i'    = [10,13..19]
    i''   = [20,25..99]
    i'''  = [100,120..l]

getGlobalVariations :: S.FileRates -> GlobalVariations
getGlobalVariations fileRates = globalVariations 
  where
    selected = selectFileRates $ reverse fileRates
    globals = getGlobal <$> selected
    globalVariations = makeGlobalDifferences globals

    selectFileRates :: S.FileRates -> S.FileRates
    selectFileRates frs = frs'
      where
        indexes = getIndexes l
        l = length frs
        frs' = getFileRatesAtIndex frs $ DL.nub indexes

getFileRatesAtIndex :: S.FileRates -> [Int] -> S.FileRates
getFileRatesAtIndex frs indexes = (frs !!) <$> ((\x -> x-1) <$>indexes)

makeGlobalDifferences :: Globals -> GlobalVariations
makeGlobalDifferences (g:gs) = (makeGlobalDifference g) <$> gs
makeGlobalDifferences _ = []

makeGlobalDifference :: Global -> Global -> GlobalVariation
makeGlobalDifference g g' = GlobalVariation f t gv
  where
    fromDay = day g
    fromHour = hour g
    toDay = day g'
    toHour = hour g'
    f = fromDay ++ " " ++ fromHour
    t = toDay ++ " " ++ toHour
    
    gv = Global "" "" mBitcoin m5 m10 m1M m100K m50K m20K m10K
    mBitcoin = makeDiff marketBitcoin g g'
    m5 = makeDiff market5 g g'
    m10 = makeDiff market10 g g'
    m1M = makeDiff market1M g g'
    m100K = makeDiff market100K g g'
    m50K = makeDiff market50K g g'
    m20K = makeDiff market20K g g'
    m10K = makeDiff market10K g g'

makeDiff :: (Global -> Double) -> Global -> Global -> Double
makeDiff f g g' = f g - f g'

getGlobal :: S.FileRate -> Global
getGlobal (S.FileRate fileName bs) = Global d t mBitcoin m5 m10 m1M m100K m50K m20K m10K
  where
    (d,t) = S.extractDateTime fileName
    
    cryptoRates = CRS.getCryptoRates bs
    rateBitcoin = CR.selectByName "Bitcoin" cryptoRates
    rates5 = take 5 cryptoRates
    rates10 = drop 1 $ take 11 cryptoRates   -- don't take BTC into account in this one, so as to reflect peripheral market
    rates1M = drop 1 $ CR.selectByMarket 1000000 cryptoRates   -- don't take BTC into account in this one, so as to reflect peripheral market
    rates100K = CR.selectByMarketRange 1000000 100000 cryptoRates
    rates50K = CR.selectByMarketRange 100000 50000 cryptoRates
    rates20K = CR.selectByMarketRange 50000 20000 cryptoRates
    rates10K = CR.selectByMarketRange 20000 10000 cryptoRates

    mBitcoin = getMarket rateBitcoin
    m5 = sum $ getMarket <$> rates5
    m10 = sum $ getMarket <$> rates10
    m1M = sum $ getMarket <$> rates1M
    m100K = sum $ getMarket <$> rates100K
    m50K = sum $ getMarket <$> rates50K
    m20K = sum $ getMarket <$> rates20K
    m10K = sum $ getMarket <$> rates10K

getMarket :: CR.CryptoRate -> Double
getMarket x = case markt of
                Just m -> m
                Nothing -> error "market not found" 
  where
    markt = CR.market_cap_usd x 

