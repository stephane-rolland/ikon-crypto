module Analyze where

import qualified Storage as S
import qualified Data.ByteString.Lazy.Char8 as DBLC
import qualified CryptoRateString as CRS
import qualified CryptoRate as CR
import qualified Data.Maybe as DM
import qualified Data.List as DL

data Analysis = Analysis GlobalVariations

instance Show Analysis where
  show (Analysis (g:gs)) = show g ++ "\n" ++ show (Analysis gs)
  show _ = "" 

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
              }

instance Show Global where
  show (Global d hr mB m5 m10 m1M m100K m50K) =
    showMarkt mB    ++ " " ++
    showMarkt m5    ++ " " ++
    showMarkt m10   ++ " " ++
    showMarkt m1M   ++ " " ++
    showMarkt m100K ++ " " ++
    showMarkt m50K

showMarkt :: Double -> String
showMarkt d = show d'
  where
    divided = d / 1000000
    d' = ceiling divided

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

analyze :: String -> IO ()
analyze storageDirectory = do
  putStrLn "analyzing"
  fileRates <- S.getRates storageDirectory
  let analyses = getAnalyses fileRates  
  putStrLn $ show analyses
  return ()

getAnalyses :: S.FileRates -> Analysis
getAnalyses rates = analysis
  where
    analysis = Analysis globalVariations
    globalVariations = getGlobalVariations rates

getGlobalVariations :: S.FileRates -> GlobalVariations
getGlobalVariations fileRates = globalVariations 
  where
    selected = selectFileRates $ reverse fileRates
    globals = getGlobal <$> selected
    globalVariations = makeGlobalDifferences globals

    selectFileRates :: S.FileRates -> S.FileRates
    selectFileRates frs = frs'
      where
        l = length frs
        indexesFar = [550,575..l]
        indexes = [1,2,3,5,7,10,15,20,25,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,230,260,280,300,325,350,375,400,425,450,475,500,525] ++ indexesFar
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
    
    gv = Global "" "" mBitcoin m5 m10 m1M m100K m50K
    mBitcoin = makeDiff marketBitcoin g g'
    m5 = makeDiff market5 g g'
    m10 = makeDiff market10 g g'
    m1M = makeDiff market1M g g'
    m100K = makeDiff market100K g g'
    m50K = makeDiff market50K g g'


makeDiff :: (Global -> Double) -> Global -> Global -> Double
makeDiff f g g' = f g - f g'

getGlobal :: S.FileRate -> Global
getGlobal (S.FileRate fileName bs) = Global d t mBitcoin m5 m10 m1M m100K m50K
  where
    (d,t) = S.extractDateTime fileName
    
    cryptoRates = CRS.getCryptoRates bs
    rateBitcoin = CRS.selectByName "Bitcoin" cryptoRates
    rates5 = take 5 cryptoRates
    rates10 = take 10 cryptoRates
    rates1M = CRS.selectByMarket 1000000 cryptoRates
    rates100K = CRS.selectByMarket 100000 cryptoRates
    rates50K = CRS.selectByMarket 50000 cryptoRates


    mBitcoin = getMarket rateBitcoin
    m5 = sum $ getMarket <$> rates5
    m10 = sum $ getMarket <$> rates10
    m1M = sum $ getMarket <$> rates1M
    m100K = sum $ getMarket <$> rates100K
    m50K = sum $ getMarket <$> rates50K

getMarket :: CR.CryptoRate -> Double
getMarket x = case markt of
                Just m -> m
                Nothing -> error "market not found" 
  where
    markt = CR.market_cap_usd x 

