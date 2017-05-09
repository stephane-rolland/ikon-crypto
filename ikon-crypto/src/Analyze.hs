module Analyze where

import qualified Storage as S
import qualified Data.ByteString.Lazy.Char8 as DBLC
import qualified CryptoRateString as CRS
import qualified CryptoRate as CR
import qualified Data.Maybe as DM

data Analysis = Analysis GlobalVariations
              deriving (Show)

data Global = Global
              {
                  marketBitcoin :: Double
                , market5 :: Double
                , market10 :: Double
                , market1M :: Double
                , market100K :: Double
                , market50K :: Double
              }
              deriving (Show)
type Globals = [Global]

data GlobalVariation = GlobalVariation Global
                       deriving Show
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
    selected = selectFileRates fileRates
    globals = fmap getGlobal selected
    globalVariations = makeGlobalDifferences globals

selectFileRates :: S.FileRates -> S.FileRates
selectFileRates frs = frs'
  where
    l = length frs
    fibo = [1,2,3,5,8,10,20,30,50,80,100,200,300,l]
    indexes = reverse $  (quot l) <$> fibo
    frs' = getFileRatesAtIndex frs indexes

getFileRatesAtIndex :: S.FileRates -> [Int] -> S.FileRates
getFileRatesAtIndex frs indexes = (frs !!) <$> ((\x -> x-1) <$>indexes)

makeGlobalDifferences :: Globals -> GlobalVariations
makeGlobalDifferences (g:g':gs) = makeGlobalDifference g g' : (makeGlobalDifferences $ g' : gs)
makeGlobalDifferences _ = []

makeGlobalDifference :: Global -> Global -> GlobalVariation
makeGlobalDifference g g' = GlobalVariation gv
  where
    gv = Global mBitcoin m5 m10 m1M m100K m50K
    mBitcoin = makeDiff marketBitcoin g g'
    m5 = makeDiff market5 g g'
    m10 = makeDiff market10 g g'
    m1M = makeDiff market1M g g'
    m100K = makeDiff market100K g g'
    m50K = makeDiff market50K g g'


makeDiff :: (Global -> Double) -> Global -> Global -> Double
makeDiff f g g' = f g - f g'

getGlobal :: S.FileRate -> Global
getGlobal (fileName, bs) = Global mBitcoin m5 m10 m1M m100K m50K
  where
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
