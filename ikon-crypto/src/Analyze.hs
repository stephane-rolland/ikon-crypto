module Analyze where

import qualified Storage as S

import qualified Global as G
import qualified CryptoCurrency as CC
import qualified System.Process as SP

data Analysis = Analysis
  {
      globalVariations :: G.GlobalVariations
    , cryptoCurrencyHistories :: CC.CryptoCurrencyHistories
  }
  
instance Show Analysis where
  show (Analysis (g:gs) l) = show g ++ "\n" ++ show (Analysis gs l)
  show (Analysis [] (l:ls)) = show l ++ "\n" ++ show (Analysis [] ls)
  show _ = "" 

clear = putStr "\ESC[2J"

clearScreen :: IO ()
clearScreen = do
  _ <- SP.system "reset"
  return ()

analyze :: String -> [String] -> IO ()
analyze storageDirectory lstCurrencies = do
  _ <- clearScreen
  putStrLn "_____________________________________________________________"
  putStrLn "analyzing"
  fileRates <- S.getRates storageDirectory
  let analyses = getAnalyses fileRates lstCurrencies 
  putStrLn $ show analyses
  return ()

getAnalyses :: S.FileRates -> [String] -> Analysis
getAnalyses rates lstCurrencies = analysis
  where
    analysis = Analysis glvs cchs
    glvs = G.getGlobalVariations rates
    cchs = CC.getCryptoCurrencyHistories rates lstCurrencies
