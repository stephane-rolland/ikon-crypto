module Lib
    (
      loop
    ) where

import qualified Control.Concurrent.Timer as CCTi
import qualified Control.Concurrent.Suspend.Lifted as CCSL
import qualified Request as R
import qualified Storage as S
import Control.Type.Operator
import qualified Control.Concurrent as CC
import qualified Control.Monad as CM
import qualified UserConfig as C
import qualified Analyze as A

loop :: C.Config -> IO () 
loop config = do
  putStrLn "Starting..."
  cs <- CC.newEmptyMVar

  let isLooping = not $ C.isOnlyOnce config 
  let storageDirectory = C.storageDirectory config
  let key = C.kAPIKey config
  let d  = C.delayRetrieveRates config
  let dd = C.delayAnalyzeOrders config
  let lstCurrencies = C.listCurrenciesOfInterest config
  putStrLn $ "working with API key = "
  putStrLn $ "isLooping = " ++ (show isLooping)

  -- run once
  doRates storageDirectory lstCurrencies
  doOrders

  --run in loops
  _ <- CC.forkIO $ do
    let delay = CCSL.mDelay d
    _ <- CCTi.repeatedTimer (onTimerRates cs isLooping storageDirectory lstCurrencies) delay
    return ()

  _ <- CC.forkIO $ do
    let delay = CCSL.mDelay dd
    _ <- CCTi.repeatedTimer onTimerOrders delay
    return ()

  CC.takeMVar cs

doRates :: String -> [String] -> IO ()
doRates dir lst = do
  requestRates dir
  analyzeRates dir lst
  automaticOrders
  
onTimerRates :: CC.MVar () -> Bool -> String -> [String] -> IO ()
onTimerRates cs isLooping storageDirectory lst = do
  doRates storageDirectory lst
  CM.when (not isLooping) $ do
    CC.putMVar cs ()
    return ()
  return ()

doOrders :: IO ()
doOrders = do
  requestOrders
  analyzeOrders
  orderProposal
  automaticOrders

onTimerOrders :: IO ()
onTimerOrders = do
  doOrders
  return ()

requestRates :: String -> IO ()
requestRates storageDirectory = do
  putStrLn "requesting rates"
  req <- R.request
  putStrLn "rates received"
  S.store req storageDirectory

analyzeRates :: String -> [String] -> IO ()
analyzeRates storageDirectory lst = do
  putStrLn "analyzing rates..."
  A.analyze storageDirectory lst

requestOrders :: IO ()
requestOrders = do
  putStrLn "requesting orders"

analyzeOrders :: IO ()
analyzeOrders = do
  putStrLn "analyzing orders..."

orderProposal :: IO ()
orderProposal = do
  putStrLn "order proposal"

automaticOrders :: IO ()
automaticOrders = do
  putStrLn "automatic orders..."
