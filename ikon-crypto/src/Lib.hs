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
import qualified GHC.Int as GI
import qualified UserConfig as C


loop :: C.Config -> IO () 
loop config = do
  putStrLn "Starting..."
  cs <- CC.newEmptyMVar

  let isLooping = not $ C.isOnlyOnce config 
  let storageDirectory = C.storageDirectory config
  let key = C.kAPIKey config
  let d = C.delayRetrieveRates config
  putStrLn $ "working with API key = "
  putStrLn $ "isLooping = " ++ (show isLooping)
  -- run once
  onTimer cs False storageDirectory

  --run in loops
  _ <- CC.forkIO $ do
    let delay = CCSL.mDelay d
    _ <- CCTi.repeatedTimer (onTimer cs isLooping storageDirectory) delay
    return ()
  CC.takeMVar cs
    
onTimer :: CC.MVar () -> Bool -> String -> IO ()
onTimer cs isLooping storageDirectory = do
  putStrLn "requesting"
  req <- R.request
  putStrLn "request received"
  S.store req storageDirectory
  CM.when isLooping $ do
    CC.putMVar cs ()
    return ()
  return ()

