module Lib
    (
      loop
    ) where

import qualified Control.Concurrent.Timer as CCTi
import qualified Control.Concurrent.Suspend.Lifted as CCSL
import qualified Request as R
import Control.Type.Operator
import qualified Control.Concurrent as CC
import qualified Control.Monad as CM


loop :: Bool -> IO () 
loop isLooping = do
  putStrLn "Starting..."
  cs <- CC.newEmptyMVar
  _ <- CC.forkIO $ do
    let delay = CCSL.mDelay 1
    _ <- CCTi.repeatedTimer (onTimer cs isLooping) delay
    return ()
  CC.takeMVar cs
    
onTimer :: CC.MVar () -> Bool -> IO ()
onTimer cs isLooping = do
  putStrLn "requesting"
  req <- R.request
  putStrLn $ show req
  putStrLn "request received"
  CM.when isLooping $ do
    CC.putMVar cs ()
    return ()
  return ()

