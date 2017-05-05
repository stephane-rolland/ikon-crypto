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


loop :: Bool -> String -> IO () 
loop isLooping storageDirectory = do
  putStrLn "Starting..."
  cs <- CC.newEmptyMVar
  _ <- CC.forkIO $ do
    let delay = CCSL.mDelay 30
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

