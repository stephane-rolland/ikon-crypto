module Main where

import Lib (loop)

import qualified System.Environment as SE
import qualified GHC.Int as GI


main :: IO ()
main = do
  commandLineArgs <- SE.getArgs
  let isOnlyOnce = getIsOnlyOnce commandLineArgs
  let storageDirectory = getStorageDirectory commandLineArgs
  let delay = getDelay commandLineArgs

  putStrLn $ "will request rates every " ++ (show delay) ++ " minutes"
  
  loop isOnlyOnce storageDirectory delay

getIsOnlyOnce :: [String] -> Bool
getIsOnlyOnce args = (not . null $ args) && any (\x -> x == "onlyonce") args

getStorageDirectory :: [String] -> String
getStorageDirectory args = head filtered
  where
    filtered = filter predicate args
    predicate ('/':pth) = True
    predicate _ = False
    
getDelay :: [String] -> GI.Int64
getDelay args = read d :: GI.Int64
  where
    delayStr = head filtered
    filtered = filter predicate args
    predicate ('d':'e':'l':'a':'y':'=':_) = True
    predicate _ = False
    d = parseDelay delayStr
    parseDelay s = drop 6 s
