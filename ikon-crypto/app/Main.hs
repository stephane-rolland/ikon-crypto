module Main where

import Lib (loop)

import qualified System.Environment as SE



main :: IO ()
main = do
  commandLineArgs <- SE.getArgs
  let isOnlyOnce = getIsOnlyOnce commandLineArgs
  let storageDirectory = getStorageDirectory commandLineArgs
  let delay = getDelay commandLineArgs

  putStrLn $ "will request rates every " ++ (show delay) ++ " minutes"
  
  loop isOnlyOnce storageDirectory

getIsOnlyOnce :: [String] -> Bool
getIsOnlyOnce args = (not . null $ args) && any (\x -> x == "onlyonce") args

getStorageDirectory :: [String] -> String
getStorageDirectory args = head filtered
  where
    filtered = filter predicate args
    predicate ('/':pth) = True
    predicate _ = False
    
getDelay :: [String] -> Int
getDelay args = read d :: Int
  where
    delayStr = head filtered
    filtered = filter predicate args
    predicate ('d':'e':'l':'a':'y':'=':_) = True
    predicate _ = False
    d = parseDelay delayStr
    parseDelay s = drop 6 s
