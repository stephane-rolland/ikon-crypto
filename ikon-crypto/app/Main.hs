module Main where

import Lib (loop)

import qualified System.Environment as SE



main :: IO ()
main = do
  commandLineArgs <- SE.getArgs
  let isOnlyOnce = getIsOnlyOnce commandLineArgs
  let storageDirectory = getStorageDirectory commandLineArgs
  loop isOnlyOnce storageDirectory

getIsOnlyOnce :: [String] -> Bool
getIsOnlyOnce args = (not . null $ args) && any (\x -> x == "onlyonce") args

getStorageDirectory :: [String] -> String
getStorageDirectory args = head filtered
  where
    filtered = filter predicate args
    predicate ('/':pth) = True
    predicate _ = False
    
    
    
