module Main where

import Lib (loop)

import qualified System.Environment as SE



main :: IO ()
main = do
  commandLineArgs <- SE.getArgs
  let isOnlyOnce = getIsOnlyOnce commandLineArgs
  loop isOnlyOnce

getIsOnlyOnce :: [String] -> Bool
getIsOnlyOnce args = (not . null $ args) && any (\x -> x == "onlyonce") args
