module Analyze where

import qualified Storage as S

analyze :: String -> IO ()
analyze storageDirectory = do
  putStrLn "analyzing"

  rates <- S.getRates storageDirectory
  return ()
  
  

