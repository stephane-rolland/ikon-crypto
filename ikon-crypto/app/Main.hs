module Main where

import Lib (loop)

import qualified System.Environment as SE
import qualified GHC.Int as GI
import qualified UserConfig as C
import qualified Encryption as E
import qualified Control.Monad as CM

main :: IO ()
main = do

  configPath <- getConfigPath
  config <- C.readConfig configPath

  putStrLn $ "Enter password:"
  pwd <- getLine

  let password = E.getPassword32 pwd
  (isGoodPassword, isNewPassword) <- E.checkPassword config password

  CM.when isNewPassword $ do
    putStrLn $ "Please enter the K API key:"
    kAPIKey <- getLine
    E.createKAPIKey (C.kAPIKeyPath config) password kAPIKey

  kAPIKey <- E.readKAPIKey (C.kAPIKeyPath config) password 

  let config' = config { C.kAPIKey = kAPIKey } 

  putStrLn $ "will request rates every " ++ (show $ C.delayRetrieveRates config) ++ " minutes"
  
  loop config'

readConfigPath :: IO String
readConfigPath = do
    commandLineArgs <- SE.getArgs
    let configPath = getPath commandLineArgs
    return configPath

getPath :: [String] -> String
getPath args = head filtered
  where
    filtered = filter predicate args
    predicate ('/':pth) = True
    predicate _ = False

getConfigPath :: IO (String)
getConfigPath = do
  commandLineArgs <- SE.getArgs
  let configPath = head commandLineArgs 
  putStrLn $ "working in directory = " ++ configPath
  return $ configPath 
