module Encryption where

import qualified UserConfig as C
import qualified System.Directory as SD
import qualified Control.Monad as CM
import qualified Data.ByteString.Char8 as DBC
import qualified Crypto.Simple.CTR as CSC

type PasswordOk = Bool
type PasswordNew = Bool
type FilePth = String
type Password = String


getPassword32 :: String -> String
getPassword32 pw = take 32 lengthened
  where
    lengthened = pw ++ pw ++ pw ++ pw ++ pw ++ pw ++ pw ++ pw ++ pw ++ pw 

checkPassword :: C.Config -> String -> IO (PasswordOk,PasswordNew)
checkPassword cfg pw = do

  let filePath = C.passwordPath cfg
  isPasswordExisting <- SD.doesFileExist filePath
  CM.when (not isPasswordExisting) $ do
    putStrLn $ "first use ! welcome !"
    createPassword filePath pw

  passwordSaved <- readPassword filePath pw

  let isPasswordOk = passwordSaved == pw

  CM.when (not isPasswordOk) $ do
    putStrLn $ "password is not good"
  
  return $ (isPasswordOk, not isPasswordExisting)

createPassword :: FilePth -> String -> IO ()
createPassword filePath pw = do
  encrypted <- CSC.encrypt pwbs pwbs
  putStr "encrypted password = " 
  DBC.putStrLn $ encrypted

  DBC.writeFile filePath encrypted
  where
    pwbs = DBC.pack pw

  
createKAPIKey :: FilePth -> Password -> String -> IO ()
createKAPIKey fp pw k = do
  encrypted <- CSC.encrypt pwbs kbs
  DBC.writeFile fp encrypted
  where
    pwbs = DBC.pack pw
    kbs = DBC.pack k

readPassword :: FilePth -> Password -> IO String
readPassword fp pw = do
  encrypted <- DBC.readFile fp
  let pwbs = DBC.pack pw
  decrypted <- CSC.decrypt pwbs encrypted
  let key = DBC.unpack decrypted
  return key
  
readKAPIKey :: FilePth -> Password -> IO String
readKAPIKey fp pw = do
  encrypted <- DBC.readFile fp
  let pwbs = DBC.pack pw
  decrypted <- CSC.decrypt pwbs encrypted
  let key = DBC.unpack decrypted
  return key
