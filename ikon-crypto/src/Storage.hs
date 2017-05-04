module Storage where
import qualified Data.ByteString.Lazy as BL
import qualified Data.Time.Clock as DTC
import qualified Data.Time.Calendar as DTCa
import qualified System.Directory as SD

store :: BL.ByteString -> String -> IO ()
store s storageDirectory = do
  filepth <- getFilePath storageDirectory  
  BL.putStrLn s
  storeToFile filepth s
  putStrLn $ "stored to file = " ++ filepth

getFilePath :: String -> IO (String)
getFilePath dir = do
  t <- DTC.getCurrentTime
  let d = DTC.utctDay t
  let dt = DTC.utctDayTime t
  let nbAllSeconds = floor dt
  let nbHours = nbAllSeconds `div` 3600 :: Integer
  let nbMinutes = (nbAllSeconds - nbHours * 3600) `div` 60 :: Integer 
  let nbSeconds = nbAllSeconds `mod` 60 :: Integer
  
  let subdir = "coincap-crypto-rates/"
  let fulldir = dir ++ subdir

  putStrLn $ "should create" ++ fulldir
  SD.createDirectoryIfMissing True fulldir
  
  let filePath = fulldir ++
                 (show d) ++ "#" ++
                 (show nbHours) ++ ":" ++
                 (show nbMinutes) ++ ":" ++
                 (show nbSeconds) ++ ".crypto_rates"
  return $ filePath
  
storeToFile :: String -> BL.ByteString -> IO ()
storeToFile pth bs = do
  BL.writeFile pth bs
  putStrLn $ "saved to " ++ pth
  return ()
