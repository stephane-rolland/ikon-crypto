module Storage where
import qualified Data.ByteString.Lazy as DBL
import qualified Data.ByteString.Lazy.Char8 as DBLC
import qualified Data.Time.Clock as DTC
import qualified Data.Time.Calendar as DTCa
import qualified System.Directory as SD
import qualified Codec.Compression.BZip as CCBZ
import qualified Control.Monad as CM

type FileRate = (String,DBLC.ByteString)
type FileRates = [FileRate]

store :: DBL.ByteString -> String -> IO ()
store s storageDirectory = do
  filepth <- getFilePath storageDirectory  
  putStrLn $ "received crypto rates, size = " ++ (show $ DBL.length s)
  storeToFile filepth s

getFilePath :: String -> IO (String)
getFilePath dir = do
  t <- DTC.getCurrentTime
  let d = DTC.utctDay t
  let dt = DTC.utctDayTime t
  let nbAllSeconds = floor dt
  let nbHours = nbAllSeconds `div` 3600 :: Integer
  let nbMinutes = (nbAllSeconds - nbHours * 3600) `div` 60 :: Integer 
  let nbSeconds = nbAllSeconds `mod` 60 :: Integer
  
  let fulldir = getRatesDirectory dir 

  putStrLn $ "check or create" ++ fulldir
  SD.createDirectoryIfMissing True fulldir
  let filePath = fulldir ++
        (show d) ++ "#" ++
        (showTime nbHours) ++ ":" ++
        (showTime nbMinutes) ++ ":" ++
        (showTime nbSeconds) ++ ".crypto_rates.bzip"
  return $ filePath
  where
    showTime x = (if x > 9 then show x else "0" ++ show x) 

storeToFile :: String -> DBL.ByteString -> IO ()
storeToFile pth bs = do
  DBL.writeFile pth compresseByteString
  putStrLn $ "saved to " ++ pth
  return ()
  where
    compresseByteString = CCBZ.compress bs

getRatesDirectory :: String -> String
getRatesDirectory dir = dir ++ "/coincap-crypto-rates-bzip/"


getRates :: String -> IO FileRates
getRates dir = do
  let fulldir = getRatesDirectory dir 
  files <- SD.getDirectoryContents fulldir

  let filesWithoutSpecials = filter (\x -> x /= "." && x /= "..") files 

  putStrLn $ show filesWithoutSpecials
  
  rates <- CM.mapM (getRate fulldir) filesWithoutSpecials
  return rates

getRate :: String -> String -> IO FileRate
getRate fd s = do
  compressed <- DBL.readFile $ fd ++ "/" ++ s
  let decompressed = CCBZ.decompress compressed
  return (s, decompressed)
