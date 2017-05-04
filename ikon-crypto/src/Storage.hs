module Storage where
import qualified Data.ByteString.Lazy as BL

store :: BL.ByteString -> IO ()
store s =
  BL.putStrLn s
