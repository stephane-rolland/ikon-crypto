module UserConfig where
import qualified GHC.Int as GI

type Lines = [String]

data Config = Config {
                         configDirectory :: String
                       , passwordPath :: String
                       , kAPIKeyPath :: String
                       , isOnlyOnce :: Bool
                       , storageDirectory :: String
                       , delayRetrieveRates :: GI.Int64
                       , delayAnalyzeOrders :: GI.Int64
                       , kAPIKey :: String
                       , listCurrenciesOfInterest :: [String]
                     }
              deriving (Show)

readConfig :: String -> IO Config
readConfig pth = do
  let filePath = pth ++ "/.user.cfg"
  content <- readFile filePath
  let allLines = lines content
  let f = getConfigLine allLines
  let onlyOnce' = read (f "onlyOnce") :: Bool
  let storageDirectory' = f "storageDirectory"
  let delayRetrieveRates' = read (f "delayRetrieveRates") :: GI.Int64
  let delayAnalyzeOrders' = read (f "delayAnalyzeOrders") :: GI.Int64
  let passwordPath' = pth ++ "/.password"
  let kAPIKeyPath' = pth ++ "/.kapikey"

  let allCurrenciesTokenized = f "listCurrenciesOfInterest"
  let listCurrencies = words allCurrenciesTokenized
  
  let cfg = Config pth passwordPath' kAPIKeyPath' onlyOnce' storageDirectory' delayRetrieveRates' delayAnalyzeOrders' "" listCurrencies

  putStrLn $ "working with config" ++ (show cfg)
  return cfg

getConfigLine :: Lines -> String -> String
getConfigLine ls paramName = removeParamName paramName selected
  where
    filtered = filter (predicate paramName) ls
    selected = head filtered

    predicate :: String -> String -> Bool
    predicate pname ss = ss `startsWith` (addEquals pname) 

    startsWith :: String -> String -> Bool
    startsWith l p = l' == p
      where
        l' = take (length p) l

    removeParamName :: String -> String -> String
    removeParamName pname l = drop (length $ addEquals pname) l

    addEquals :: String -> String
    addEquals ss = ss ++ " = "
      
