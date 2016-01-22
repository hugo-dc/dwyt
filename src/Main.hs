-- Download YT Videos
import System.Environment
import System.Process
import GHC.IO.Exception
import Control.Exception
import System.IO
import System.Directory
import Data.List
import System.Exit
import System.Random
import System.Info (os)
--import System.FilePath.Windows (pathSeparator)


-- | Main Program
main :: IO ()
main = do
  args   <- getArgs
  case args of
    []     -> tryDownload
    (x:[]) -> sched x 
    _      -> putStrLn "Usage:\n\tdwyt <url>"

-- | Get path separator depending on OS
getSeparator :: Char
getSeparator | os == "windows" = '\\'
             | otherwise       = '/'
  
-- | Download list file
dwFILE :: IO String
dwFILE = do
  home <- getHomeDirectory
--  putStrLn home
  return $ home ++ [getSeparator] ++ "dwyt.lst"

-- | Get First Parameter
getFirst :: [String] -> String 
getFirst [] = "" 
getFirst (x:_) = x

-- | Get Random URL from Download List
getRandom :: [String] -> IO String
getRandom [] = return ""
getRandom xs = do
  g <- newStdGen
  let (r,_) = randomR(1, length xs) g 
  let s     = xs !! ( r - 1 )
  return s

-- | Print output and wait for user input
printOutput :: String -> IO ()
printOutput s = do
  putStrLn s
  _ <- getLine
  putStrLn ""

-- | Check if file exists
-- | if not, creates empty file
testFile :: String -> IO ()
testFile fname = do
  fex <- doesFileExist fname
  if not fex then
    appendFile fname ""
  else return ()
  
-- | Check if URL has been downloaded
-- | Or is already schedulled
-- | If not, save url to be downloaded
sched :: String -> IO ()
sched x  = do
  dwf <- dwFILE
  testFile (dwf ++ ".dw")
  downloaded <- readFile $ dwf ++ ".dw"

  if null downloaded then
    appendSched x
  else do
    schedulled <- readFile dwf
    if x `elem` ( lines downloaded) || 
       x `elem` ( lines schedulled) then
        printOutput "URL already downloaded or schedulled!"
    else
        appendSched x

-- | Try to download a random URL
tryDownload :: IO ()
tryDownload = do
  dwf <- dwFILE
  testFile dwf
  dwlist <- readFile dwf
  itm <- getRandom $ lines dwlist

  if null dwlist then
      emptyList 
  else
      putStrLn $ "Url: " ++ itm
  putStrLn "Downloading..."

  let lst = lines dwlist
      pr = readProcessWithExitCode "youtube-dl" [itm] []

  result <- pr

  let ec = (\(x, _, _) -> x ) result
      rs = (\(_, y, z) -> y ++ z ) result

  if ec /= ExitSuccess then 
    exitProgram ec rs itm lst
  else 
    printOutput rs
  print itm
  removeUrl itm lst
  appendDW itm

-- | Print error in command execution and exit program immediately
exitProgram :: ExitCode -> String -> String -> [String] -> IO ()
exitProgram e r u l = do
  print e
  printOutput r
  appendFail u
  removeUrl u l
  exitFailure

-- | Inform that Download list is empty and stop the program
emptyList :: IO ()
emptyList = do 
    printOutput "Download list is empty!"
    exitFailure

-- | Remove Url from download list
removeUrl :: String -> [String]-> IO ()
removeUrl url urls = do
  let new  = delete url urls
  bracketOnError (openTempFile "." "temp")
    (\(tempName, tempHandle) -> do 
       hClose tempHandle 
       removeFile tempName)
    (\(tempName, tempHandle) -> do 
      hPutStr tempHandle $ unlines new
      hClose tempHandle
      dwf <- dwFILE
      removeFile dwf
      renameFile tempName dwf)
      
-- | Add new URL in download list
appendSched :: String -> IO () 
appendSched url = do
  dwf <- dwFILE
  appendFile dwf ( url ++ "\n" )
  printOutput $ "Url " ++ url ++ " added!"   

-- | Add new URL to the already downloaded videos list
appendDW :: String -> IO ()
appendDW dw = do
  dwf <- dwFILE
  testFile dwf
  appendToFile ( dwf ++ ".dw" ) dw

-- | Save URL in the failed downloads list
appendFail :: String -> IO ()
appendFail fail = do
  dwf <- dwFILE
  appendToFile (dwf ++ ".fail") (fail++ "\n") 

-- | Save data into file
appendToFile :: String -> String -> IO ()
appendToFile file url = appendFile file ( url ++ "\n" ) 
