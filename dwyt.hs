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


-- | Main Program
main :: IO ()
main = do
    args   <- getArgs
    if length args  > 1 then 
        putStrLn "Usage:\n\tdwyt <url>"
    else
        check $ getFirst args

-- | Download list file
dwFILE :: String
dwFILE = "dwyt.lst"

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
                    s      = xs !! ( r - 1 )
                return s

-- | Print output and wait for user input
printOutput :: String -> IO ()
printOutput s = do 
                putStrLn s
                _ <- getLine
                putStrLn ""

-- | Check if URL has been downloaded
-- | Or is already schedulled
check :: String -> IO ()
check [] = tryDownload 
check x  = do 
    downloaded <- readFile $ dwFILE ++ ".dw"
    schedulled <- readFile dwFILE

    if x `elem` ( lines downloaded) || 
       x `elem` ( lines schedulled) then
        printOutput "URL already downloaded or schedulled!"
    else
        appendSched x

-- | Try to download a random URL
tryDownload :: IO ()
tryDownload = do 
    dwlist <- readFile dwFILE
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
      removeFile dwFILE
      renameFile tempName dwFILE)
      
-- | Add new URL in download list
appendSched :: String -> IO () 
appendSched url = do 
    appendFile dwFILE ( url ++ "\n" )
    printOutput $ "Url " ++ url ++ " added!"   

-- | Add new URL to the already downloaded videos list
appendDW :: String -> IO ()
appendDW = appendToFile ( dwFILE ++ ".dw" ) 

-- | Save URL in the failed downloads list
appendFail :: String -> IO ()
appendFail = appendToFile (dwFILE ++ ".fail") 

-- | Save data into file
appendToFile :: String -> String -> IO ()
appendToFile file url = appendFile file ( url ++ "\n" ) 


