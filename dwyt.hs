-- Youtube Downloader scheduller
import System.Environment
import System.Process
import GHC.IO.Exception
import Control.Exception
import System.IO
import System.Directory
import Data.List
import System.Exit
import System.Random

dwFILE :: String
dwFILE = "dwyt.lst"

main :: IO ()
main = do
    args   <- getArgs
    if length args  > 1 then 
        putStrLn "Usage:\n\tdwyt <url>"
    else
        check $ getFirst args


getFirst :: [String] -> String 
getFirst [] = "" 
getFirst (x:_) = x

getRandom :: [String] -> IO String
getRandom [] = return ""
getRandom xs = do 
                g <- newStdGen
                let (r,_) = randomR(1, length xs) g 
                    s      = xs !! ( r + 1 )
                return s


printOutput :: String -> IO ()
printOutput s = do 
                putStrLn s
                _ <- getLine
                putStrLn ""

check :: String -> IO ()
check [] = tryDownload 
check x  = do 
    downloaded <- readFile $ dwFILE ++ ".dw"
    schedulled <- readFile dwFILE
    let dwn = checkDownloaded x $ lines downloaded
        sch = checkSchedulled x $ lines schedulled
    if dwn || sch then 
        printOutput "URL already downloaded or schedulled!"
    else
        appendSched x

checkDownloaded :: String -> [String] -> Bool
checkDownloaded x y = x `elem` y 

checkSchedulled :: String -> [String] -> Bool
checkSchedulled x y = x `elem` y

tryDownload :: IO ()
tryDownload = do 
    dwlist <- readFile dwFILE
    itm <- getRandom $ lines dwlist

    if null dwlist then
        emptyList 
    else
        --putStrLn $ "Url: " ++ ( getFirst $ lines dwlist )
        putStrLn $ "Url: " ++ itm
    putStrLn "Downloading..."

    -- getFirst item in list
    let lst = lines dwlist
        pr = readProcessWithExitCode "youtube-dl" [itm] []

    result <- pr

    let ec = (\(x, _, _) -> x ) result
        rs = (\(_, y, z) -> y ++ z ) result

    if ec /= ExitSuccess then 
      exitProgram ec rs itm lst
    else 
      printOutput rs

    --putStrLn $ show itm 
    print itm

    removeUrl itm lst
    appendDW itm

exitProgram :: ExitCode -> String -> String -> [String] -> IO ()
exitProgram e r u l = do
    --putStrLn $ show e
    print e
--    putStrLn r
    printOutput r
    appendFail u
    removeUrl u l
    exitFailure

emptyList :: IO ()
emptyList = do 
    printOutput "Download list is empty!"
    exitFailure

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
      
      

appendSched :: String -> IO () 
appendSched url = do 
    appendFile dwFILE ( url ++ "\n" )
    printOutput $ "Url " ++ url ++ " added!"   


appendDW :: String -> IO ()
--appendDW url = appendToFile ( dwFILE ++ ".dw" ) url 
appendDW = appendToFile ( dwFILE ++ ".dw" ) 

appendFail :: String -> IO ()
--appendFail url = appendToFile (dwFILE ++ ".fail") url
appendFail = appendToFile (dwFILE ++ ".fail") 

appendToFile :: String -> String -> IO ()
appendToFile file url = appendFile file ( url ++ "\n" ) 


