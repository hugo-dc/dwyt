-- Youtube Downloader scheduller
import System.Environment
import System.Process
import GHC.IO.Exception
import Control.Exception
import System.IO
import System.Directory
import Data.List
import System.Exit

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

printOutput :: String -> IO ()
printOutput s = do 
                putStrLn s
                getLine
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
    putStrLn "Trying to download..."
    -- getDownload File
    dwlist <- readFile dwFILE

    putStrLn $ getFirst $ lines dwlist
    -- getFirst item in list
    let lst = lines dwlist
        itm = getFirst lst 
        pr = readProcessWithExitCode "youtube-dl" [itm] []

    result <- pr

    let ec = (\(x, _, _) -> x ) result
        rs = (\(_, y, z) -> y ++ z ) result

    if ec /= ExitSuccess then 
      exitProgram ec rs itm lst
    else 
      printOutput rs

    removeUrl itm lst
    appendDW itm

exitProgram :: ExitCode -> String -> String -> [String] -> IO ()
exitProgram e r u l = do
    putStrLn $ show e
--    putStrLn r
    printOutput r
    appendFail u
    removeUrl u l
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
appendDW url = appendToFile ( dwFILE ++ ".dw" ) url 

appendFail :: String -> IO ()
appendFail url = appendToFile (dwFILE ++ ".fail") url

appendToFile :: String -> String -> IO ()
appendToFile file url = appendFile file ( url ++ "\n" ) 


