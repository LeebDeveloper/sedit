module Main
    ( main
    ) where


import System.Environment
import Data.List

data InputArgs = InputArgs
    {   
        prevVsn :: FilePath,
        currVsn :: FilePath,
        diff    :: Maybe FilePath
    } deriving(Show)

--main :: IO ()
main = do
    a <- getArgs
    case parseArgs a of
        Just ia -> putStrLn $ show ia
        Nothing -> putStrLn "Usage:\nsedit <previous version> <current version> [<output diff>]"
    

parseArgs :: [String] -> Maybe InputArgs
parseArgs (prev:curr:xs) = Just InputArgs
    {
        prevVsn = prev,
        currVsn = curr,
        diff    = head' xs
    }
parseArgs _ = Nothing


head' :: [a] -> Maybe a
head' []     = Nothing
head' (x:xs) = Just x
