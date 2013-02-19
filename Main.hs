module Main where

import FFI
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Control.Monad (guard)
import Debug.Trace

trim :: String -> String
trim = reverse . dropWhile (flip elem " \t") . reverse . dropWhile (flip elem " \t")

main :: IO ()
main = do ln <- getLine
          if ln == ":q"
            then do putStrLn "Bye!"
                    exitSuccess
            else do processLine ln
                    main

processLine :: String -> IO ()
processLine ln = do let (lib, rest) = break (flip elem " \t") $ trim ln
                    let (fn, arg)   = break (flip elem " \t") $ trim rest
                    let a  = (read $ trim arg) :: Double
                    putStrLn $ show (lib, fn, a)