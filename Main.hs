module Main where

import FFITest
import Debug.Trace

trim :: String -> String
trim = reverse . dropWhile (flip elem " \t") . reverse . dropWhile (flip elem " \t")

main :: IO ()
main = do ln <- getLine
          let (lib, rest) = break (flip elem " \t") $ trim ln
          let (fn, arg)   = break (flip elem " \t") $ trim rest
          let a  = (read $ trim arg) :: Double
          let res = call lib fn a
          putStrLn $ show res
          main

call :: String -> String -> Double -> Double
call lib fn arg = fn' arg
    where lib' = dlopen lib
          fn'  = dlsym lib' fn