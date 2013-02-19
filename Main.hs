{-# LANGUAGE GADTs #-}
module Main where

import FFI
import Parser
import Dlsym

import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO
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
processLine ln = case parseSig ln of
                   Right (file, name, Exists typ) ->
                       do putStrLn $ "in " ++ file ++ ", " ++ name ++ " : " ++ show typ
                          let handle = dlopen file
                          let f = dlsym handle name typ
                          putStrLn "got fn"
                          processArgs typ f
                   Left err -> putStrLn $ "Error: " ++ show err

getArg :: FSig (a -> b) -> IO a
getArg (FArg FInt _) = putStr "Integer> " >> hFlush stdout >> getLine >>= return . read
getArg (FArg FDouble _) = putStr "Double> " >> hFlush stdout >> getLine >>= return . read
getArg (FArg FFloat _) = putStr "Float> " >> hFlush stdout >> getLine >>= return . read

call :: FSig (a -> b) -> (a -> b) -> a -> (b, FSig b)
call (FArg FInt r) f x = (f x, r)
call (FArg FDouble r) f x = (f x, r)
call (FArg FFloat r) f x = (f x, r)

processArgs :: FSig a -> a -> IO ()
-- Pattern match needed to get type info from GADT
processArgs (FReturn FInt) x = putStrLn $ "Result: " ++ show x
processArgs (FReturn FFloat) x = putStrLn $ "Result: " ++ show x
processArgs (FReturn FDouble) x = putStrLn $ "Result: " ++ show x
-- Same for call
processArgs s@(FArg _ _) f =
    do a <- getArg s
       let (f', s') = call s f a
       processArgs s' f'