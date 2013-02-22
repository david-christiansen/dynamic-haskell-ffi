module Main where

import Foreign.LibFFI
import Foreign.Ptr (nullPtr)
import System.Posix.DynamicLinker

import System.Posix.DynamicLinker.Prim (DL)
import System.IO
import System.Exit (exitSuccess)

import Text.Parsec
import Text.Parsec.String
import Control.Monad.Identity
import Data.Maybe
import qualified Data.Map as M
import Control.Monad.State.Strict
import qualified Control.Exception as E
import System.Console.Haskeline

------------------------------------
-- Basic types and data structures
------------------------------------

data FTy = FInt | FDouble | FFloat deriving Show

data FArg = AInt Int | ADouble Double | AFloat Float deriving Show

matches :: FTy -> FArg -> Bool
matches FInt (AInt _) = True
matches FDouble (ADouble _) = True
matches FFloat (AFloat _) = True
matches _ _ = False

data FSig = FSig [FTy] FTy deriving Show

data Command = Load FilePath | Declare String FSig | Call String [FArg] | Quit deriving Show


----------------
-- Parser
----------------
name :: Parser String
name = many1 letter <?> "identifier"

inParens :: Parser a -> Parser a
inParens p = do char '('
                many space
                res <- p
                many space
                char ')'
                return res

filename :: Parser FilePath
filename = do char '/'
              rest <- many $ noneOf " \t\n\r"
              return ('/':rest)

typ :: Parser FTy
typ = int <|> double <|> float
    where int = string "int" >> return FInt
          double = string "double" >> return FDouble
          float = string "float" >> return FFloat

orEmpty :: Maybe String -> String
orEmpty = fromMaybe ""

orZero  :: Maybe String -> String
orZero = fromMaybe "0"

int :: Parser FArg
int = do sign <- optionMaybe $ string "-"
         digits <- many1 digit
         return . AInt . read $ orEmpty sign ++ digits

float :: Parser FArg
float = do sign <- optionMaybe $ string "-"
           wholePart <- many1 digit
           decPart <- optionMaybe $ char '.' >> many1 digit
           return . AFloat . read $ orEmpty sign ++ wholePart ++ "." ++ fromMaybe "0" decPart

double :: Parser FArg
double = do sign <- optionMaybe $ string "-"
            wholePart <- many1 digit
            decPart <- optionMaybe $ char '.' >> many1 digit
            return . ADouble . read $ orEmpty sign ++ wholePart ++ "." ++ fromMaybe "0" decPart

arg :: FTy -> Parser FArg
arg FInt    = int
arg FDouble = double
arg FFloat  = float

argList :: [FTy] -> Parser [FArg]
argList args = do char '(' >> argList' args
    where argList' :: [FTy] -> Parser [FArg]
          argList' [] = char ')' >> return []
          argList' [ty] = do a <- arg ty
                             char ')'
                             return [a]
          argList' (ty:tys) = do a <- arg ty
                                 char ','
                                 many space
                                 as <- argList' tys
                                 return $ a:as
          singleton x = [x]


command :: ReplState -> Parser Command
command st = try load <|> try declare <|> try quit <|> call
    where load = do string ":o"
                    many1 space
                    f <- filename
                    return $ Load f
          declare = do ret <- typ
                       many1 space
                       name <- name
                       many space
                       args <- inParens $ sepBy1 typ $ many space >> char ',' >> many space
                       return $ Declare name $ FSig args ret
          quit = string ":q" >> return Quit
          call = do fn <- choice (map string defined) -- only allow defined names
                    let argTs = fromMaybe [] $ fmap argTypes $ M.lookup fn $ funcs st
                    args <- argList argTs
                    return $ Call fn args
          defined = M.keys $ funcs st
          argTypes (FSig args _) = args



---------
-- REPL
---------

data ReplState = ReplState { lib :: Maybe DL
                           , funcs :: M.Map String FSig
                           }

type Repl = InputT (StateT ReplState IO)

getLib :: Repl (Maybe DL)
getLib = fmap lib (lift get)

setLib :: DL -> Repl ()
setLib l = lift $ put $ ReplState (Just l) (M.fromList [])

getFunc :: String -> Repl (Maybe FSig)
getFunc name = fmap (\s -> M.lookup name (funcs s)) (lift get)

setFunc :: String -> FSig -> Repl ()
setFunc name sig = do s <- lift get
                      let names = funcs s
                      lift $ put $ s { funcs = M.insert name sig names }

msg :: String -> Repl ()
msg = lift . lift . putStrLn

repl :: Repl ()
repl = do line <- getInputLine "> "
          continue <- handleLine $ fromMaybe "" line
          if continue
            then repl
            else msg "Bye!"

load :: FilePath -> Repl ()
load filename = do handle <- lift . lift $
                             Prelude.catch (fmap Right $ dlopen filename [RTLD_NOW])
                                  (\e -> return $ Left $ show e)
                   case handle of
                     Left err -> msg err
                     Right h | undl h == nullPtr -> msg $ "Got null opening " ++ filename
                             | otherwise         -> do msg $ "Loading " ++ filename
                                                       setLib h

call :: String -> [FArg] -> Repl ()
call fn args = do handle <- getLib
                  sig <- getFunc fn
                  case sig of
                    Just (FSig argTs ret) ->
                        do cfn <- lift . lift $ dlsym (fromMaybe Default handle) fn
                           -- TODO typecheck args
                           res <- lift . lift $ callFn ret cfn args
                           msg . show $ res
                    Nothing ->
                        msg $ "Function " ++ fn ++ " not found"
    where callFn FDouble cfn args = fmap (ADouble . realToFrac) $
                                    callFFI cfn retCDouble (prepArgs args)
          callFn FFloat cfn args = fmap (AFloat . realToFrac) $
                                   callFFI cfn retCFloat (prepArgs args)
          callFn FInt cfn args = fmap AInt $
                                 callFFI cfn retInt (prepArgs args)
          prepArgs = map prepArg
          prepArg (ADouble d) = argCDouble $ realToFrac d
          prepArg (AFloat f)  = argCFloat $ realToFrac f
          prepArg (AInt i)    = argCInt $ fromIntegral i

handleLine :: String -> Repl Bool
handleLine ln = do st <- lift get
                   case parse (command st) "<user input>" ln of
                     Left err -> do msg $ show err
                                    return True
                     Right Quit -> return False
                     Right (Load filename) -> load filename >> return True
                     Right (Declare name sig) -> do setFunc name sig
                                                    msg $ "Declared " ++ name
                                                    return True
                     Right (Call name args) -> call name args >> return True

main :: IO ()
main = evalStateT (runInputT defaultSettings repl) (ReplState Nothing $ M.fromList [])