{-# LANGUAGE RankNTypes, GADTs #-}
module Parser where

import FFI

import Text.Parsec
import Text.Parsec.String
import Control.Monad.Identity

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

typ :: Parser (Exists FBase)
typ = int <|> double <|> float
    where int = string "int" >> return (Exists FInt)
          double = string "double" >> return (Exists FDouble)
          float = string "float" >> return (Exists FFloat)

signature :: Parser (FilePath, String, Exists FSig)
signature = do from <- filename
               many1 space
               ret <- typ
               many1 space
               name <- name
               many space
               args <- inParens $ sepBy1 typ $ many space >> char ',' >> many space
               return (from, name, mkSig ret args)
    where mkSig :: Exists FBase -> [Exists FBase] -> Exists FSig
          mkSig (Exists r) [] = Exists $ FReturn r
          mkSig (Exists r) ((Exists a):as) = case (mkSig (Exists r) as) of
                                               Exists rest -> Exists $ FArg a rest


parseSig :: String -> Either ParseError (FilePath, String, Exists FSig)
parseSig = parse signature "<user input>"
