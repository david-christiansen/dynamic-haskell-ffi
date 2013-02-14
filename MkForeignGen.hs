module MkForeignGen where

import Data.List
import FFITest

header = "module MkForeign where\nimport FFITest\n"

ftypes = [FInt, FFloat, FChar, FString, FUnit]

toC :: FTy -> String
toC FInt = "CInt"
toC FFloat = "CFloat"
toC FChar = "CChar"
toC FString = "CString"
toC FUnit = "CUnit"

toHs :: FTy -> String
toHs FInt = "Int"
toHs FFloat = "Float"
toHs FChar = "Char"
toHs FString = "String"
toHs FUnit = "()"

toCSig :: FFn -> String
toCSig (Sig args ret) = concatMap (\t -> toC t ++ " -> ") args ++ toC ret

toHsSig :: FFn -> String
toHsSig (Sig args ret) = concatMap (\t -> toHs t ++ " -> ") args ++ toHs ret

argList 0 = [[]]
argList n = do args <- argList (n - 1)
               ft <- ftypes
               return $ ft : args

prnArgList als = "[" ++ (concat $ intersperse ", " $ map show als) ++ "]"

sigs n = do args <- argList n
            ret <- ftypes
            return $ Sig args ret

convName :: FFn -> String
convName (Sig args ret) = "mk" ++ concatMap show args ++ show ret ++ "Fn"

mkFunConv :: FFn -> String
mkFunConv s@(Sig args ret) =
    "foreign import ccall \"dynamic\"\n" ++
    "  " ++ convName s ++ " :: FunPtr (" ++ toCSig s ++ ") -> (" ++ toHsSig s ++ ")"

mkConv :: [FFn] -> String
mkConv = typ ++ concatMap mkCaseConv
    where mkCaseConv s = "conv f (" ++ show s ++ ") = " ++ convName s ++ " f\n"
          typ = ""

