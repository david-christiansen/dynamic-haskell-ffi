{-# LANGUAGE GADTs, KindSignatures, RankNTypes #-}

module FFI where
import Foreign.C
import Foreign.Ptr
import System.IO.Unsafe
import Data.List

data Exists c = forall a. Exists (c a)

instance Show (Exists c) where
  show x = "Exists <existential>"

data FBase :: * -> * where
  FInt :: FBase Int
  FFloat :: FBase Float
  FDouble :: FBase Double

instance Show (FBase a) where
  show FInt = "FInt"
  show FFloat = "FFloat"
  show FDouble = "FDouble"

data FSig :: * -> * where
  FReturn :: FBase a -> FSig a
  FArg :: FBase a -> FSig b -> FSig (a -> b)

instance Show (FSig a) where
  show (FReturn r) = "FReturn " ++ show r
  show (FArg a r) = "FArg (" ++ show a ++ ") (" ++ show r ++ ")"

data FFun a = FFun String (FSig a) deriving Show

type SOHandle = Ptr ()

foreign import ccall unsafe "dlfcn.h dlopen"
  c_dlopen :: CString -> SOHandle

foreign import ccall unsafe "dlfcn.h dlsym"
  c_dlsym :: SOHandle -> CString -> FunPtr (CDouble -> CDouble)

foreign import ccall "dynamic"
  mkFn :: FunPtr (CDouble -> CDouble) -> Double -> Double

mkString :: String -> CString
mkString str = unsafePerformIO $ newCString str

dlopen :: String -> SOHandle
dlopen lib = c_dlopen (mkString lib)

--dlsym :: SOHandle -> String -> FSig a -> a
--dlsym handle name (FArg FDouble (FReturn FDouble)) = mkFn (c_dlsym handle (mkString name)) . realToFrac
--dlsym _ _ s = error $ "Not implemented for " ++ show s

libc = dlopen "/lib/x86_64-linux-gnu/libc.so.6"


-- Code generation
toC :: FBase a -> String
toC FInt = "CInt"
toC FFloat = "CFloat"
toC FDouble = "CDouble"

toH :: FBase a -> String
toH FInt = "Int"
toH FFloat = "Float"
toH FDouble = "Double"

toCSig :: FSig a -> String
toCSig (FReturn r) = toC r
toCSig (FArg a r) = toC a ++ " -> " ++ toCSig r

toHSig :: FSig a -> String
toHSig (FReturn r) = toH r
toHSig (FArg a r) = toH a ++ " -> " ++ toHSig r

foreignImportName :: FSig a -> String
foreignImportName sig = "mk" ++ name' sig ++ "Fn"
    where name' :: FSig a -> String
          name' (FArg a r) = show a ++ name' r
          name' (FReturn r) = show r

genForeignDlsym :: FSig a -> String
genForeignDlsym sig = header ++ "\n  " ++ name ++ " :: " ++ typ
    where header = "foreign import ccall unsafe \"dlfcn.h dlsym\""
          name = "c_" ++ foreignImportName sig
          typ = "SOHandle -> CString -> FunPtr (" ++ toCSig sig ++ ")"

genForeignImport :: FSig a -> String
genForeignImport sig = header ++ "  " ++ name ++ " :: " ++ typ
    where header = "foreign import ccall \"dynamic\"\n"
          name = foreignImportName sig
          typ = "FunPtr (" ++ toCSig sig ++ ") -> (" ++ toHSig sig ++ ")"

genCase :: FSig a -> String
genCase sig =  bindings ++ " = " ++ foreignImportName sig ++ " " ++ getCFn -- ++ " . " ++ conv sig
    where bindings = "dlsym handle name (" ++ show sig ++ ")"
          getCFn = "(c_" ++ foreignImportName sig ++ " handle (mkString name))"
          conv :: FSig a -> String
          conv (FArg _ r) = conv r
          conv (FReturn FInt) = "fromIntegral"
          conv (FReturn FFloat) = "realToFrac"
          conv (FReturn FDouble) = "realToFrac"

-- Existential type necessary to work around parametricity / GADT
data SigList = Nil | forall a. Cons (FSig a) SigList

instance Show SigList where
  show Nil = "[]"
  show xs = "[" ++ show' xs
      where show' (Cons sig Nil) = show sig ++ "]"
            show' (Cons sig rest) = show sig ++ ", " ++ show' rest

addArg :: FBase a -> SigList -> SigList
addArg _ Nil = Nil
addArg b (Cons sig rest) = Cons (FArg b sig) (addArg b rest)

concatSL :: SigList -> SigList -> SigList
concatSL Nil sl = sl
concatSL (Cons s r) sl = Cons s $ concatSL r sl

mapSL :: (forall a. FSig a -> b) -> SigList -> [b]
mapSL _ Nil = []
mapSL f (Cons s r) = f s : mapSL f r

-- Return exhaustive list of up-to-n-arg sigs.
enumSigs :: Int -> SigList
enumSigs n = foldl concatSL (enumSigs' 1) $ map enumSigs' [2 .. n]
    where enumSigs' 0 = Cons (FReturn FInt) $
                        Cons (FReturn FFloat) $
                        Cons (FReturn FDouble) $
                        Nil
          enumSigs' n = addArg FInt next `concatSL`
                        addArg FDouble next `concatSL`
                        addArg FFloat next
                 where next = enumSigs' (n - 1)

-- Code generation

genImports n = (intercalate "\n\n" $ mapSL genForeignImport $ enumSigs n) ++
               "------------------\n\n" ++
               (intercalate "\n\n" $ mapSL genForeignDlsym $ enumSigs n)

genCases n = intercalate "\n" $ mapSL genCase $ enumSigs n
genDlsym n = header ++ "\n" ++ genCases n ++ "\n" ++ fail
    where header = "dlsym :: SOHandle -> String -> FSig a -> a"
          fail = "dlsym _ _ s = error $ \"Not implemented for \" ++ show s"
genModule n = header ++ "\n\n" ++ genImports n ++ "\n-----------\n" ++ genDlsym n
    where header = "{-# LANGUAGE GADTs #-}\nmodule Dlsym where\nimport FFI\nimport Foreign.C\nimport Foreign.Ptr\n"