{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, ForeignFunctionInterface #-}
module FFITest where
import Foreign.C
import Foreign.Ptr
import System.IO.Unsafe

-- Copy Idris' ffi types, as that's what I want to use later

data FTy = FInt | FFloat | FChar | FString {- | FPtr -} | FUnit
           deriving Show

data FFn = Sig [FTy] FTy
           deriving Show

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

dlsym :: SOHandle -> String -> Double -> Double
dlsym handle name = mkFn(c_dlsym handle (mkString name)) . realToFrac


libc = dlopen "/lib/x86_64-linux-gnu/libc.so.6"

