{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module MkForeignCls where

import Foreign.C
import Foreign.Ptr

class MkForeign a b | a -> b where
  conv :: a -> b

-- examples

foreign import ccall "dynamic"
  mkCIntCIntFn :: FunPtr (CInt -> CInt) -> (Int -> Int)

instance MkForeign (FunPtr (CInt -> CInt)) (Int -> Int) where
  conv = mkCIntCIntFn