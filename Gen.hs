module Gen where
import FFI

main :: IO ()
main = writeFile "Dlsym.hs" $ genModule 6

