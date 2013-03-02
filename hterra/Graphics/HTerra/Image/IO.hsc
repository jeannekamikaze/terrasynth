{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.HTerra.Image.IO where

import Graphics.HTerra.Image

import Data.Array.Accelerate
import Data.Array.Accelerate.Array.Sugar (Array(..))
import Data.Array.Accelerate.Array.Data (ptrsOfArrayData)
import Data.Array.Accelerate.IO
import Foreign.C.String (CString, withCString)
import Foreign.C.Types
import Foreign.Ptr (Ptr)
import Prelude as P

-- | Save the image as PGM.
writePGM :: FilePath -> Image Float -> IO ()
writePGM file img =
         let (Z:.w:.h) = arrayShape img
             (Array _ adata) = img
             ((),ptr) = ptrsOfArrayData adata
         in withCString file $ write_pgm (P.fromIntegral w) (P.fromIntegral h) ptr

foreign import ccall unsafe "write_pgm"
        write_pgm :: CInt -> CInt -> Ptr Float -> CString -> IO ()
