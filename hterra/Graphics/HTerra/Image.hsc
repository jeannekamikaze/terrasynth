{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.HTerra.Image
(
    -- * Data Types
    N.Seed
,   Image
,   Matrix
,   Backend
,   Width
,   Height
,   CellSize
    -- * Image creation
,   noiseImage
,   noiseImage'
,   matrix
    -- * Input and output
,   writePGM
)
where

import Graphics.HTerra.Noise as N

import Data.Array.Accelerate as A
import Data.Array.Accelerate.IO
import Data.ByteString.Char8 as B
import Data.Char (chr)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr)
import System.IO
import Prelude as P

type Image a = Array DIM2 a
type Matrix a = Array DIM2 a
type Backend a = Acc a -> a
type Width = Int
type Height = Int
type CellSize = Float

-- | Create an image from a noise function.
noiseImage :: (Elt a, Elt b) => Backend (Image b) -> Noise a b -> Matrix a -> Image b
noiseImage run noise mat = run $ noiseImage' noise mat

-- | Create an image from a noise function.
noiseImage' :: (Elt a, Elt b) => Noise a b -> Matrix a -> Acc (Image b)
noiseImage' noise mat = A.map noise . use $ mat

-- | Create a matrix used as the source of a noise image.
matrix :: (Num a, Enum a, Elt b)
       => (Point2 a -> Point2 b) -- ^ A function to apply to every point in the matrix.
       -> Width -> Height -> Matrix (Point2 b)
matrix f w h = A.fromList (Z:.w:.h) . P.map f $ [(y,x) | y <- [0..h'], x <- [0..w']]
       where w' = P.fromIntegral w
             h' = P.fromIntegral h

-- | Save the image as PGM.
writePGM :: FilePath -> Image Float -> IO ()
writePGM file img =
         let (Z:.w:.h) = arrayShape img
         in withCString file $ \cfile ->
            allocaArray (w*h) $ \p -> do
            toPtr img ((),p)
            write_pgm (P.fromIntegral w) (P.fromIntegral h) p cfile

foreign import ccall unsafe "write_pgm"
        write_pgm :: CInt -> CInt -> Ptr Float -> CString -> IO ()
