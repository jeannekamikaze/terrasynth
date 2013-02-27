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
    -- * Input and output
,   writePGM
,   writePGM'
)
where

import Graphics.HTerra.Noise as N

import Data.Array.Accelerate as A
import Data.ByteString.Char8 as B
import Data.Char (chr)
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

-- | Save the image as PGM.
writePGM :: FilePath -> Image Float -> IO ()
writePGM file img =
         let Z:.w:.h = arrayShape img
             img' = pack . P.map toChar . toList $ img
         in withBinaryFile file WriteMode $ writePGM' w h img'

-- | Save the image as PGM.
writePGM' :: Int -> Int -> ByteString -> Handle -> IO ()
writePGM' w h bits hnd =
          let header = headerPGM w h
          in hPut hnd header >> hPut hnd bits

headerPGM :: Int -> Int -> ByteString
headerPGM w h = pack $ "P5 " ++ show w ++ " " ++ show h ++ " 255" ++ "\n"

toChar :: Float -> Char
toChar = chr . P.floor . (*255)
