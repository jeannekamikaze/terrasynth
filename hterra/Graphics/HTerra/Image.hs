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
,   perlinImage
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

import qualified Data.Array.Accelerate.Interpreter as I

type Image = Array DIM2 Float
type Matrix a = Array DIM2 a
type Backend = Acc Image -> Image
type Width = Int
type Height = Int
type CellSize = Float

-- | Create an image from a noise function.
noiseImage :: Backend -> Noise (Point2 Float) Float -> Matrix (Point2 Float) -> Image
noiseImage run noise mat = run $ noiseImage' noise mat

-- | Create an image from a noise function.
noiseImage' :: Noise (Point2 Float) Float -> Matrix (Point2 Float) -> Acc Image
noiseImage' noise mat = A.map noise . use $ mat

matrix :: CellSize -> Width -> Height -> Matrix (Point2 Float)
matrix cs w h = A.fromList (Z:.w:.h) $ [(y,x) | y <- [0,s..h'], x <- [0,s..w']]
       where w' = (P.fromIntegral w - 1) / cs
             h' = (P.fromIntegral h - 1) / cs
             s  = 1/cs

-- | Create an image based on Perlin noise.
perlinImage :: Backend -> Seed -> CellSize -> Width -> Height -> Image
perlinImage run seed cs w h = noiseImage run (perlin seed) $ matrix cs w h

-- | Save the image as PGM.
writePGM :: FilePath -> Image -> IO ()
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
headerPGM w h = pack . P.filter (/='"') $
          "P5 " ++ show w ++ " " ++ show h ++ " 255" ++ "\n"

toChar :: Float -> Char
toChar = chr . P.floor . (*255)

black :: Int -> Int -> Image
black w h = A.fromList (Z:.w:.h) (repeat 0)
