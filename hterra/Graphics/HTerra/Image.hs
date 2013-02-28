module Graphics.HTerra.Image
(
    -- * Data Types
    N.Seed
,   Image
,   Backend
,   Width
,   Height
,   CellSize
    -- * Image creation
,   image
,   pixels
)
where

import Graphics.HTerra.Noise as N

import Data.Array.Accelerate as A

type Image a = Array DIM2 a
type Backend a = Acc a -> a
type Width = Int
type Height = Int
type CellSize = Float

-- | Create an image from a noise function.
image :: (Elt a, Elt b) => Backend (Image b) -> Noise a b -> Acc (Image a) -> Image b
image run noise = run . A.map noise

-- | Create an image by evaluating a function at every pixel.
pixels
    :: (IsNum a, Elt a, Elt b)
    => Width -> Height
    -> (Exp a -> Exp a -> Exp b) -- ^ The function to apply to every point in the matrix.
    -> Acc (Image b)
pixels w h f = A.generate (constant (Z:.w:.h)) $
       \ix -> let (Z:.x:.y) = unlift ix
              in f (A.fromIntegral x) (A.fromIntegral y)
