module Graphics.HTerra.Image
(
    -- * Data Types
    N.Seed
,   Image
,   Backend
,   Width
,   Height
    -- * Image creation
,   image
,   image'
)
where

import Graphics.HTerra.Noise as N

import Data.Array.Accelerate as A

type Image a = Array DIM2 a
type Backend a = Acc a -> a
type Width = Int
type Height = Int

-- | Create an image from a noise function.
image :: (IsNum a, Elt a, Elt b)
      => Backend (Image b) -> Noise (Point2 a) b -> Width -> Height -> Image b
image run noise w h = run $ image' noise w h

image' :: (IsNum a, Elt a, Elt b) => Noise (Point2 a) b -> Width -> Height -> Acc (Image b)
image' noise w h = A.generate (constant (Z:.w:.h)) $
       \ix -> let (Z:.x:.y) = unlift ix
              in noise . lift $ (A.fromIntegral x, A.fromIntegral y)
