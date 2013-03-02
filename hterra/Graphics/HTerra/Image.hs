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
,   fBmImage
,   fBmImage'
)
where

import Graphics.HTerra.Noise as N

import Data.Array.Accelerate as A
import Prelude as P

type Image a = Array DIM2 a
type Backend a = Acc a -> a
type Width = Int
type Height = Int

-- | Create an image from a noise function.
image :: (IsNum a, Elt a, Elt b)
      => Backend (Image b) -> Noise (Point2 a) b -> Width -> Height -> Image b
image run noise w h = run $ image' noise w h

-- | Create an image from a noise function.
image' :: (IsNum a, Elt a, Elt b) => Noise (Point2 a) b -> Width -> Height -> Acc (Image b)
image' noise w h = A.generate (constant (Z:.w:.h)) $
       \ix -> let (Z:.x:.y) = unlift ix
              in noise . lift $ (A.fromIntegral x, A.fromIntegral y)

fBmImage :: (IsFloating a, Elt a)
         => Backend (Image a) -> Noise (Point2 a) a -> H a -> Lacunarity a -> Octaves
         -> Width -> Height -> Image a
fBmImage run noise hu l o w h = run $ fBmImage' noise hu l o w h

fBmImage' :: (IsFloating a, Elt a)
          => Noise (Point2 a) a -> H a -> Lacunarity a -> Octaves
          -> Width -> Height -> Acc (Image a)
fBmImage' noise hu l o w h = A.map (/maxfBm) $ fBm' o 1 1 black
          where black = A.generate (constant (Z:.w:.h)) $ \_ -> 0
                maxfBm = constant $ geom gain (P.fromIntegral o)
                gain = l ** (-2*hu)
                fBm' 0 _ _ img = img
                fBm' i a f img = img' `seq` fBm' (i-1) (a*gain) (f*l) img'
                     where img' = next `seq` A.zipWith (+) img next
                           next = image' ((*amp) . noise . toOctave) w h
                           toOctave p = scale (constant f) p
                           amp = constant a

geom r n = (1 - r**n) / (1-r)

scale :: (IsNum a, Elt a) => Exp a -> Exp (Point2 a) -> Exp (Point2 a)
scale s p = lift (s*x, s*y) where (x,y) = (A.fst p, A.snd p)

cells' :: (IsNum a, IsNum b, Elt a, Elt b)
       => Noise (Point2 a) b -> Octaves
       -> Width -> Height -> Acc (Image b)
cells' noise o w h = fBm' o black
       where fBm' 0 img = img
             fBm' i img = fBm' (i-1) $ A.zipWith (+) img (image' noise w h)
             black = A.generate (constant (Z:.w:.h)) $ \_ -> 0
