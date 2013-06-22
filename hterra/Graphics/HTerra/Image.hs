{-# LANGUAGE TypeOperators #-}
module Graphics.HTerra.Image
(
    -- * Data Types
    N.Seed
,   Image
,   Runner
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
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (chr)
import System.IO
import Prelude as P

type ByteString = B.ByteString

type Image a = Array DIM2 a
type Runner a b = (Acc a -> Acc b) -> a -> b
type Width = Int
type Height = Int

-- | Create an image from a noise function.
image :: (Arrays c, IsNum a, Elt a, Elt b)
      => Runner c (Image b)
      -> (Acc c -> Noise (Point2 a) b) -> Width -> Height -> c -> Image b
image run1 noise w h = run1 $ image' noise w h

-- | Create an image from a noise function.
image' :: (Arrays c, IsNum a, Elt a, Elt b)
       => (Acc c -> Noise (Point2 a) b) -> Width -> Height -> Acc c -> Acc (Image b)
image' noise' w h params =
       let noise = noise' params
       in A.generate (constant (Z:.w:.h)) $
          \ix -> let (Z:.x:.y) = unlift ix
                 in noise . lift $ (A.fromIntegral x, A.fromIntegral y)

-- | Create an image using fractional Brownian motion.
fBmImage :: (IsFloating a, Elt a, Arrays c)
         => Runner (FbmParams c a) (Image a)
         -> (Acc c -> Noise (Point2 a) a)
         -> Octaves -> Width -> Height -> FbmParams c a -> Image a
fBmImage run1 noise o w h = run1 $ fBmImage' noise o w h

-- | Create an image using fractional Brownian motion.
fBmImage' :: (IsFloating a, Elt a, Arrays c)
          => (Acc c -> Noise (Point2 a) a)
          -> Octaves -> Width -> Height -> Acc (FbmParams' c a) -> Acc (Image a)
fBmImage' noise' o w h params =
          let noise = fBm' noise' params
          in A.fold1 (+) $ A.generate (constant (Z:.w:.h:.o)) $
             \ix -> let (Z:.x:.y:.z) = unlift ix :: (Z :. Exp Int :. Exp Int :. Exp Int)
                    in noise . lift $ (A.fromIntegral x, A.fromIntegral y, A.fromIntegral z)
