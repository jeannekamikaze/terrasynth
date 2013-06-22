<<<<<<< HEAD
{-# LANGUAGE TypeOperators #-}
=======
>>>>>>> dev
module Graphics.HTerra.Noise
(
    -- * Data types
    Point2
,   Noise
,   Perms
,   Gradients
,   CellSize
,   Hurst
,   Lacunarity
,   Octaves
,   Seed
    -- * Noise functions
    -- ** Perlin noise
,   PerlinParams
,   perlinParams
,   perms
,   grads
,   perlin
    -- ** Fractional Brownian Motion
,   FbmParams
,   fBmParams
,   fBm
,   FbmParams'
,   fBmParams'
,   fBm'
)
where

import Graphics.HTerra.Interp

import Data.Array.Accelerate as A
import Data.Bits ((.&.))
import Data.List (foldl1')
import System.Random
import Prelude as P

type Point2 a = (a,a)
type Point3 a = (a,a,a)
type Noise a b = Exp a -> Exp b

type Perms a = Vector a
type Gradients a = Vector (Point2 a)

<<<<<<< HEAD
type CellSize = Float
type Hurst = Float
type Lacunarity = Float
=======
type CellSize a = a
type Hurst a = a
type Lacunarity a = a
>>>>>>> dev
type Octaves = Int

-- | A random number seed.
type Seed = Int

-- | Create a permutation table of N values.
perms :: (Integral a, Random a, Elt a) => Seed -> Int -> Perms a
perms seed n = A.fromList (Z:.n) . fmap P.fromIntegral . P.take n $
      randomRs (0,n-1) (mkStdGen seed)

-- | Create a table of N gradient vectors along the unit circle.
grads :: (Floating a, Elt a) => Int -> Gradients a
grads n = A.fromList (Z:.n) $ grads' n
      where grads' i =
                   let step = 2*pi / P.fromIntegral i
                       grads' a = (cos a, sin a) : grads' (a+step)
                   in P.take i $ grads' 0

-- 1D permutation table index function.
perm :: (Elt a, IsIntegral b, Elt b) => Acc (Perms a) -> Exp b -> Exp a
perm perms x = perms ! index1 (A.fromIntegral x .&. 255)

-- 2D permutation table index function.
index :: (IsIntegral a, Elt a) => Acc (Perms a) -> Exp (Point2 Int) -> Exp a
index ps p = perm' (x + perm' y)
      where perm' = perm ps
            (x,y) = unlift . num $ p

-- Index the gradients vector.
grad :: (Elt a, IsIntegral b, Elt b) => Acc (Gradients a) -> Exp b -> Exp (Point2 a)
grad grads i = grads ! index1 (A.fromIntegral i)

-- | Perlin noise parameters.
type PerlinParams a = (Perms Word8, Gradients a, Vector a)

perlinParams :: Elt a => Perms Word8 -> Gradients a -> CellSize a -> PerlinParams a
perlinParams ps gs cs = (ps, gs, A.fromList (Z:.1) [cs])

-- | Perlin noise function.
perlin :: (IsFloating a, Elt a) => Smooth a -> Acc (PerlinParams a) -> Noise (Point2 a) a
perlin smooth accParams p' =
       let (perms, grads, params) = unlift accParams
           cs  = params ! index1 0
           p   = scale (1 / cs) p'
           -- Compute gradients
           p0  = floor' p
           p1  = p0 `plus` constant (1,0)
           p2  = p0 `plus` constant (0,1)
           p3  = p0 `plus` constant (1,1)
           idx = index perms
           g0  = grad grads $ idx p0
           g1  = grad grads $ idx p1
           g2  = grad grads $ idx p2
           g3  = grad grads $ idx p3
           -- Compute weights
           s   = g0 `dot` (p `minus` num p0)
           t   = g1 `dot` (p `minus` num p1)
           u   = g2 `dot` (p `minus` num p2)
           v   = g3 `dot` (p `minus` num p3)
           -- Interpolate values
           (x,y)   = unlift p
           (x0,y0) = unlift (num p0)
           sx  = smooth (x-x0)
           sy  = smooth (y-y0)
           a   = lerp sx s t
           b   = lerp sx u v
           c   = lerp sy a b
       in  c*0.5 + 0.5

-- | fBm parameters.
type FbmParams c a = (c, Vector a)

fBmParams :: Elt a => c -> Hurst a -> Lacunarity a -> FbmParams c a
fBmParams c h l = (c, A.fromList (Z:.2) [h, l])

-- | fBm noise function.
fBm :: (IsFloating a, Elt a, Arrays c)
    => Octaves
    -> (Acc c -> Noise (Point2 a) a)
    -> Acc (FbmParams c a)
    -> Noise (Point2 a) a

fBm o noise' accParams p = iter o 1 1 0
    where (basisParams, params) = unlift accParams
          h  = params ! index1 0
          l  = params ! index1 1
          o' = constant o
          noise = noise' basisParams
          gain = l ** (-2*h)
          maxfBm = gain ==* 1 ? (1, geom gain (P.fromIntegral o))
          iter o f a val
               | o == 0 = val / maxfBm
               | otherwise =  iter (o-1) (l*f) (a*gain) $ (+val) . (*a) . noise . scale f $ p

-- | fBm' parameters.
type FbmParams' c a = (c, Vector a)

fBmParams' :: (IsFloating a, Elt a) => c -> Hurst a -> Lacunarity a -> Octaves -> FbmParams' c a
fBmParams' c h l o = (c, A.fromList (Z:.3) [h, l, P.fromIntegral o])

-- | fBm noise function.
fBm' :: (IsFloating a, Elt a, Arrays c)
     => (Acc c -> Noise (Point2 a) a) -- ^ The basis noise function
     -> Acc (FbmParams' c a)
     -> Noise (Point3 a) a

fBm' noise' accParams p = (/maxfBm) . (*a) . noise . scale f . lift $ (x,y)
     where (basisParams, params) = unlift accParams
           h = params ! index1 0
           l = params ! index1 1
           o = params ! index1 2
           (maxfBm, a, f, x, y) = iter h l o p
           noise = noise' basisParams
           iter :: (IsFloating a, Elt a)
                => Exp (Hurst a) -> Exp (Lacunarity a) -> Exp a -> Exp (a,a,a)
                -> (Exp a, Exp a, Exp a, Exp a, Exp a)
           iter h l o p = (maxfBm, a, f, x, y)
                where (x,y,z) = unlift p
                      f = l ** z
                      a = l ** (-2*h*z)
                      gain = l ** (-2*h)
                      maxfBm = gain ==* 1 ? (1, geom gain o)

geom r n = (1 - r**n) / (1-r)

floor' :: (IsFloating a, Elt a) => Exp (Point2 a) -> Exp (Point2 Int)
floor' p = lift (x',y')
       where (x,y) = unlift p
             x' = A.floor x
             y' = A.floor y

num :: (IsNum a, Elt a) => Exp (Point2 Int) -> Exp (Point2 a)
num p = lift (x',y')
      where (x,y) = unlift p :: (Exp Int, Exp Int)
            x' = A.fromIntegral x
            y' = A.fromIntegral y

plus :: (IsNum a, Elt a) => Exp (Point2 a) -> Exp (Point2 a) -> Exp (Point2 a)
plus = lift2 f
     where f :: (IsNum a, Elt a) => (Exp a, Exp a) -> (Exp a, Exp a) -> (Exp a, Exp a)
           f (ax,ay) (bx,by) = (ax+bx, ay+by)

minus :: (IsNum a, Elt a) => Exp (Point2 a) -> Exp (Point2 a) -> Exp (Point2 a)
minus = lift2 f
      where f :: (IsNum a, Elt a) => (Exp a, Exp a) -> (Exp a, Exp a) -> (Exp a, Exp a)
            f (ax,ay) (bx,by) = (ax-bx, ay-by)

dot :: (IsNum a, Elt a) => Exp (Point2 a) -> Exp (Point2 a) -> Exp a
dot = lift2 f
    where f :: (IsNum a, Elt a) => (Exp a, Exp a) -> (Exp a, Exp a) -> Exp a
          f (ax,ay) (bx,by) = ax*bx + ay*by

scale :: (IsNum a, Elt a) => Exp a -> Exp (Point2 a) -> Exp (Point2 a)
scale s p = lift (s*x, s*y) where (x,y) = (A.fst p, A.snd p)
