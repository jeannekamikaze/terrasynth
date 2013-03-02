{-# LANGUAGE TypeOperators #-}
module Graphics.HTerra.Noise
(
    -- * Data types
    Point2
,   Noise
,   Perms
,   Gradients
,   CellSize
,   H
,   Lacunarity
,   Octaves
,   Seed
    -- * Noise functions
,   perlin
,   perlin'
,   fBm
)
where

import Graphics.HTerra.Interp

import Data.Array.Accelerate as A
import Data.Bits ((.&.))
import Data.List (foldl1')
import System.Random
import Prelude as P

type Point2 a = (a,a)
type Noise a b = Exp a -> Exp b

type Perms a = Acc (Vector a)
type Gradients a = Acc (Vector (Point2 a))

type CellSize = Float
type H a = a
type Lacunarity a = a
type Octaves = Int

-- | A random number seed.
type Seed = Int

-- Create a permutation table of N values.
perms :: (Num a, Eq a, Integral a, Random a) => Seed -> Int -> [a]
perms seed n = fmap P.fromIntegral . P.take n $ randomRs (0,n-1) (mkStdGen seed)

-- Create a list of N gradient vectors along the unit circle.
grads :: Int -> [Point2 Float]
grads i =
      let step = 2*pi / (P.fromIntegral i)
          grads' a = (cos a, sin a) : grads' (a+step)
      in P.take i $ grads' 0

-- 1D permutation table index function.
perm :: (Elt a, IsIntegral b, Elt b) => Perms a -> Exp b -> Exp a
perm perms x = perms ! index1 (A.fromIntegral x .&. 255)

-- 2D permutation table index function.
index :: (IsIntegral a, Elt a) => Perms a -> Exp (Point2 Int) -> Exp a
index ps p = perm' (x' + perm' y')
      where perm' = perm ps
            (x,y) = unlift p :: (Exp Int, Exp Int)
            x' = A.fromIntegral x
            y' = A.fromIntegral y

-- Index the gradients vector.
grad :: (Elt a, IsIntegral b, Elt b) => Gradients a -> Exp b -> Exp (Point2 a)
grad grads i = grads ! index1 (A.fromIntegral i)

perlin :: Seed -> CellSize -> Noise (Point2 Float) Float
perlin seed cs =
       let perms' = use . A.fromList (Z:.256) $ perms seed 256
           grads' = use . A.fromList (Z:.256) $ grads 256
       in perlin' perms' grads' scurve cs

perlin' :: Perms Word8 -> Gradients Float -> Smooth Float -> CellSize -> Noise (Point2 Float) Float
perlin' perms grads smooth cs p' =
        let p = scale (1 / constant cs) p'
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
            s   = g0 `dot` (p `minus` toFloat p0)
            t   = g1 `dot` (p `minus` toFloat p1)
            u   = g2 `dot` (p `minus` toFloat p2)
            v   = g3 `dot` (p `minus` toFloat p3)
            -- Interpolate values
            (x,y)   = unlift p :: (Exp Float, Exp Float)
            (x0,y0) = unlift (toFloat p0) :: (Exp Float, Exp Float)
            sx  = smooth (x-x0)
            sy  = smooth (y-y0)
            a   = lerp sx s t
            b   = lerp sx u v
            c   = lerp sy a b
        in  c*0.5 + 0.5

fBm :: (RealFrac a, IsFloating a, Ord a, Elt a)
    => Noise (Point2 a) a -- ^ The basis noise function
    -> H a                -- ^ The Hurst exponent, a value between 0 and 1
    -> Lacunarity a       -- ^ The lacunarity or frequency step between successive frequencies
    -> Octaves            -- ^ The number of octaves to add
    -> Noise (Point2 a) a
{-fBm noise h' l' o' p = A.fold1 (+) vals ! index0 / maxfBm
    where vals   = noises -- A.zipWith (*) amps noises
          amps   = A.map (\x -> gain**x) nums
          gain   = l ** (-2*h)
          noises = A.map noise points
          points = A.zipWith scale freqs $ A.fill (index1 o) p
          freqs  = A.map (\x -> l**x) nums
          nums   = A.generate (index1 o) $ \ix -> let (Z:.i) = unlift ix in A.fromIntegral i
          maxfBm = geom gain (A.fromIntegral o)
          h = constant h'
          l = constant l'
          o = constant o'-}

fBm noise h l o p = foldl1' (+) vals / maxfBm
    where vals   = P.zipWith (*) amps noises
          amps   = P.map constant $ iterate (*gain) 1
          gain   = l ** (-2*h)
          noises = P.map noise points
          points = P.zipWith scale freqs (P.replicate o p)
          freqs  = P.map constant $ iterate (*l) 1
          maxfBm = constant $ if gain == 1 then 1 else geom gain (P.fromIntegral o)

{-fBm noise h' l' nocts p = fBm' nocts 1 1 0
    where h = constant h'
          l = constant l'
          gain' = l' ** (-2*h')
          gain = constant gain'
          maxfBm = constant $ if gain' == 1 then 1 else geom gain' (P.fromIntegral nocts)
          fBm' 0 f a val = val / maxfBm
          fBm' o f a val =
               let val' = (+val) . (*a) . noise . scale f $ p
               in val' `seq` fBm' (o-1) (l*f) (a*gain) val'-}

geom r n = (1 - r**n) / (1-r)

floor' :: Exp (Point2 Float) -> Exp (Point2 Int)
floor' p = lift (x',y')
       where (x,y) = unlift p :: (Exp Float, Exp Float)
             x' = A.floor x
             y' = A.floor y

toFloat :: Exp (Point2 Int) -> Exp (Point2 Float)
toFloat p = lift (x',y')
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
