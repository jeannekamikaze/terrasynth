{-# LANGUAGE TypeOperators #-}
module Graphics.HTerra.Noise
(
    -- * Data types
    Point2
,   Noise
,   Perms
,   Gradients
,   Seed
    -- * Noise functions
,   perlin
,   perlin'
)
where

import Graphics.HTerra.Interp

import Data.Array.Accelerate as A
import Data.Bits ((.&.))
import System.Random
import Prelude as P

import qualified Data.Array.Accelerate.Interpreter as I

type Point2 a = (a,a)
type Noise a b = Exp a -> Exp b
type Perms a = Acc (Vector a)
type Gradients a = Acc (Vector (Point2 a))

-- | A random number seed.
type Seed = Int

-- Create a permutation table of N values.
perms :: (Num a, Eq a, Random a) => Seed -> a -> [a]
perms seed n = create (mkStdGen seed) 0 []
      where create g i xs
                   | i == n = xs
                   | otherwise =
                     let (x,g') = randomR (0,n-1) g
                         xs' = x:xs
                     in xs' `seq` create g' (i+1) xs'

-- Create a list of N gradient vectors along the unit circle.
grads :: Int -> [Point2 Float]
grads i =
      let step = 2*pi / (P.fromIntegral i)
          grads' a = (cos a, sin a) : grads' (a+step)
      in P.take (2*i) $ grads' 0

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

perlin :: Seed -> Noise (Point2 Float) Float
perlin seed =
       let perms' = use . A.fromList (Z:.256) $ perms seed 256
           grads' = use . A.fromList (Z:.256) $ grads 256
       in perlin' perms' grads' scurve

perlin' :: Perms Word8 -> Gradients Float -> Smooth Float -> Noise (Point2 Float) Float
perlin' perms grads smooth p =
        let -- Compute gradients
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

plus :: (IsNum a, Elt a) => Exp (a,a) -> Exp (a,a) -> Exp (a,a)
plus a b = lift (ax+bx, ay+by)
     where (ax,ay) = (A.fst a, A.snd a)
           (bx,by) = (A.fst b, A.snd b)

minus :: (IsNum a, Elt a) => Exp (a,a) -> Exp (a,a) -> Exp (a,a)
minus a b = lift (ax-bx, ay-by)
      where (ax,ay) = (A.fst a, A.snd a)
            (bx,by) = (A.fst b, A.snd b)

dot :: (IsNum a, Elt a) => Exp (Point2 a) -> Exp (Point2 a) -> Exp a
dot a b = ax*bx + ay*by
    where (ax,ay) = (A.fst a, A.snd a)
          (bx,by) = (A.fst b, A.snd b)
