{-# LANGUAGE TypeOperators #-}
module Graphics.HTerra.Noise
(
)
where

import Graphics.HTerra.Interp

import Data.Array.Accelerate as A
import Data.Bits ((.&.))

type Noise a b = Acc a -> Exp b
type Seed = Int

type Perms a = Acc (Vector a)
type Gradients a = Acc (Vector a)

-- 1D permutation table index function.
perm :: (Elt a, IsIntegral b, Elt b) => Perms a -> Exp b -> Exp a
perm perms x = perms ! index1 (A.fromIntegral x .&. 255)

-- 2D permutation table index function.
index :: (IsIntegral a, Elt a, IsIntegral b, Elt b) => Perms a -> Acc (Vector b) -> Exp a
index ps p = perm' (x + perm' y)
      where perm' = perm ps
            x = A.fromIntegral $ p ! index1 0
            y = A.fromIntegral $ p ! index1 1

-- Index the gradients vector.
grad :: (Elt a, IsIntegral b, Elt b) => Gradients a -> Exp b -> Acc (Vector a)
grad grads i = A.generate (index1 2) $
     \ix -> let (Z:.x) = unlift ix
                x' = A.fromIntegral x
                i' = A.fromIntegral i
            in grads ! index1 (2*i'+x')

perlin' :: Perms Word8 -> Gradients Float -> Smooth Float -> Noise (Vector Float) Float
perlin' perms grads smooth p =
        let p0  = A.map A.floor p :: Acc (Vector Int)
            p1  = p0 `plus` A.generate (index1 2) right :: Acc (Vector Int)
            p2  = p0 `plus` A.generate (index1 2) down :: Acc (Vector Int)
            p3  = p0 `plus` A.fill (index1 2) 1 :: Acc (Vector Int)
            idx = index perms
            g0  = grad grads $ idx p0 :: Acc (Vector Float)
            g1  = grad grads $ idx p1 :: Acc (Vector Float)
            g2  = grad grads $ idx p2 :: Acc (Vector Float)
            g3  = grad grads $ idx p3 :: Acc (Vector Float)
            s   = g0 `dot` (p `minus` A.map A.fromIntegral p0)
            t   = g1 `dot` (p `minus` A.map A.fromIntegral p1)
            u   = g2 `dot` (p `minus` A.map A.fromIntegral p2)
            v   = g3 `dot` (p `minus` A.map A.fromIntegral p3)
            x   = p ! index1 0
            y   = p ! index1 1
            x0  = A.fromIntegral $ p0 ! index1 0
            y0  = A.fromIntegral $ p0 ! index1 0
            sx  = smooth (x-x0)
            sy  = smooth (y-y0)
            a   = lerp s t sx
            b   = lerp u v sx
            c   = lerp a b sy
        in  c*0.5 + 0.5

right :: (IsIntegral a, Elt a, IsIntegral b, Elt b) => Exp (Z:.b) -> Exp a
right ix = let (Z:.x) = unlift ix in 1 - A.fromIntegral x

down :: (IsIntegral a, Elt a, IsIntegral b, Elt b) => Exp (Z:.b) -> Exp a
down ix = let (Z:.x) = unlift ix in A.fromIntegral x

-- Vector addition.
plus :: (IsNum a, Elt a) => Acc (Vector a) -> Acc (Vector a) -> Acc (Vector a)
plus a b = A.zipWith (+) a b

-- Vector subraction.
minus :: (IsNum a, Elt a) => Acc (Vector a) -> Acc (Vector a) -> Acc (Vector a)
minus a b = A.zipWith (-) a b

-- Vector dot product.
dot :: (IsNum a, Elt a) => Acc (Array (Z :. Int) a) -> Acc (Array (Z :. Int) a) -> Exp a
dot a b = (!index0) . A.fold1 (+) $ A.zipWith (*) a b
