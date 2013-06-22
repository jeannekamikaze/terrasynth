{-# LANGUAGE DeriveDataTypeable #-}
module Main where

------------------------------------------
-- Hi, this is a self-contained example.
--
-- Skip to 'evil:'
------------------------------------------

import Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as I
import qualified Data.Array.Accelerate.CUDA as C
import System.Console.CmdArgs

import Data.Bits ((.&.))
import Data.List (foldl1')
import System.Random

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (chr)
import System.IO

import Prelude as P

type ByteString = B.ByteString

-- Interpolation

type Smooth a = Exp a -> Exp a

type Interpolation a
     =  Exp a -- ^ Interpolation factor
     -> Exp a -- ^ First value
     -> Exp a -- ^ Second value
     -> Exp a

-- | Smooth out the given value using S-curve.
scurve :: (IsNum a, Elt a) => Smooth a
scurve t = t * t * (t * (-2) + 3)

-- | A generalised lerp that takes a smooth function as an argument.
lerp' :: (IsNum a, Elt a) => Smooth a -> Interpolation a
lerp' s t a b = a + (b-a)*t' where t' = s t

-- | Linearly interpolate two values.
lerp :: (IsNum a, Elt a) => Interpolation a
lerp = lerp' id

-- Noise

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

------------------------------------------------------------------------
-- evil: This is the function causing slow Analyzer times when o >= 6 --
------------------------------------------------------------------------
fBm noise h l o p = foldl1' (+) vals / maxfBm
    where vals   = P.zipWith (*) amps noises
          amps   = P.map constant $ iterate (*gain) 1
          gain   = l ** (-2*h)
          noises = P.map noise points
          points = P.zipWith scale freqs (P.replicate o p)
          freqs  = P.map constant $ iterate (*l) 1
          maxfBm = constant $ if gain == 1 then 1 else geom gain (P.fromIntegral o)

-------------------------------------------------------------------------------
-- evil: An alternative implementation of the above using explicit recursion --
--       It's not any better.                                                --
-------------------------------------------------------------------------------
-- Suffers from the same problem.
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

---------------------------------------------------
-- Nothing to see here. Skip to 'section: Image' --
---------------------------------------------------

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

--------------------
-- section: Image --
--------------------

type Image a = Array DIM2 a
type Backend a = Acc a -> a
type Width = Int
type Height = Int

--------------------------------------------------------------------------------------
-- These are the functions responsible for creating images. They take a noise function
-- such as fBm as an argument.
---------------------------------------------------------------------------------------

-- | Create an image from a noise function.
image :: (IsNum a, Elt a, Elt b)
      => Backend (Image b) -> Noise (Point2 a) b -> Width -> Height -> Image b
image run noise w h = run $ image' noise w h

-- | Create an image from a noise function.
image' :: (IsNum a, Elt a, Elt b) => Noise (Point2 a) b -> Width -> Height -> Acc (Image b)
image' noise w h = A.generate (constant (Z:.w:.h)) $
       \ix -> let (Z:.x:.y) = unlift ix
              in noise . lift $ (A.fromIntegral x, A.fromIntegral y)

--------------
-- Image IO --
--------------

-- | Save the image as PGM.
writePGM :: FilePath -> Image Float -> IO ()
writePGM file img =
         let Z:.w:.h = arrayShape img
             img' = B.pack . P.map toChar . toList $ img
         in withBinaryFile file WriteMode $ writePGM' w h img'

-- | Save the image as PGM.
writePGM' :: Int -> Int -> ByteString -> Handle -> IO ()
writePGM' w h bits hnd =
          let header = headerPGM w h
          in B.hPut hnd header >> B.hPut hnd bits

headerPGM :: Int -> Int -> ByteString
headerPGM w h = B.pack $ "P5 " ++ show w ++ " " ++ show h ++ " 255" ++ "\n"

toChar :: Float -> Char
toChar = chr . P.floor . (*255)

----------------------------------------------------------------------------
-- Main
--
-- When running the program, use -n X where X is the number of iterations.
-- The other arguments take sane defaults.
----------------------------------------------------------------------------

fBmPerlin :: Backend (Image Float) -> Seed -> CellSize
          -> H Float -> Lacunarity Float -> Octaves
          -> Width -> Height -> FilePath -> IO ()
fBmPerlin run seed cs hu l o w h file =
          let img = image run (fBm (perlin seed cs) hu l o) w h
          in writePGM file img

data Runner = Interpreter | Cuda deriving (Data, Typeable, Show)

data Demo = Demo
     { backend  :: Runner
     , seed     :: Int
     , cellSize :: Float
     , hurst    :: Float
     , lacu     :: Float
     , noct     :: Int
     , width    :: Int
     , height   :: Int
     , file     :: String
     } deriving (Data, Typeable, Show)

defaultArgs = cmdArgsMode $ Demo
            { backend = Cuda        &= name "b" &= typ "Interpreter | Cuda"
                                    &= help "Accelerate backend"
            , seed = 123            &= name "s" &= help "Random seed"
            , cellSize = 64         &= name "c" &= help "Cell size"
            , hurst = 0.5           &= name "u" &= typ "[0,1]" &= help "Hurst exponent"
            , lacu = 2              &= name "l" &= help "Lacunarity"
            , noct = 2              &= name "n" &= help "The number of octaves to add"
            , width = 2048          &= name "w" &= help "Image width"
            , height = 2048         &= name "h" &= help "Image height"
            , file = "foo.pgm"      &= name "f" &= typFile &= help "Output file name"
            }

toBackend Interpreter = I.run
toBackend Cuda = C.run

main = do
     (Demo run s cs hu lacu noct w h file) <- cmdArgsRun defaultArgs
     putStrLn "Use -n X to control the number of iterations. --help for details."
     putStrLn "On my machine, -n 7 starts getting unusually slow."
     fBmPerlin (toBackend run) s cs hu lacu noct w h file
