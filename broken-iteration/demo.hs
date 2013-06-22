{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
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

-------------------
-- Interpolation --
-------------------

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

-----------
-- Noise --
-----------

type Point2 a = (a,a)
type Point3 a = (a,a,a)
type Noise a b = Exp a -> Exp b

type Perms a = Vector a
type Gradients a = Vector (Point2 a)

type CellSize = Float
type Hurst = Float
type Lacunarity = Float
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
                   let step = 2*pi / (P.fromIntegral i)
                       grads' a = (cos a, sin a) : grads' (a+step)
                   in P.take i $ grads' 0

-- 1D permutation table index function.
perm :: (Elt a, IsIntegral b, Elt b) => Acc (Perms a) -> Exp b -> Exp a
perm perms x = perms ! index1 (A.fromIntegral x .&. 255)

-- 2D permutation table index function.
index :: (IsIntegral a, Elt a) => Acc (Perms a) -> Exp (Point2 Int) -> Exp a
index ps p = perm' (x' + perm' y')
      where perm' = perm ps
            (x,y) = unlift p :: (Exp Int, Exp Int)
            x' = A.fromIntegral x
            y' = A.fromIntegral y

-- Index the gradients vector.
grad :: (Elt a, IsIntegral b, Elt b) => Acc (Gradients a) -> Exp b -> Exp (Point2 a)
grad grads i = grads ! index1 (A.fromIntegral i)

-- | Perlin noise parameters.
type PerlinParams = (Perms Word8, Gradients Float, Vector Float)

perlinParams :: Perms Word8 -> Gradients Float -> CellSize -> PerlinParams
perlinParams ps gs cs = (ps, gs, A.fromList (Z:.1) [cs])

-- | Perlin noise function.
perlin :: Smooth Float -> Acc PerlinParams -> Noise (Point2 Float) Float
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

-- | fBm parameters.
type FbmParams a = (a, Vector Float)

fBmParams :: a -> Hurst -> Lacunarity -> Octaves -> FbmParams a
fBmParams c h l o = (c, A.fromList (Z:.3) [h, l, P.fromIntegral o])

-- | fBm noise function.
fBm' :: (Arrays c)
     => (Acc c -> Noise (Point2 Float) Float) -- ^ The basis noise function
     -> Acc (FbmParams c)
     -> Noise (Point3 Float) Float

fBm' noise' accParams p' = (/maxfBm) . (*a) . noise . scale f $ lift (x,y)
     where (basisParams, params) = unlift accParams
           h = params ! index1 0
           l = params ! index1 1
           o = params ! index1 2
           f = l ** z
           a = l ** (-2*h*z)
           --a = gain ** z
           gain = l ** (-2*h)
           maxfBm = geom gain o
           noise = noise' basisParams
           (x,y,z) = unlift p' :: (Exp Float, Exp Float, Exp Float)

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


-----------
-- Image --
-----------

type Image a = Array DIM2 a
type Runner a b = (Acc a -> Acc b) -> a -> b
type Width = Int
type Height = Int

--------------------------------------------------------------------------------------
-- evil: This is the code that generates an image. Problem starts here.
---------------------------------------------------------------------------------------

-- | Create an image using fractional Brownian motion.
fBmImage :: (Arrays c)
         => Runner (FbmParams c) (Image Float)
         -> (Acc c -> Noise (Point2 Float) Float)
         -> Octaves -> Width -> Height -> FbmParams c -> Image Float
fBmImage run1 noise o w h = run1 $ fBmImage' noise o w h

-- | Create an image using fractional Brownian motion.
fBmImage' :: (Arrays c)
          => (Acc c -> Noise (Point2 Float) Float)
          -> Octaves -> Width -> Height -> Acc (FbmParams c) -> Acc (Image Float)
fBmImage' noise' o w h params =
          let noise = fBm' noise' params
          in A.fold1 (+) $ A.generate (constant (Z:.w:.h:.o)) $
             \ix -> let (Z:.x:.y:.z) = unlift ix :: (Z :. Exp Int :. Exp Int :. Exp Int)
                    in noise . lift $ (A.fromIntegral x, A.fromIntegral y, A.fromIntegral z)

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
----------------------------------------------------------------------------

fBmPerlin :: Seed -> CellSize -> Hurst -> Lacunarity -> Octaves -> Width -> Height
          -> FilePath -> IO ()
fBmPerlin seed cs hu l o w h file =
          let ps  = perms seed 256
              gs  =  grads 256
              basisParams = perlinParams ps gs cs
              params = fBmParams basisParams hu l o
              img = fBmImage C.run1 (perlin scurve) o w h params
          in writePGM file img

data Demo = Demo
     { seed     :: Int
     , cellSize :: Float
     , hurst    :: Float
     , lacu     :: Float
     , noct     :: Int
     , width    :: Int
     , height   :: Int
     , file     :: String
     } deriving (Data, Typeable, Show)

defaultArgs = cmdArgsMode $ Demo
            { seed = 123            &= name "s" &= help "Random seed"
            , cellSize = 64         &= name "c" &= help "Cell size"
            , hurst = 0.5           &= name "u" &= typ "[0,1]" &= help "Hurst exponent"
            , lacu = 2              &= name "l" &= help "Lacunarity"
            , noct = 6              &= name "n" &= help "The number of octaves"
            , width = 256           &= name "w" &= help "Image width"
            , height = 256          &= name "h" &= help "Image height"
            , file = "foo.pgm"      &= name "f" &= typFile &= help "Output file name"
            }

main = do
     (Demo s cs hu lacu noct w h file) <- cmdArgsRun defaultArgs
     fBmPerlin s cs hu lacu noct w h file
