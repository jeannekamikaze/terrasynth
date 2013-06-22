{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as I
import qualified Data.Array.Accelerate.CUDA as C
import Data.Bits
import qualified Data.ByteString.Char8 as B
import Data.Char (chr)
import System.Console.CmdArgs
import System.IO
import System.Random
import Prelude as P

type ByteString = B.ByteString

--
-- Interpolation
--

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

--
-- Noise
--

type Point2 a = (a,a)
type Noise a b = Exp a -> Exp b
type Perms a = Acc (Vector a)
type Gradients a = Acc (Vector (Point2 a))

-- | A random number seed.
type Seed = Int

-- Create a permutation table of 256 values.
perms :: Seed -> [Word8]
perms seed = create (mkStdGen seed) 0 []
      where create _ 256 xs = xs
            create g i xs =
                   let (x,g') = randomR (0,255) g
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
       let perms' = use . A.fromList (Z:.256) $ perms seed
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

--
-- Image
--

type Image = Array DIM2 Float
type Matrix a = Array DIM2 a
type Backend = Acc Image -> Image
type Width = Int
type Height = Int
type CellSize = Float

-- | Create an image from a noise function.
noiseImage :: Backend -> Noise (Point2 Float) Float -> Matrix (Point2 Float) -> Image
noiseImage run noise mat = run $ noiseImage' noise mat

-- | Create an image from a noise function.
noiseImage' :: Noise (Point2 Float) Float -> Matrix (Point2 Float) -> Acc Image
noiseImage' noise mat = A.map noise . use $ mat

-- | Save the image as PGM.
writePGM :: FilePath -> Image -> IO ()
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

perlinImage :: Backend -> Seed -> CellSize -> Width -> Height -> FilePath -> IO ()
perlinImage run seed cs w h file =
            let image = perlinImage' run seed cs w h
            in writePGM file image

perlinImage' :: Backend -> Seed -> CellSize -> Width -> Height -> Image
perlinImage' run seed cs w h = noiseImage run (perlin seed) $ matrix cs w h

matrix :: CellSize -> Width -> Height -> Matrix (Point2 Float)
matrix cs w h = A.fromList (Z:.w:.h) $ [(y,x) | y <- [0,s..h'], x <- [0,s..w']]
       where w' = (P.fromIntegral w - 1) / cs
             h' = (P.fromIntegral h - 1) / cs
             s  = 1/cs

--
-- Main
--

data Runner = Interpreter | Cuda deriving (Data, Typeable, Show)

data Demo = Demo
     { backend  :: Runner
     , seed     :: Int
     , cellSize :: Float
     , width    :: Int
     , height   :: Int
     , file     :: String
     } deriving (Data, Typeable, Show)

defaultArgs = cmdArgsMode $ Demo
            { backend = Cuda &= name "b" &= typ "Interpreter|Cuda" &= help "Accelerate backend"
            , seed = 123 &= name "s" &= help "Random seed"
            , cellSize = 64 &= name "c" &= help "Cell size"
            , width = 1024 &= name "w" &= help "Image width"
            , height = 1024 &= name "h" &= help "Image height"
            , file = "foo.pgm" &= name "f" &= typFile &= help "Output file name"
            }

toBackend Interpreter = I.run
toBackend Cuda = C.run

main = do
     (Demo run s cs w h file) <- cmdArgsRun defaultArgs
     perlinImage (toBackend run) s cs w h file
