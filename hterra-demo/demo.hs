{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Graphics.HTerra.Image
import Graphics.HTerra.Noise as N

import Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as I
import qualified Data.Array.Accelerate.CUDA as C
import System.Console.CmdArgs
import Prelude as P

perlinImage :: Backend (Image Float)-> Seed -> CellSize -> Width -> Height -> FilePath -> IO ()
perlinImage run seed cs w h file =
            let image = perlinImage' run seed cs w h
            in writePGM file image

perlinImage' :: Backend (Image Float) -> Seed -> CellSize -> Width -> Height -> Image Float
perlinImage' run seed cs w h = noiseImage run (N.perlin seed) $ matrix' cs w h

matrix' :: CellSize -> Width -> Height -> Matrix (Point2 Float)
matrix' cs = matrix pdiv
        where pdiv (x,y) = (P.fromIntegral x / cs, P.fromIntegral y / cs)

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
            { backend = Interpreter &= name "b" &= typ "Interpreter|Cuda" &= help "Accelerate backend"
            , seed = 123 &= name "s" &= help "Random seed"
            , cellSize = 64 &= name "c" &= help "Cell size"
            , width = 256 &= name "w" &= help "Image width"
            , height = 256 &= name "h" &= help "Image height"
            , file = "foo.pgm" &= name "f" &= typFile &= help "Output file name"
            }

toBackend Interpreter = I.run
toBackend Cuda = C.run

main = do
     (Demo run s cs w h file) <- cmdArgsRun defaultArgs
     perlinImage (toBackend run) s cs w h file
