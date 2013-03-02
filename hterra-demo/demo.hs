{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Graphics.HTerra.Image
import Graphics.HTerra.Image.IO
import Graphics.HTerra.Noise as N

import Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as I
import qualified Data.Array.Accelerate.CUDA as C
import System.Console.CmdArgs
import Prelude as P

cells :: Backend (Image Float) -> Seed -> CellSize -> Octaves -> Width -> Height -> FilePath -> IO ()
cells run seed cs o w h file =
      let img = cellsImage run (perlin seed cs) o w h
      in writePGM file img

fBmPerlin :: Backend (Image Float) -> Seed -> CellSize
          -> H Float -> Lacunarity Float -> Octaves
          -> Width -> Height -> FilePath -> IO ()
fBmPerlin run seed cs hu l o w h file =
          let img = image run (fBm (perlin seed cs) hu l o) w h
              --img = fBmImage run (perlin seed cs) hu l o w h
          in writePGM file img

perlinImage :: Backend (Image Float)-> Seed -> CellSize -> Width -> Height -> FilePath -> IO ()
perlinImage run seed cs w h file =
            let img = image run (perlin seed cs) w h
            in writePGM file img

gen cs x y = let cs' = constant cs in lift (x/cs', y/cs')

data Runner = Interpreter | Cuda deriving (Data, Typeable, Show)
data NoiseType = Perlin | Fbm | Cells deriving (Data, Typeable, Show)

data Demo = Demo
     { backend  :: Runner
     , noise    :: NoiseType
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
            { backend = Interpreter &= name "b" &= typ "Interpreter | Cuda"
                                    &= help "Accelerate backend"
            , noise = Perlin        &= name "t" &= typ "Perlin | Fbm | Cells"
                                    &= help "The type of noise"
            , seed = 123            &= name "s" &= help "Random seed"
            , cellSize = 64         &= name "c" &= help "Cell size"
            , hurst = 0.5           &= name "u" &= typ "[0,1]" &= help "Hurst exponent"
            , lacu = 2              &= name "l" &= help "Lacunarity"
            , noct = 6              &= name "n" &= help "The number of octaves to add"
            , width = 256           &= name "w" &= help "Image width"
            , height = 256          &= name "h" &= help "Image height"
            , file = "foo.pgm"      &= name "f" &= typFile &= help "Output file name"
            }

toBackend Interpreter = I.run
toBackend Cuda = C.run

main = do
     (Demo run noise s cs hu lacu noct w h file) <- cmdArgsRun defaultArgs
     case noise of
          Perlin -> perlinImage (toBackend run) s cs w h file
          Fbm    -> fBmPerlin (toBackend run) s cs hu lacu noct w h file
          Cells  -> cells (toBackend run) s cs noct w h file
