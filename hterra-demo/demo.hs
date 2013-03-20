{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Graphics.HTerra.Image
import Graphics.HTerra.Image.IO
import Graphics.HTerra.Interp
import Graphics.HTerra.Noise as N

import qualified Data.Array.Accelerate.CUDA as C
import System.Console.CmdArgs
import Prelude as P

{-cells :: Backend (Image Float) -> Seed -> CellSize -> Octaves -> Width -> Height -> FilePath -> IO ()
cells run seed cs o w h file =
      let img = cellsImage run (perlin seed cs) o w h
      in writePGM file img-}
cells = undefined

fBmPerlin :: Seed -> CellSize -> Hurst -> Lacunarity -> Octaves -> Width -> Height
          -> FilePath -> IO ()
fBmPerlin seed cs hu l o w h file =
          let ps  = perms seed 256
              gs  =  grads 256
              basisParams = perlinParams ps gs cs
              params = fBmParams basisParams hu l o
              img = fBmImage C.run1 (perlin scurve) o w h params
          in writePGM file img

perlinImage :: Seed -> CellSize -> Width -> Height -> FilePath -> IO ()
perlinImage seed cs w h file =
            let ps  = perms seed 256
                gs  =  grads 256
                params = perlinParams ps gs cs
                img = image C.run1 (perlin scurve) w h params
            in writePGM file img

data NoiseType = Perlin | Fbm | Cells deriving (Data, Typeable, Show)

data Demo = Demo
     { noise    :: NoiseType
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
            { noise = Perlin        &= name "t" &= typ "Perlin | Fbm | Cells"
                                    &= help "The type of noise"
            , seed = 123            &= name "s" &= help "Random seed"
            , cellSize = 64         &= name "c" &= help "Cell size"
            , hurst = 0.5           &= name "u" &= typ "[0,1]" &= help "Hurst exponent"
            , lacu = 2              &= name "l" &= help "Lacunarity"
            , noct = 6              &= name "n" &= help "The number of octaves"
            , width = 256           &= name "w" &= help "Image width"
            , height = 256          &= name "h" &= help "Image height"
            , file = "foo.pgm"      &= name "f" &= typFile &= help "Output file name"
            }

main = do
     (Demo noise s cs hu lacu noct w h file) <- cmdArgsRun defaultArgs
     case noise of
          Perlin -> perlinImage s cs w h file
          Fbm    -> fBmPerlin s cs hu lacu noct w h file
          Cells  -> cells s cs noct w h file
