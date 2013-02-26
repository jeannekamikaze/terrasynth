module Main where

import Graphics.HTerra.Image

import Data.Array.Accelerate.Interpreter

perlin :: Seed -> CellSize -> Width -> Height -> FilePath -> IO ()
perlin seed cs w h file =
       let image = perlinImage run seed cs w h
       in writePGM file image
