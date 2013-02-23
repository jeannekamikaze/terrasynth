module Graphics.HTerra.Image
(
    Image
,   writePGM
,   writePGM'
)
where

import Data.Array.Accelerate as A
import Data.ByteString.Char8 as B
import Data.Char (chr)
import System.IO
import Prelude as P

type Image = Array DIM2 Float

-- | Save the image as PGM.
writePGM :: FilePath -> Image -> IO ()
writePGM file img =
         let Z:.w:.h = arrayShape img
             img' = pack . P.map toChar . toList $ img
         in withBinaryFile file WriteMode $ writePGM' w h img'

-- | Save the image as PGM.
writePGM' :: Int -> Int -> ByteString -> Handle -> IO ()
writePGM' w h bits hnd =
          let header = headerPGM w h
          in hPut hnd header >> hPut hnd bits

headerPGM :: Int -> Int -> ByteString
headerPGM w h = pack . P.filter (/='"') $
          "P5 " ++ show w ++ " " ++ show h ++ " 255" ++ "\n"

toChar :: Float -> Char
toChar = chr . P.floor . (*255)

black :: Int -> Int -> Image
black w h = A.fromList (Z:.w:.h) (repeat 0)
