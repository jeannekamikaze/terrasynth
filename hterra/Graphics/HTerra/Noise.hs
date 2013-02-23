module Graphics.HTerra.Noise
(
)
where

import Graphics.HTerra.Interp
import Data.Array.Accelerate

type Noise a b = Vector a -> b
type Seed = Int

type PRNtable a = Acc (Vector a)
type ValueTable a = Acc (Vector a)

perlin2 :: (IsNum r, IsFloating f)
        => PRNtable r -> ValueTable f -> Interpolation f -> Noise f f
