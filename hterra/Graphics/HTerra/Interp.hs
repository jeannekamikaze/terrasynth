module Graphics.HTerra.Interp
(
    -- * Data types
    Smooth
,   Interpolation
    -- * Smooth functions
,   scurve
,   scos
    -- * Interpolation functions
,   lerp'
,   lerp
,   scurvei
,   cosi
)
where

import Data.Array.Accelerate

type Smooth a = Exp a -> Exp a

type Interpolation a
     =  Exp a -- ^ Interpolation factor
     -> Exp a -- ^ First value
     -> Exp a -- ^ Second value
     -> Exp a

-- | Smooth out the given value using S-curve.
scurve :: (IsNum a, Elt a) => Smooth a
scurve t = t * t * (t * (-2) + 3)

-- | Smooth out the given value using cos.
scos :: (IsFloating a, Elt a) => Smooth a
scos t = (1 - cos (pi-t)) / 2

-- | A generalised lerp that takes a smooth function as an argument.
lerp' :: (IsNum a, Elt a) => Smooth a -> Interpolation a
lerp' s t a b = a + (b-a)*t' where t' = s t

-- | Linearly interpolate two values.
lerp :: (IsNum a, Elt a) => Interpolation a
lerp = lerp' id

-- | Interpolate two values using the S-curve smooth function.
scurvei :: (IsNum a, Elt a) => Interpolation a
scurvei = lerp' scurve

-- | Interpolate two values using the cos smooth function.
cosi :: (IsFloating a, Elt a) => Interpolation a
cosi = lerp' scos

knotspline4 :: (IsFloating a, Elt a) => Exp a -> Exp a -> Exp a -> Exp a -> Exp a -> Exp a
knotspline4 k0 k1 k2 k3 x = (c3*x + c2)*x + c0
            where
                c0 = k1
                c1 = 0.5 * (k2-k0)
                c2 = k0 - 2.5*k1 + 2*k2 - 0.5*k3
                c3 = (-0.5)*k0 + 1.5*k1 - 1.5*k2 + 0.5*k3
