module Graphics.HTerra.Interp
where

import Data.Array.Accelerate

type Interpolation a = Exp a -> Exp a -> Exp a -> Exp a

lerp :: (IsNum a, Elt a) => Interpolation a
lerp t a b = a + t*(b-a)

scurve :: (IsNum a, Elt a) => Interpolation a
scurve t a b = a*x + b*(1-x)
       where
        x = 3*t2 - 2*t3
        t2 = t*t
        t3 = t*t2

cosi :: (IsFloating a, Elt a) => Interpolation a
cosi t a b = a*x + b*(1-x) where x = 1 - cos (pi-t) / 2

knotspline4 :: (IsFloating a, Elt a) => Exp a -> Exp a -> Exp a -> Exp a -> Exp a -> Exp a
knotspline4 k0 k1 k2 k3 x = (c3*x + c2)*x + c0
            where
                c0 = k1
                c1 = 0.5 * (k2-k0)
                c2 = k0 - 2.5*k1 + 2*k2 - 0.5*k3
                c3 = (-0.5)*k0 + 1.5*k1 - 1.5*k2 + 0.5*k3
