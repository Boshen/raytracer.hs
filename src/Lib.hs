module Lib (getImage) where

import Data.Maybe
import Data.Vec ((:.)(..), Vec3, dot, zipWith)
import Data.Vector.Storable (generate)
import Vision.Image
import Vision.Primitive (ix2)

data Ray = Ray
    { start :: Vec3 Double
    , direction :: Vec3 Double
    }

data Sphere = Sphere
    { position :: Vec3 Double
    , radius :: Double
    }

spheres :: [Sphere]
spheres =
    [ Sphere (200 :. 300 :. 0) 100
    , Sphere (300 :. 200 :. 50) 50
    , Sphere (400 :. 400 :. 0) 100
    , Sphere (540 :. 140 :. 0) 100
    ]

getImage :: Int -> Int -> RGB
getImage w h = Manifest size pixels
    where
        size = ix2 h w
        pixels = generate (w * h) $ getPixel w

getPixel :: Int -> Int -> RGBPixel
getPixel w i = if hit then RGBPixel 0 0 0 else RGBPixel 255 255 255
    where
        (x, y) = (i `div` w, i `mod` w)
        ray = Ray (fromIntegral x :. fromIntegral y :. (-2000)) (0 :. 0 :. 1)
        hits = intersect ray <$> spheres
        hit = not . null $ catMaybes hits

intersect :: Ray -> Sphere -> Maybe (Sphere, Double)
intersect ray sphere =
    let
        a = dot dir dir
        dist = Data.Vec.zipWith (-) strt p
        b = 2 * dot dir dist
        c = dot dist dist - r * r
        d = b * b - 4 * a * c
        sqrtD = sqrt d
        t0 = (-b + sqrtD) / 2
        t1 = (-b - sqrtD) / 2
        mint = minimum $ filter (>=0) [t0, t1]
    in
        if d < 0 then Nothing else Just (sphere, mint)
    where
        (strt, dir) = (start ray, direction ray)
        (p, r) = (position sphere, radius sphere)
