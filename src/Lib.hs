module Lib (getImage) where

import Data.Vec ((:.)(..), Vec3, dot, zipWith)
import Data.Vector.Storable
import Vision.Image
import Vision.Primitive (Size, ix2)

type Dimension = (Int, Int)

data Ray = Ray
    { start :: Vec3 Double
    , dir :: Vec3 Double
    }

data Sphere = Sphere
    { pos :: Vec3 Double
    , radius :: Double
    }

sphere :: Sphere
sphere = Sphere (20 :. 20 :. 20) 20.0

w = 40
h = 40

size :: Size
size = ix2 h w

getImage :: RGB
getImage = Manifest size pixels

pixels :: Vector RGBPixel
pixels = generate (w * h) getPixel

getPixel :: Int -> RGBPixel
getPixel i = if hit then RGBPixel 0 0 0 else RGBPixel 255 255 255
    where
        (x, y) = (i `div` w, i `mod` w)
        ray = Ray (fromIntegral x :. fromIntegral y :. 0) (0 :. 0 :. 1)
        hit = intersect ray

intersect :: Ray -> Bool
intersect ray =
    let
        a = dot dir dir -- a = d.d
        dist = Data.Vec.zipWith (-) start pos -- dist = p0 - c
        b = 2 * dot dir dist -- 2d.(p0 - c)
        c = dot dist dist - r * r -- (p0 - c).(p0 - c) - r^2
        d = b * b - 4 * a * c -- discriminant
    in
        d >= 0
    where
        (Ray start dir) = ray
        (Sphere pos r) = sphere
