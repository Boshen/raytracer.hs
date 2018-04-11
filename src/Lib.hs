module Lib (getImage) where

import Data.List (minimumBy)
import Data.Maybe
import Data.Ord
import Data.Vector.Storable (generate)
import Linear.Metric
import Linear.V3
import Linear.Vector
import Vision.Image
import Vision.Primitive (ix2)

data Ray = Ray
    { start :: V3 Double
    , direction :: V3 Double
    }

data Sphere = Sphere
    { position :: V3 Double
    , radius :: Double
    , diffuse :: V3 Double
    }

data Light = Light
    { lightPosition :: V3 Double
    , intensity :: V3 Double
    }

spheres :: [Sphere]
spheres =
    [ Sphere (V3 200 300 0) 100 (V3 1 0 0)
    , Sphere (V3 300 200 50) 50 (V3 0 1 0)
    , Sphere (V3 400 400 0) 100 (V3 0 0 1)
    , Sphere (V3 540 140 0) 100 (V3 (242 / 255) (190 / 255) (69 / 255))
    ]

lights :: [Light]
lights =
    [ Light (V3 0 240 100) (V3 1 1 1)
    , Light (V3 3200 3000 (-1000)) (V3 0.6 0.7 1)
    , Light (V3 600 0 (-100)) (V3 0.3 0.5 1)
    ]

getImage :: Int -> Int -> RGB
getImage w h = Manifest size pixels
    where
        size = ix2 h w
        pixels = generate (w * h) $ getPixel w

getPixel :: Int -> Int -> RGBPixel
getPixel w i = if hit then RGBPixel r g b else RGBPixel 255 255 255
    where
        (x, y) = (i `div` w, i `mod` w)
        ray = Ray (V3 (fromIntegral x) (fromIntegral y) (-2000)) (V3 0 0 1)
        hits = catMaybes $ intersect ray <$> spheres
        hit = not . null $ hits
        (sphere, d) = minimumBy (comparing snd) hits
        (V3 r g b) = round <$> traceLights ray sphere d lights

traceLights :: Ray -> Sphere -> Double -> [Light] -> V3 Double
traceLights ray sphere d ls =
    min 255 . (*255) <$> (sum $ (traceLight ray sphere d) <$> ls)

traceLight :: Ray -> Sphere -> Double -> Light -> V3 Double
traceLight ray sphere d light = if lambert <= 0 then V3 0 0 0 else coef
    where
        newStart = (start ray) + ((*d) <$> direction ray)
        normal = normalize $ newStart - (position sphere)
        dist = (lightPosition light) - newStart
        dir = (/(norm dist)) <$> dist
        lambert = dir `dot` normal
        coef =  (*lambert) <$> ((diffuse sphere) * (intensity light))

intersect :: Ray -> Sphere -> Maybe (Sphere, Double)
intersect ray sphere =
    let
        a = dir `dot` dir
        dist = strt ^-^ p
        b = 2 * dir `dot` dist
        c = dist `dot` dist - r * r
        d = b * b - 4 * a * c
        sqrtD = sqrt d
        t0 = (-b + sqrtD) / 2
        t1 = (-b - sqrtD) / 2
        mint = minimum $ filter (> 0.001) [t0, t1]
    in
        if d < 0 then Nothing else Just (sphere, mint)
    where
        (strt, dir) = (start ray, direction ray)
        (p, r) = (position sphere, radius sphere)
