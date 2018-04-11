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
    , reflection :: Double
    }

data Light = Light
    { lightPosition :: V3 Double
    , intensity :: V3 Double
    }

spheres :: [Sphere]
spheres =
    [ Sphere (V3 200 300 0) 100 (V3 1 0 0) 0.2
    , Sphere (V3 300 200 50) 50 (V3 0 1 0) 0.5
    , Sphere (V3 400 400 0) 100 (V3 0 0 1) 0.9
    , Sphere (V3 540 140 0) 100 (V3 (242 / 255) (190 / 255) (69 / 255)) 0.8
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
getPixel w i = RGBPixel r g b
    where
        color = V3 0 0 0
        (x, y) = (i `div` w, i `mod` w)
        ray = Ray (V3 (fromIntegral x) (fromIntegral y) (-2000)) (V3 0 0 1)
        (V3 r g b) = max 0 . round . min 255 . (*255) <$> snd (trace 0 1 color ray)

trace :: Int -> Double -> V3 Double -> Ray -> (Double, V3 Double)
trace level coef color ray = if notHit || coef <= 0 || level >= 15
    then if notHit && level == 0 then (0, V3 1 1 1) else (0, color)
    else trace (level + 1) (coef * reflection sphere) (color + c) newRay
    where
        hits = catMaybes $ intersect ray <$> spheres
        notHit = null hits
        (sphere, d) = minimumBy (comparing snd) hits
        c = (*coef) <$> traceLights ray sphere d lights

        newStart = (start ray) + ((*d) <$> direction ray)
        normal = normalize $ newStart - (position sphere)
        reflect = 2 * ((direction ray) `dot` normal)
        newRay = Ray newStart $ (direction ray - ((*reflect) <$> normal))

traceLights :: Ray -> Sphere -> Double -> [Light] -> V3 Double
traceLights ray sphere d ls =
    sum $ (traceLight ray sphere d) <$> ls

traceLight :: Ray -> Sphere -> Double -> Light -> V3 Double
traceLight ray sphere d light = if lambert <= 0 then V3 0 0 0 else coef
    where
        newStart = (start ray) + ((*d) <$> direction ray)
        normal = normalize $ newStart - (position sphere)
        dist = (lightPosition light) - newStart
        dir = (/(norm dist)) <$> dist
        lambert = dir `dot` normal
        coef = (*lambert) <$> ((diffuse sphere) * (intensity light))

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
        ts = filter (>= 0.001) [t0, t1]
    in
        if d < 0 || null ts then Nothing else Just (sphere, minimum ts)
    where
        (strt, dir) = (start ray, direction ray)
        (p, r) = (position sphere, radius sphere)
