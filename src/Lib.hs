module Lib (getImage) where

import Data.Maybe
import Data.Vector.Storable (generate)
import Linear.Metric
import Linear.V3
import Linear.Vector
import Vision.Image
import Vision.Primitive (ix2)

type Color = V3 Double -- Color as an RGB value between 0 and 1
type Vector = V3 Double

data Ray = Ray
    { start :: Vector
    , direction :: Vector
    }

data Shape =
    Plane
    { position :: Vector
    , planeNormal :: Vector
    , color :: Color
    , reflection :: Double
    , specular :: Color
    , shininess :: Double
    }
    | Sphere
    { position :: Vector
    , radius :: Double
    , color :: Color
    , reflection :: Double
    , specular :: Color
    , shininess :: Double
    } deriving (Eq)

data Light = Light
    { lightPosition :: Vector
    , lightIntensity :: Color
    }

-- distance of intersection, between shape and ray
data Intersection = Intersection Double Ray Shape

shapes :: [Shape]
shapes =
    [ Sphere (V3 200 300 0) 100 (V3 1 0 0) 0.2 (V3 1 1 1) 30
    , Sphere (V3 350 250 0) 50 (V3 0 1 0) 0.2 (V3 1 1 1) 30
    , Sphere (V3 400 400 0) 100 (V3 0 0 1) 0.2 (V3 1 1 1) 30
    , Sphere (V3 540 140 0) 100 (V3 (242 / 255) (190 / 255) (69 / 255)) 0.2 (V3 1 1 1) 30
    , Plane (V3 200 300 (100)) (V3 0 0 (-1)) (V3 0 0 0) 0.2 (V3 1 1 1) 0
    ]

lights :: [Light]
lights =
    [ Light (V3 2000 2000 3000) (V3 0.8 0.8 0.8)
    , Light (V3 (-2000) (-2000) 3000) (V3 0.8 0.8 0.8)
    ]

getImage :: Int -> Int -> RGB
getImage w h = Manifest size pixels
    where
        size = ix2 h w
        pixels = generate (w * h) $ getPixel w

getPixel :: Int -> Int -> RGBPixel
getPixel w i = RGBPixel r g b
    where
        (x, y) = (i `div` w, i `mod` w)
        ray = Ray (V3 (fromIntegral x) (fromIntegral y) (-2000)) (V3 0 0 1)
        summedColor = trace 0 1 (V3 0 0 0) ray
        (V3 r g b) = max 0 . round . min 255 . (*255) <$> snd summedColor

trace :: Int -> Double -> Color -> Ray -> (Double, Color)
trace level coef clr ray = if notHit || coef <= 0 || level >= 15
    then if notHit && level ==0 then (0, V3 1 1 1) else (0, clr)
    else trace (level + 1) (coef * reflection shape) (clr + shadeColor) newRay
    where
        int = foldl (minIntersect ray) Nothing shapes
        notHit = isNothing int
        (Intersection d _ shape) = fromJust int
        shadeColor = coef *^ (sum $ calcColor (fromJust int) <$> lights)

        newStart = (start ray) + (d *^ direction ray)
        n = normalize $ newStart - (position shape)
        reflect = 2 * ((direction ray) `dot` n)
        newRay = Ray newStart (direction ray - (reflect *^ n))

calcColor :: Intersection -> Light -> Color
calcColor i@(Intersection _ (Ray _ r) shape) (Light lightPos li) = if inShadow then ambient else c
    where
        p = intersectionPoint i -- point
        v = normalize $ p - r -- view
        l = normalize $ p - lightPos -- light
        n = case shape of
            (Plane _ normal _ _ _ _) -> normal
            (Sphere pos _ _ _ _ _) -> normalize $ p - pos -- normal

        -- when the object is blocked by another object
        shadowRay = Ray (p + 0.001 *^ l) l
        inShadow = isJust $ foldl (minIntersect shadowRay) Nothing (filter (/= shape) shapes)

        -- when there is no light source
        ambient = 0.1 *^ (color shape)

        -- lambertian reflection is often used as a model for diffuse reflection
        -- object's from the real world reflect on average around 18% of the light they receive.
        lambertian = (max 0 (n `dot` l)) *^ li * (color shape)

        -- Blinn–Phong shading model
        h = normalize $ l + v -- halfway vector between the viewer and light-source vectors
        specularColor = ((max 0 (n `dot` h)) ** (shininess shape)) *^ li * (specular shape)

        -- total color
        c = ambient + lambertian + specularColor

intersectionPoint :: Intersection -> Color
intersectionPoint (Intersection d (Ray strt dir) _) = strt + ((*d) <$> dir)

minIntersect :: Ray -> (Maybe Intersection) -> Shape -> (Maybe Intersection)
minIntersect ray i shape = if d > 0 && (isNothing i || d < dist)
    then Just (Intersection d ray shape)
    else i
        where
            (Intersection dist _ _) = fromJust i
            d = intersect ray shape

intersect :: Ray -> Shape -> Double
intersect (Ray s dir) (Plane p n _ _ _ _) = if t <= 0 then 0 else t
    where
        t = ((p - s) `dot` n) / (dir `dot` n)

intersect (Ray s dir) (Sphere p r _ _ _ _) = if null roots then 0 else minimum roots
    where
        d = s - p
        roots = filter (> 10**(-6)) $ solveq (dir `dot` dir, 2 * dir `dot` d, d `dot` d - r * r)

solveq :: (Double, Double, Double) ->[Double]
solveq (a, b, c)
    | (d < 0) = []
    | (d > 0) = [(-b - sqrt d) / (2 * a), (-b + sqrt d) / (2 * a)]
    | otherwise = [-b / (2 * a)]
    where
        d = b * b - 4 * a * c
