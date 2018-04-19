{-# LANGUAGE TypeOperators   #-}

module Lib (getImage, RGB8) where

import Data.Maybe
import Linear.Metric
import Linear.V3
import Linear.Vector
import Data.Array.Repa (Array, DIM2, D, Z (..), (:.)(..))
import qualified Data.Array.Repa as R
import Codec.Picture (Pixel8)

type RGB8 = (Pixel8, Pixel8, Pixel8)
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
    , diffuseReflection :: Double
    , diffuseColor :: Color
    , reflection :: Double
    , specularRefection :: Double -- k_s [0, 1]
    , shininess :: Double
    }
    | Sphere
    { position :: Vector
    , radius :: Double
    , diffuseReflection :: Double -- k_d in [0, 1], == ka, ambient reflection coefficient
    , diffuseColor :: Color -- c_d; rho_d = k_d * c_d
    , reflection :: Double -- [0, 1]
    , specularRefection :: Double -- k_s [0, 1]
    , shininess :: Double -- [0, inf)
    } deriving (Eq)

data Light =
    AmbientLight
    { radiance :: Double -- [0, inf)
    , lightColor :: Color -- c_l
    }
    | DirectionalLight
    { radiance :: Double -- l_s
    , lightColor :: Color -- c_l
    , lightDirection :: Vector
    }
    | PointLight
    { radiance :: Double
    , lightColor :: Color
    , lightLocation :: Vector
    }

-- distance of intersection, between shape and ray
data Intersection = Intersection Double Ray Shape

eye, lookat, uu, vv, ww :: Vector
eye = V3 0 (-100) 500
lookat = V3 0 0 (-50)
ww = normalize $ eye - lookat
vv = V3 0 1 0
uu = normalize $ vv `cross` ww

viewDistance :: Double
viewDistance = 400

shapes :: [Shape]
shapes =
    [ Sphere (V3 0 50 0) 50 0.8 (V3 1 0 0) 0.2 0.2 20
    , Sphere (V3 150 50 0) 50 0.8 (V3 0 1 0) 0.2 0.2 20
    , Sphere (V3 (-100) 50 (-300)) 50 0.8 (V3 0 0 1) 0.2 0.2 20
    , Plane  (V3 0 100 0) vv 0.8 (V3 1 1 1) 0.5 0.5 1
    ]

lights :: [Light]
lights =
    [ AmbientLight 0.1 (V3 0.05 0.05 0.05)
    , DirectionalLight 2 (V3 1 1 1) (V3 1 (-1) 0)
    , PointLight 3 (V3 1 1 1) (V3 (100) (500) (-200))
    ]

getImage :: Int -> Int -> Array D DIM2 RGB8
getImage w h = R.fromFunction (Z :. w :. h) $ getPixel w h

getPixel :: Int -> Int -> (Z :. Int :. Int) -> RGB8
getPixel w h (Z :. j :. i) = (r, g, b)
    where
        (i', j') = (fromIntegral i, fromIntegral j)
        (w', h') = (fromIntegral w, fromIntegral h)
        x = j' - h' / 2.0
        y = i' - w' / 2.0
        n = 5 -- sample points for anti-aliasing
        samples = [((x' + 0.5) / n, (y' + 0.5) / n) | x' <- [0..n-1], y' <- [0..n-1]]
        colors = getSample (x, y) <$> samples
        (V3 r g b) = max 0 . round . min 255 . (*255) . (/(n * n)) <$> sum colors

getSample :: (Double, Double) -> (Double, Double) -> Color
getSample (x, y) (dx, dy) = snd $ trace 0 1 (V3 0 0 0) (Ray eye dir)
    where
        x' = x + dx
        y' = y + dy
        dir = normalize $ (x' *^ uu) + (y' *^ vv) - (viewDistance *^ ww)

trace :: Int -> Double -> Color -> Ray -> (Double, Color)
trace level coef clr ray = if notHit || coef <= 0 || level >= 15
    then if notHit && level == 0 then (0, V3 0 0 0) else (0, clr)
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
calcColor (Intersection _ _ shape) (AmbientLight l_s c_l) = (k_d *^ c_d) * (l_s *^ c_l)
    where
        k_d = diffuseReflection shape
        c_d = diffuseColor shape

calcColor i@(Intersection _ _ shape) (DirectionalLight l_s c_l l) =
    if n `dot` l > 0
    then (k_d *^ c_d ^/ 3.14) ^* (n `dot` l) * (l_s *^ c_l)
    else V3 0 0 0
    where
        p = intersectionPoint i -- point
        k_d = diffuseReflection shape
        c_d = diffuseColor shape
        n = case shape of
            (Plane _ normal _ _ _ _ _) -> normal
            (Sphere pos _ _ _ _ _ _) -> normalize $ p - pos -- normal

calcColor i@(Intersection _ (Ray s _) shape) (PointLight l_s c_l lightPos) =
    if inShadow then V3 0 0 0 else c
    where
        p = intersectionPoint i -- point
        k_d = diffuseReflection shape
        c_d = diffuseColor shape
        k_s = specularRefection shape
        e = shininess shape
        w = normalize $ p - s
        l = normalize $ p - lightPos -- light
        n = case shape of
            (Plane _ normal _ _ _ _ _) -> normal
            (Sphere pos _ _ _ _ _ _) -> normalize $ p - pos-- normal

        -- when the object is blocked by another object
        shadowRay = Ray (p + 0.001 *^ l) l
        inShadow = isJust $ foldl (minIntersect shadowRay) Nothing (filter (/= shape) shapes)

        -- lambertian reflection is often used as a model for diffuse reflection
        -- object's from the real world reflect on average around 18% of the light they receive.
        lambertian = if n `dot` l > 0
            then (k_d *^ c_d ^/ 3.14) ^* (n `dot` l) * (l_s *^ c_l)
            else V3 0 0 0

        -- Phong shading model
        -- h = normalize $ l + v -- halfway vector between the viewer and light-source vectors
        r = (-1) *^ l + 2 * (n `dot` l) *^ n
        spec = if n `dot` l > 0
            then k_s * ((r `dot` w) ** e) * (n `dot` l) *^ (l_s *^ c_l)
            else V3 0 0 0

        -- total color
        c = lambertian + spec

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
intersect (Ray s dir) (Plane p n _ _ _ _ _) = if t <= 0 then 0 else t
    where
        t = ((p - s) `dot` n) / (dir `dot` n)

intersect (Ray s dir) (Sphere p r _ _ _ _ _) = if null roots then 0 else minimum roots
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
