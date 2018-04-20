{-# LANGUAGE TypeOperators   #-}

module Lib (getImage, RGB8) where

import Control.Lens
import Control.Applicative
import Data.Foldable
import Data.Maybe
import Data.Ord
import Linear.Metric
import Linear.V3
import Linear.Vector
import Data.Array.Repa (Array, DIM2, D, Z (..), (:.)(..))
import qualified Data.Array.Repa as R
import Codec.Picture (Pixel8)

import Types
import Object
import Ray

type RGB8 = (Pixel8, Pixel8, Pixel8)

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

eye, lookat, uu, vv, ww :: Vector
eye = V3 0 (-100) 500
lookat = V3 0 0 (-50)
ww = normalize $ eye - lookat
vv = V3 0 1 0
uu = normalize $ vv `cross` ww

viewDistance :: Double
viewDistance = 400

objects :: [Object]
objects =
    [ Sphere (V3 0 50 0) 50 (Material 0.8 (V3 1 0 0) 0 0.2 20)
    , Sphere (V3 150 50 0) 50 (Material 0.8 (V3 0 1 0) 0 0.2 20)
    , Sphere (V3 (-100) 50 (-300)) 50 (Material 0.8 (V3 0 0 1) 0 0.2 20)
    , Plane  (V3 0 100 0) (V3 0 (-1) 0) (Material 0.5 (V3 0.5 0.5 0.5) 0.5 0 0)
    ]

lights :: [Light]
lights =
    [ AmbientLight 0.1 (V3 0.05 0.05 0.05)
    , DirectionalLight 1 (V3 1 1 1) (V3 1 (-1) 0)
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
getSample (x, y) (dx, dy) = snd $ trace 0 1 (V3 0 0 0) (Ray eye d)
    where
        x' = x + dx
        y' = y + dy
        d = normalize $ (x' *^ uu) + (y' *^ vv) - (viewDistance *^ ww)

trace :: Int -> Double -> Color -> Ray -> (Double, Color)
trace level coef clr ray = if notHit || coef <= 0 || level >= 15
    then if notHit && level == 0 then (0, V3 0 0 0) else (0, clr)
    else trace (level + 1) (coef * object^.material^.reflection) (clr + shadeColor) newRay
    where
        int = minIntersect ray objects
        notHit = isNothing int
        (object, rayHit) = fromJust int
        shadeColor = coef *^ (sum $ calcColor object rayHit <$> lights)

        n = rayHit^.hitNormal
        reflect = 2 * ((ray^.rayDirection) `dot` n)
        newRay = Ray (rayHit^.hitPoint) ((ray^.rayDirection) - (reflect *^ n))

calcColor :: Object -> RayHit -> Light -> Color
calcColor object _ (AmbientLight l_s c_l) = (k_d *^ c_d) * (l_s *^ c_l)
    where
        k_d = object^.material^.diffuseReflection
        c_d = object^.material^.diffuseColor

calcColor object (RayHit _ _ n _) (DirectionalLight l_s c_l l) =
    if n `dot` l > 0
    then (k_d *^ c_d ^/ 3.14) ^* (n `dot` l) * (l_s *^ c_l)
    else V3 0 0 0
    where
        k_d = object^.material.diffuseReflection
        c_d = object^.material.diffuseColor

calcColor object (RayHit (Ray s _) p n _) (PointLight l_s c_l lightPos) =
    if inShadow then V3 0 0 0 else c
    where
        k_d = object^.material^.diffuseReflection
        c_d = object^.material^.diffuseColor
        k_s = object^.material^.specularRefection
        e = object^.material^.shininess
        w = normalize $ p - s
        l = normalize $ p - lightPos -- light

        -- when the object is blocked by another object
        shadowRay = Ray (p + 0.001 *^ l) l
        inShadow = isJust $ minIntersect shadowRay (filter (/= object) objects)

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

minIntersect :: Intersectable a => Ray -> [a] -> Maybe (a, RayHit)
minIntersect ray os
    | null objects = Nothing
    | null hits = Nothing
    | otherwise = Just $ minimumBy (comparing $ view hitDistance . snd) hits
        where
            maybeHits = intersects ray <$> os
            hits = catMaybes $ zipWith (liftA2 (,) . Just) os maybeHits
