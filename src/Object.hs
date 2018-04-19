module Object where

import Linear.Metric
import Linear.Vector

import Types
import Ray

data Material = Material
    { diffuseReflection :: Double -- k_d in [0, 1], == ka, ambient reflection coefficient
    , diffuseColor :: Color -- c_d; rho_d = k_d * c_d
    , reflection :: Double -- [0, 1]
    , specularRefection :: Double -- k_s [0, 1]
    , shininess :: Double -- [0, inf)
    } deriving (Eq)

data Object =
    Plane
    { position :: Vector
    , planeNormal :: Vector
    , material :: Material
    }
    | Sphere
    { position :: Vector
    , radius :: Double
    , material :: Material
    } deriving (Eq)

instance Intersectable Object where
    intersects ray@(Ray s dir) (Plane p n _)
        | dist <= 0 = Nothing
        | otherwise = Just $ RayHit ray hp n dist
        where
            dist = ((p - s) `dot` n) / (dir `dot` n)
            hp = s + dist *^ dir

    intersects ray@(Ray s dir) (Sphere p r _)
        | null roots = Nothing
        | otherwise = Just $ RayHit ray hp n dist
        where
            d = s - p -- discriminant
            roots = filter (> 10**(-6)) $ solveq (dir `dot` dir, 2 * dir `dot` d, d `dot` d - r * r)
            dist = minimum roots
            hp = s + dist *^ dir
            n = normalize $ hp - p

solveq :: (Double, Double, Double) ->[Double]
solveq (a, b, c)
    | (d < 0) = []
    | (d > 0) = [(-b - sqrt d) / (2 * a), (-b + sqrt d) / (2 * a)]
    | otherwise = [-b / (2 * a)]
    where
        d = b * b - 4 * a * c
