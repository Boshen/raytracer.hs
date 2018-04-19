module Object where

import Types

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

