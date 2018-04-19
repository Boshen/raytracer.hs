module Ray where

import Types

data Ray = Ray
    { start :: Vector
    , direction :: Vector
    }

data RayHit = RayHit
    { hitRay :: Ray
    , hitPoint :: Vector
    , hitNormal :: Vector
    , hitDist :: Double
    }

class Intersectable a where
    intersects :: Ray -> a -> Maybe RayHit
