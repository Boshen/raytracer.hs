module Main where

import Codec.Picture
import Data.Array.Repa

import Lib

toImage :: Int -> Int -> Array U DIM2 RGB8 -> Image PixelRGB8
toImage w h a = generateImage gen w h
  where
    gen x y =
        let (r, g, b) = a ! (Z :. x :. y)
        in PixelRGB8 r g b

main :: IO ()
main = do
    (w, h) <- return (500, 500)
    img <- computeUnboxedP $ getImage w h
    savePngImage "test.png" . ImageRGB8 . (toImage w h) $ img
