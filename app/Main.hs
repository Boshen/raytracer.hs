module Main where

import Vision.Image.Storage.DevIL (Autodetect (..), save)

import Lib

main :: IO ()
main = do
    mErr <- save Autodetect "test.png" $ getImage
    case mErr of
        Nothing  ->
            putStrLn "Success."
        Just err -> do
            putStrLn "Unable to save the image:"
            print err
