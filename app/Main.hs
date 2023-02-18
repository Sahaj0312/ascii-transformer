module Main (main) where

import AsciiConverter.Lib (Config (Config, imageColor, imageWidth), convertToAscii, resizeImage)
import Control.Monad.IO.Class
import Graphics.Image (readImage)
import System.Directory
import System.FilePath
import Prelude as P

main :: IO ()
main = do
  let config = Config {imageWidth = 100, imageColor = False}
  -- image <- readImage "tigerLow.jpg"
  currentDir <- getCurrentDirectory
  files <- listDirectory currentDir
  let imageFiles = filter (\file -> takeExtension file `elem` [".jpg", ".jpeg", ".png"]) files

  mapM_
    ( \file -> do
        putStrLn $ "Loading image file: " ++ file
        image <- readImage file
        case image of
          Left _ -> putStrLn "Couldn't read the image"
          Right img -> do
            let resizedImg = resizeImage (imageWidth config) img
            converted <- liftIO $ convertToAscii resizedImg config
            putStrLn converted
    )
    imageFiles

{-case image of
  Left _ -> putStrLn "Couldn't read the image"
  Right img -> do
    let resizedImg = resizeImage (imageWidth config) img
    converted <- liftIO $ convertToAscii resizedImg config
    putStrLn converted
    -}

-- imgs n =
--   do
--     currentDir <- getCurrentDirectory
--     files <- listDirectory currentDir
--     let imageFiles = filter (\file -> takeExtension file `elem` [".jpg", ".jpeg", ".png"]) files

--     mapM_
--       ( \file -> do
--           putStrLn $ "Loading image file: " ++ file
--           mbImage <- readImageRGB VU file
--           displayImage mbImage
--       )
--       imageFiles
--     return n