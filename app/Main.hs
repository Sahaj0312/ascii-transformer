module Main (main) where

import AsciiConverter.Lib (Config (Config, imageColor, imageWidth), convertToAscii, resizeImage)
import AsciiConverter.ExtractFrames (extractFrames, hasFFmpeg)
import Control.Monad.IO.Class
import Graphics.Image (readImage)
import System.Directory
import System.FilePath
import Prelude as P
import System.Exit (exitFailure)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  hasFFmpegFlag <- hasFFmpeg
  if not hasFFmpegFlag then do
    putStrLn "Please install FFmpeg to use this program"
    exitFailure
  else do
    let config = Config {imageWidth = 100, imageColor = False}
    -- image <- readImage "tigerLow.jpg"
    currentDir <- getCurrentDirectory
    let imageExtensions = [".jpg", ".jpeg", ".png"]
    let checkForImages = do
          files <- listDirectory currentDir
          let imageFiles = filter (\file -> takeExtension file `elem` imageExtensions) files
          if null imageFiles
            then do
              putStrLn "No image files found. Waiting for images to be added..."
              threadDelay 1000000 -- Wait for 1 second before checking again
              checkForImages
            else do
              putStrLn $ "Found " ++ show (length imageFiles) ++ " image file(s). Converting to ASCII art..."
              mapM_
                (\file -> do
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

    files <- listDirectory currentDir
    let videoFiles = filter (\file -> takeExtension file == ".mp4") files
    case videoFiles of
      [videoFile] -> do
        putStrLn ("Extracting frames from: " ++ videoFile)
        extractFrames videoFile
        checkForImages
      _ -> checkForImages
