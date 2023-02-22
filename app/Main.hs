import AsciiConverter.Lib (Config (Config, imageColor, imageWidth), convertToAscii, resizeImage, resizeTerminal, deleteImages, extractImageIndex)
import AsciiConverter.ExtractFrames (extractFrames, hasFFmpeg)
import Control.Monad.IO.Class
import Graphics.Image (readImage, rows, cols)
import System.Directory
import System.FilePath
import Prelude as P
import System.Exit (exitFailure)
import Control.Concurrent (threadDelay)
import Data.List (sortBy)
import Data.Ord (comparing)

main :: IO ()
main = do
  hasFFmpegFlag <- hasFFmpeg
  if not hasFFmpegFlag then do
    putStrLn "Please install FFmpeg to use this program"
    exitFailure
  else do
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
              let sortedImageFiles = sortBy (comparing extractImageIndex) imageFiles
              putStrLn $ "Found " ++ show (length imageFiles) ++ " image file(s). Converting to ASCII art..."
              mapM_ (\file -> do
                image <- readImage file
                case image of
                  Left _ -> do
                    putStrLn "Couldn't read the image"
                  Right img -> do
                    let maxImageWidth = if rows img > 58 then (58 * cols img) `div` rows img else 100
                    let config = Config {imageWidth = maxImageWidth, imageColor = False}
                    let resizedImg = resizeImage (imageWidth config) img
                    resizeTerminal (imageWidth config)
                    liftIO $ convertToAscii resizedImg config
                ) sortedImageFiles

    files <- listDirectory currentDir
    let videoFiles = filter (\file -> takeExtension file == ".mp4") files
    case videoFiles of
      [videoFile] -> do
        putStrLn ("Extracting frames from: " ++ videoFile)
        extractFrames videoFile
        checkForImages
        deleteImages currentDir
      _ -> checkForImages
