module AsciiConverter.Lib
  ( Config (..),
    resizeImage,
    convertToAscii,
    resizeTerminal,
    deleteImages
  )
where

import AsciiConverter.Const (asciiCharactersMap, brightnessWeight)
import Data.List (map)
import Data.Vector.Storable (toList)
import Graphics.Image as I
import Graphics.Image.Interface (Array (toVector))
import Prelude
import System.Console.Terminal.Size
import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering, LineBuffering))
import System.Directory
import System.FilePath

data Config = Config
  { imageWidth :: Int,
    imageColor :: Bool
  }

resizeImage :: Array arr cs e => Int -> Image arr cs e -> Image arr cs e
resizeImage scaleWidth img = scale Bilinear Edge (scaleFactor, scaleFactor) img
  where
    currentWidth = I.cols img
    scaleFactor = fromIntegral scaleWidth / fromIntegral currentWidth :: Double

replacePixel :: Pixel RGB Double -> String
replacePixel (PixelRGB r g b) = character
  where
    red = r * 255 :: Double
    green = g * 255 :: Double
    blue = b * 255 :: Double
    i = floor $ (0.2126 * red + 0.7152 * green + 0.0722 * blue) * brightnessWeight :: Int
    c = rgbToAnsi(r,g,b)
    character = c ++ [asciiCharactersMap !! i]



rgbToAnsi :: (Double, Double, Double) -> String
rgbToAnsi (r, g, b)
    | r == 0 && g == 0 && b == 0 = "\x1b[30m"
    | r == 255 && g == 255 && b == 255 = "\x1b[37m"
    | r > g && r > b && (r - g) > 30 = "\x1b[31m"
    | g > r && g > b && (g - r) > 30 = "\x1b[32m"
    | b > r && b > g && (b - g) > 30 = "\x1b[34m"
    | r > g && g > b = "\x1b[33m"
    | g > r && r > b = "\x1b[36m"
    | r > b && b > g = "\x1b[35m"
    | otherwise = "\x1b[30m"

convertToAscii :: Image VS RGB Double -> Config -> IO String
convertToAscii img config = do
  let pixelsVector = toVector img
  let pixelsList = toList pixelsVector :: [Pixel RGB Double]
  let converted = Data.List.map replacePixel pixelsList
  let withLineBreaks = insertAtN (imageWidth config) "\n" converted
  return $ concat withLineBreaks

insertAtN :: Int -> a -> [a] -> [a]
insertAtN 0 _ xs = xs
insertAtN _ _ [] = []
insertAtN n y xs
  | length xs < n = xs
  | otherwise = take n xs ++ [y] ++ insertAtN n y (drop n xs)

resizeTerminal :: Int -> IO ()
resizeTerminal terminalWidth = do
  Just (Window h _) <- size
  let i = fromIntegral h :: Int
  hSetBuffering stdout NoBuffering
  putStr $ "\ESC[8;" ++ show i ++ ";" ++ show terminalWidth ++ "t"
  hSetBuffering stdout LineBuffering

deleteImages :: FilePath -> IO ()
deleteImages dir = do
  files <- listDirectory dir
  let images = filter (\f -> takeExtension f == ".jpg") files
  mapM_ (\f -> removeFile (dir </> f)) images
