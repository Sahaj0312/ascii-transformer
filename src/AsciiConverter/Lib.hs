module AsciiConverter.Lib
  ( Config (..),
    resizeImage,
    convertToAscii,
    resizeTerminal,
    deleteImages,
    extractImageIndex
  )
where

import AsciiConverter.Const (asciiCharactersMap, brightnessWeight, asciiCharactersMapRev)
import Data.List (map)
import Data.Vector.Storable (toList)
import Graphics.Image as I
import Graphics.Image.Interface (Array (toVector))
import Prelude
import System.Console.Terminal.Size
import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering, LineBuffering))
import System.Directory
import System.FilePath
import Control.Concurrent (threadDelay)
import Data.Char (isDigit)
import Text.Read (readMaybe)


data Config = Config
  { imageWidth :: Int,
    imageColor :: Bool
  }

resizeImage :: Array arr cs e => Int -> Image arr cs e -> Image arr cs e
resizeImage scaleWidth img = scale Bilinear Edge (scaleFactor, scaleFactor) img
  where
    currentWidth = I.cols img
    scaleFactor = fromIntegral scaleWidth / fromIntegral currentWidth :: Double

replacePixel :: Pixel RGB Double -> Double -> String
replacePixel (PixelRGB r g b) brightness = character
  where
    list = if brightness > 102.00
             then asciiCharactersMap
             else asciiCharactersMapRev
    red = r * 255 :: Double
    green = g * 255 :: Double
    blue = b * 255 :: Double
    i = floor $ (0.2126 * red + 0.7152 * green + 0.0722 * blue) * brightnessWeight :: Int
    c = rgbToAnsi(r,g,b)
    character = c ++ [list !! i]

calcBrightness :: Pixel RGB Double -> Double
calcBrightness (PixelRGB r g b) = (r * g * b) / 3


rgbToAnsi :: (Double, Double, Double) -> String
rgbToAnsi (r, g, b)
    | r == 0 && g == 0 && b == 0 = "\ESC[90m"
    | r == 255 && g == 255 && b == 255 = "\x1b[97m"
    | r > g && r > b && (r - g) > 30 = "\x1b[91m"
    | g > r && g > b && (g - r) > 30 = "\x1b[92m"
    | b > r && b > g && (b - g) > 30 = "\x1b[94m"
    | r > g && g > b = "\x1b[93m"
    | g > r && r > b = "\x1b[96m"
    | r > b && b > g = "\x1b[95m"
    | otherwise = "\ESC[90m"

convertToAscii :: Image VS RGB Double -> Config -> IO ()
convertToAscii img config = do
  let pixelsVector = toVector img
  let pixelsList = toList pixelsVector :: [Pixel RGB Double]
  let brightness = Data.List.map calcBrightness pixelsList
  --putStrLn "The brightness of this frame is:"
  --putStrLn $ show (Prelude.sum(brightness)/fromIntegral(length brightness))
  let converted = Data.List.map (\x -> replacePixel x (Prelude.sum(brightness)/fromIntegral(length brightness))) pixelsList 
  let withLineBreaks = insertAtN (imageWidth config) "\n" converted
  putStrLn $ concat withLineBreaks
  threadDelay 90000 -- TODO: find optimal value that works for most videos

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

extractImageIndex :: FilePath -> Int
extractImageIndex path =
  case readMaybe indexStr of
    Just index -> index
    Nothing -> error $ "Invalid image index: " ++ indexStr
  where indexStr = takeWhile isDigit $ reverse $ takeWhile (/= '-') $ reverse path
