module AsciiConverter.Const
  ( asciiCharactersMap,
    maxBrightness,
    brightnessWeight,
  )
where

asciiCharactersMap :: String
asciiCharactersMap = " .°*oO#@)"

maxBrightness :: Int
maxBrightness = 255

brightnessWeight :: Double
brightnessWeight = fromIntegral (length asciiCharactersMap) / fromIntegral maxBrightness