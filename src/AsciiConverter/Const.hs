module AsciiConverter.Const
  ( asciiCharactersMap,
    maxBrightness,
    brightnessWeight,
    asciiCharactersMapRev,
  )
where

asciiCharactersMap :: String
asciiCharactersMap = " .°*oO#@)"

asciiCharactersMapRev :: String
asciiCharactersMapRev = ")@#Oo*°. "

maxBrightness :: Int
maxBrightness = 255

-- Have to adjust the mapping function based on the ascii characters used
brightnessWeight :: Double
brightnessWeight = fromIntegral (length asciiCharactersMap) / fromIntegral maxBrightness