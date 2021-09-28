-- | Color cycling routines.
-- This module provides functions for maintaining a map from arbitrary strings to colors and
-- generating new colors for unknown names by cycling over the RGB spectrum.
--
-- P.S. We use the spelling "color" in our identifiers for no particular reason
-- other than internal consistency.
module Tools.ColorMap (
  ColorMap(ColorMap),
  ColorMap1(ColorMap1),
  prepareColorMap,
  cycleColor,
  cycleColor1,
  computeColor,
  augment,
  defaultColorMap)
where

import qualified Data.ByteString.Char8 as S
import Data.Colour (Colour)
import qualified Data.Colour.Names as C
import Data.Colour.SRGB (RGB(RGB), sRGB24, sRGB24read, toSRGB24)
import qualified Data.Map as M
import Data.Maybe (fromJust)

-- | Color scheme id -> Color map
newtype ColorMap = ColorMap (M.Map S.ByteString ColorMap1)

data ColorMap1 = ColorMap1 {
  colorMap :: M.Map S.ByteString (Colour Double), -- ^ Current map from arbitrary strings to color descriptions
  colorWheel :: [Colour Double]             -- ^ Next colors for assigning to as yet unknown names
  } deriving (Eq, Show)

prepareColorMap :: [(S.ByteString, [S.ByteString])] -> ColorMap
prepareColorMap ms = ColorMap $ M.fromList $ (S.pack "", defaultColorMap):
  [ (scheme, ColorMap1 M.empty $ cycle $ map (fromJust . readColor . S.unpack) colorNames)
  | (scheme, colorNames) <- ms
  ]

-- | Starts with empty names-colors map and mid-range grey
defaultColorMap :: ColorMap1
defaultColorMap = ColorMap1 M.empty defaultColorWheel

defaultColorWheel :: [Colour Double]
defaultColorWheel =
  [C.green, C.blue, C.red, C.brown, C.orange, C.magenta, C.grey, C.purple, C.violet, C.lightblue, C.crimson, C.burlywood]
    ++ map nextColor defaultColorWheel

-- | Compute color for a given name within the associated map.
-- This function encapsulates the following rules:
--
--  * If @color@ is a 6-digit hexadecimal value of the form '#FA34B7' then this is used as an immediate
--    RGB color,
--
--  * If @color@ is a color name from SVG1.1 specification (http://www.w3.org/TR/SVG11/types.html#ColorKeywords)
--    then the corresponding color is returned,
--
--  * Otherwise, the @color@ name is looked up in the @map@ and if it is not found, a new color is generated
--    using a simple cycling function.
computeColor :: ColorMap -> S.ByteString -> (Colour Double, ColorMap)
computeColor cmap color = case readColor (S.unpack color) of
  Nothing -> cycleColor cmap color
  Just c  -> (c, cmap)

readColor :: String -> Maybe (Colour Double)
readColor cs@('#':_) = Just (sRGB24read cs)
readColor cs         = C.readColourName cs

cycleColor :: ColorMap -> S.ByteString -> (Colour Double, ColorMap)
cycleColor (ColorMap cmap) name = case M.lookup scheme cmap of
    Nothing -> cycleColor (ColorMap cmap) S.empty -- Use default color scheme then.
    Just m  -> let (res, m') = cycleColor1 m subColor in (res, ColorMap $ M.insert scheme m' cmap)
  where
    (scheme, subColor) = case S.uncons name of
      -- /scheme/color
      Just ('/', name') -> S.break (=='/') name'
      _                 -> (S.empty, name)

-- | Compute the color associated to a given name, providing an updated map with
-- possibly new colors in the cycle.
cycleColor1 :: ColorMap1 -> S.ByteString -> (Colour Double, ColorMap1)
cycleColor1 cmap name = case M.lookup name (colorMap cmap) of
  Just c  -> (c, cmap)
  Nothing -> (next, augment cmap (name,next) wheel')
  where
    (next:wheel') = colorWheel cmap

nextColor :: Colour Double -> Colour Double
nextColor col = let RGB r g b = toSRGB24 col in sRGB24 (r+7) (g+17) (b+23)

augment :: ColorMap1 -> (S.ByteString, Colour Double) -> [Colour Double] -> ColorMap1
augment cmap (name,col) wheel =
  ColorMap1 (M.insert name col (colorMap cmap)) wheel
