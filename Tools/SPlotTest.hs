{-# LANGUAGE OverloadedStrings #-}

module Tools.SPlotTest where

import qualified Data.ByteString.Char8 as S
import Data.Colour (Colour)
import Data.Colour.SRGB (sRGB24)
import qualified Data.Map as M
import System.Exit (ExitCode(..))
import System.IO (stderr)
import Test.HUnit hiding (counts)
import Tools.ColorMap (ColorMap1(..), augment, cycleColor1, defaultColorMap)

colorCyclingTests :: Test
colorCyclingTests = TestList
  [ cycleColor1 defaultColorMap "bar"   ~?= (base, colorMap "bar")
  , cycleColor1 (colorMap "foo") "bar" ~?= (nextColor, augmentedColorMap "bar")
  , cycleColor1 (colorMap "foo") "baz" ~?= (nextColor, augmentedColorMap "baz")
  ]
  where
    base, nextColor, nextNextColor :: Colour Double
    base                   = (sRGB24 128 128 128)
    nextColor              = sRGB24 (128+7) (128+17) (128+23)
    nextNextColor          = sRGB24 (128+7+7) (128+17+17) (128+23+23)
    colorsAssoc :: S.ByteString -> [(S.ByteString, Colour Double)]
    colorsAssoc       name = [(name,(sRGB24 128 128 128))]
    colorMap :: S.ByteString -> ColorMap1
    colorMap          name = ColorMap1 (M.fromList (colorsAssoc name)) [nextColor]
    augmentedColorMap :: S.ByteString -> ColorMap1
    augmentedColorMap name = augment (colorMap "foo") (name, nextNextColor) []

splotTests :: Test
splotTests = TestList [
  colorCyclingTests
  ]

runAllTests :: Test -> IO ExitCode
runAllTests tests = do
  counts <- runTest tests
  case (errors counts + failures counts) of
    0 -> return ExitSuccess
    n -> return (ExitFailure n)

runTest :: Test -> IO Counts
runTest  t = do
  (counts, _) <- runTestText (putTextToHandle stderr False) t
  return counts
