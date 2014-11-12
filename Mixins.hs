{-# OPTIONS_GHC -fno-warn-orphans #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Mixins
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Mixins where

import Prelude
import Data.Word8 (Word8)
import Data.Colour
import Data.Colour.SRGB
import Data.Text (pack)
import Text.Lucius hiding (Color)
import Text.Printf

type Color = AlphaColour Float

instance ToCss Color where
    toCss col = toCss . pack $ printf "rgba(%d,%d,%d,%.2f)" r g b (alphaChannel col)
        where RGB r g b = toSRGB24 (col `over` black)

rgba :: Word8 -> Word8 -> Word8 -> Float -> Color
rgba r g b = withOpacity (sRGB24 r g b)

bright03 :: Color -> Color
bright03 = blend 0.3 (rgba 255 255 255 1)

bright01 = blend 0.1 (rgba 255 255 255 1)

themeText :: Color
themeText = rgba 50 50 50 1

themeBackground :: Color
themeBackground = rgba 235 235 235 1

themeColors :: [Color]
themeColors =
    [ rgba 226 42  0   1 -- 0 red
    , rgba 140 0   255 1 -- 1 violet
    , rgba 200 200 0   1 -- 2 yellow
    , rgba 0   200 200 1 -- 3 cyan
    , rgba 200 200 200 1 -- 4 gray
    , rgba 40  250 0   1 -- 5 green
    ]

c, bg, dimc :: Int -> Color
c = (themeColors !!)
bg = dissolve 0.2 . c
dimc = darken 0.2 . c
opaq = dissolve 0.5

-- box-shadow

shadowRight :: String -> Mixin
shadowRight n = [luciusMixin|
    box-shadow:-#{n} 0 #{n} -#{n} gray inset;
|]

shadowTop :: String -> Mixin
shadowTop n = [luciusMixin|
    box-shadow:0 #{n} #{n} -#{n} gray inset;
|]

shadowTopNotInset :: String -> Mixin
shadowTopNotInset n = [luciusMixin|
    box-shadow:0 -#{n} #{n} -#{n} #bbb;
|]
