{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Module      :  Data.ChainCodes
-- Copyright   :  (c) Mateusz Kowalczyk, 2014
-- License     :  GPL-3
--
-- Decoding of chain codes embedded in images.
module Data.ChainCodes where

import Codec.Picture
import Codec.Picture.Types
import Data.Function
import Control.Monad
import Data.Word

type BlobPixel = (Int, Int, Int)
type Colour = PixelRGB8
type PixelPos = (Int, Int)

-- | Reads in an 'Image' that uses 'PixelRGB8' as its base.
-- Rejects any other format.
readRGB8 ∷ FilePath → IO (Either String (Image PixelRGB8))
readRGB8 = readImage >=> return . \case
  Right (ImageRGB8 i) → Right i
  Right _ → Left "Unsuported image format. RGB8 images only please."
  Left err → Left err

-- | Generic application over 'DynamicImage'.
onImage ∷ (∀ a. Image a → b) → DynamicImage → b
onImage f d = case d of
  ImageY8 img -> f img
  ImageY16 img -> f img
  ImageYF img -> f img
  ImageYA8 img -> f img
  ImageYA16 img -> f img
  ImageRGB8 img -> f img
  ImageRGB16 img -> f img
  ImageRGBF img -> f img
  ImageRGBA8 img -> f img
  ImageRGBA16 img -> f img
  ImageYCbCr8 img -> f img
  ImageCMYK8 img -> f img
  ImageCMYK16 img -> f img

-- | Given an 'Image' parametrised by 'PixelRGB8' and given a
-- 'Colour', we try to find the first pixel that matches the 'Colour'.
--
-- We start checking at the top left corner of the image, checking each row
-- fully before progressing a column: we check @(width, height + 1)@ then
-- @(width, height + 1)@ and so on where top left corner of the image is (0, 0)
-- and positive height is towards the bottom.
findSpot ∷ Image PixelRGB8 → Colour → Maybe PixelPos
findSpot img@(Image w h d) c
  | w <= 0 || h <= 0 = Nothing
  | otherwise =
      go [ (wi, hi) | wi ← [0 .. w - 1], hi ← [0 .. h - 1] ]
      where
        go ∷ [PixelPos] → Maybe PixelPos
        go []     = Nothing
        go (x:xs) = if uncurry (pixelAt img) x == c
                    then Just x
                    else go xs
