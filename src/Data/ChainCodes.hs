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
import Data.Map hiding (map)

type Colour = PixelRGB8

-- | Position of a pixel along with the clock
-- direction used by 'chainCode'.
type PixelPos = (Int, Int, Int)

type Position = (Int, Int)


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
findSpot ∷ Image PixelRGB8 → Colour → Maybe Position
findSpot img@(Image w h d) c
  | w <= 0 || h <= 0 = Nothing
  | otherwise =
      go [ (wi, hi) | wi ← [0 .. w - 1], hi ← [0 .. h - 1] ]
      where
        go ∷ [(Int, Int)] → Maybe Position
        go []     = Nothing
        go (x:xs) = if uncurry (pixelAt img) x == c
                    then Just x
                    else go xs

-- | Given an 'Image' parametrised by 'PixelRGB8' and given a
-- 'Colour', we try to find the chain code in the binary image which has
-- the passed in colour.
--
-- Note that only a single shape is accepted inside of the image. The
-- starting positing is determined using 'findSpot'.
--
-- The output list contains unique positions only: the beginning and
-- end position are not treated the same. If 'findSpot' fails, we return
-- 'Nothing'.
chainCode ∷ Image PixelRGB8 → Colour → Maybe [Position]
chainCode img@(Image w h d) c = findSpot img c >>= \pos →
  let ppos = (fst pos, snd pos, 0)
  in Just $ go [0 ..] (fromList [(0, ppos)]) ppos
  where
    ns ∷ Map Int (Int, Int)
    ns = fromList $ zip [0 ..]
           [ (0, 1), (1, 1), (1, 0), (1, -1)
           , (0, -1), (-1, -1), (-1, 0), (-1, 1)]

    go ∷ [Int] → Map Int PixelPos → PixelPos → [Position]
    go (count:counts) positions p@(sx, sy, _) =
      map (dropThird . snd) . toList $ loop (count:counts) positions
      where
        dropThird ∷ (a, b, c) → (a, b)
        dropThird (x, y, _) = (x, y)

        loop ∷ [Int] → Map Int PixelPos → Map Int PixelPos
        loop (count:counts) positions
          | count == 0 || not (eqV positions count) =
              let current@(fx, fy, fd) = positions ! count
                  inBounds (x, y) = x >= 0 && y >= 0 && x < w && y < h
                  places = [ (mx, my, m)  | i ← [5 + fd .. fd + 13]
                                          , let m = i `mod` 8
                                                (nx, ny) = ns ! m
                                                o@(mx, my) = (fx + nx, fy + ny)
                                          , inBounds o
                                          , uncurry (pixelAt img) o == c
                                          ]
              in loop counts $ case places of
                [] → positions
                x:_ → insert (count + 1) x positions
          | otherwise = delete 0 positions

        eqV ∷ Map Int PixelPos → Int → Bool
        eqV p i = let (x, y, _) = p ! i
                  in (x, y) == (sx, sy)
