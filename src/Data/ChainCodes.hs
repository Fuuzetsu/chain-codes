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

-- | Friendly alias for 'PixelRGB8'
type Colour = PixelRGB8

-- | Position of a pixel along with the clock
-- direction used by 'chainCode'.
type PixelPos = (Int, Int, Int)

-- | Simple alias for 'Int' pair
type Position = (Int, Int)

-- | In a chain code, we'll keep the original pixel positions as well as
-- the direction which we can use for signal processing.
type ChainCode = [PixelPos]

-- | Reads in an 'Image' that uses 'PixelRGB8' as its base.
-- Rejects any other format.
readRGB8 ∷ FilePath → IO (Either String (Image PixelRGB8))
readRGB8 = readImage >=> return . \case
  Right (ImageRGB8 i) → Right i
  Right _ → Left "Unsuported image format. RGB8 images only please."
  Left err → Left err

-- | Given an 'Image' parametrised by 'PixelRGB8' and given a
-- 'Colour', we try to find the first pixel that matches the 'Colour'.
--
-- We start checking at the top left corner of the image, checking each row
-- fully before progressing a column: we check @(width, height)@ then
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
-- the passed in colour. Note that this is the colour of your shape and any
-- other colour is assumed to be the background: to process a black shape, pass
-- in a black colour.
--
-- Note that only a single shape is accepted inside of the image. The
-- starting positing is determined using 'findSpot'.
--
-- The output list contains unique positions only: the beginning and
-- end position are not treated the same. If 'findSpot' fails, we return
-- 'Nothing'.
chainCode ∷ Image PixelRGB8 → Colour → Maybe ChainCode
chainCode img@(Image w h d) c = findSpot img c >>= \pos →
  let ppos = (fst pos, snd pos, 0)
  in Just $ go [0 ..] (fromList [(0, ppos)]) ppos
  where
    ns ∷ Map Int (Int, Int)
    ns = fromList $ zip [0 ..]
           [ (0, 1), (1, 1), (1, 0), (1, -1)
           , (0, -1), (-1, -1), (-1, 0), (-1, 1)]

    go ∷ [Int] → Map Int PixelPos → PixelPos → ChainCode
    go (count:counts) positions p@(sx, sy, _) =
      map snd . toList $ loop (count:counts) positions
      where
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
