{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.ChainCodes.ChainSpec (main, spec) where

import Data.ChainCodes
import Data.ChainCodes.Blobs
import Test.Hspec
import Codec.Picture

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseString" $ do
    let img = "test/res/a.gif"
        white = PixelRGB8 255 255 255
        red = PixelRGB8 255 0 0
        readImg x = readRGB8 x >>= \case
          Left _ → fail $ "Failed reading " ++ img
          Right s → return s

    it "can find a spot" $ do
      i ← readImg img
      findSpot i white `shouldBe`Just (0, 0)

    it "fails with Nothing when it can't find a spot" $ do
      i ← readImg img
      findSpot i red `shouldBe` Nothing

    it "can read in the correct chaincode" $ do
      i ← readImg img
      chainCodeWith i white (== white) `shouldBe` Just agifChain
