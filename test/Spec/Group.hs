{-# LANGUAGE OverloadedStrings #-}

module Spec.Group ( describeGroup ) where

import           Test.Hspec

import           Bindings.HDF5.Core    (IndexType (..), IterOrder (..))
import qualified Bindings.HDF5.Link    as L

import           Spec.Util

-- | Describe the H5G Group interface
describeGroup :: Spec
describeGroup = do
  around (withGroup "group1") $ do
    it "can create group at top-level" $ \file -> do
      name <- L.getLinkNameByIdx file "/" ByName Increasing 0 Nothing
      name `shouldBe` "group1"

  -- TODO : test lookup non-existent group
