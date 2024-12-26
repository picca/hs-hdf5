module Spec.Util
  ( withTestFile
  , withTestFile'
  , withGroup
  ) where

import           Control.Exception     (bracket)
import           System.IO
import           System.IO.Temp

import qualified Bindings.HDF5.File    as F
import qualified Bindings.HDF5.Group   as G
import qualified Data.ByteString.Char8 as BS

-- TODO : Use H5FD_CORE driver to avoid writing files : will require high-level wrapper for H5Pset_fapl_core

-- | Call a handle with a temporary HDF5 file
withTestFile :: (FilePath -> F.File -> IO a) -> IO a
withTestFile callback = withSystemTempFile "hs-hdf5.hdf" callback2 where
  callback2 path handle = bracket aquire release run where
    aquire = do
      hClose handle
      F.createFile (BS.pack path) [F.Truncate] Nothing Nothing
    run file = do callback path file
    release file = do F.closeFile file

withTestFile' :: (F.File -> IO a) -> IO a
withTestFile' callback = withTestFile (\_path file -> callback file)

withGroup :: BS.ByteString -> (F.File -> IO a) -> IO a
withGroup name f = withTestFile' $ \file -> do
  group <- G.createGroup file name Nothing Nothing Nothing
  G.closeGroup group
  f file
