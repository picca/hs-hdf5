#include <bindings.h>
#include <hdf5.h>

-- |A driver which stores the HDF5 data in main memory using
-- only the HDF5 public API. This driver is useful for fast
-- access to small, temporary hdf5 files.
module Bindings.HDF5.Raw.H5FD.Core where

import Foreign.C.Types
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)

import Bindings.HDF5.Raw.H5
import Bindings.HDF5.Raw.H5I

import Foreign.Ptr.Conventions

#if defined(H5FD_core_init)
# mangle_ident "H5FD_CORE"
  :: HId_t
# mangle_ident "H5FD_CORE"
    = unsafePerformIO (#mangle_ident "H5FD_core_init")

-- |Initialize this driver by registering the driver with the library.
--
-- > hid_t H5FD_core_init(void);x
# ccall H5FD_core_init, IO <hid_t>
#endif

#if H5_VERSION_LE(1,8,18)
-- |Shut down the VFD.
--
-- > void H5FD_core_term(void);
#ccall H5FD_core_term, IO ()
#endif

-- |Modify the file access property list to use the H5FD_CORE
-- driver.  The 'increment' specifies how much to grow the memory
-- each time we need more.
--
-- > herr_t H5Pset_fapl_core(hid_t fapl_id, size_t increment,
-- >        hbool_t backing_store);
#ccall H5Pset_fapl_core, <hid_t> -> <size_t> -> <hbool_t> -> IO <herr_t>

-- |Queries properties set by the H5Pset_fapl_core() function.
--
-- > herr_t H5Pget_fapl_core(hid_t fapl_id, size_t *increment/*out*/,
-- >        hbool_t *backing_store/*out*/);
#ccall H5Pget_fapl_core, <hid_t> -> Out <size_t> -> Out <hbool_t> -> IO <herr_t>

