#include <bindings.h>
#include <hdf5.h>

module Bindings.HDF5.Raw.H5FD.StdIO where

import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)

import Bindings.HDF5.Raw.H5
import Bindings.HDF5.Raw.H5I

#if defined(H5FD_stdio_init)
# mangle_ident "H5FD_STDIO"
  :: HId_t
# mangle_ident "H5FD_STDIO"
    = unsafePerformIO (#mangle_ident "H5FD_stdio_init")

-- |Initialize this driver by registering the driver with the library.
--
-- > hid_t H5FD_stdio_init(void);
# ccall H5FD_stdio_init, IO <hid_t>
#endif

#if H5_VERSION_LE(1,8,18)

-- |Shut down the VFD.
--
-- > void H5FD_stdio_term(void);
#ccall H5FD_stdio_term, IO ()

#endif

-- |Modify the file access property list to use the H5FD_STDIO
-- driver.  There are no driver-specific properties.
--
-- herr_t H5Pset_fapl_stdio(hid_t fapl_id);
#ccall H5Pset_fapl_stdio, <hid_t> -> IO <herr_t>

