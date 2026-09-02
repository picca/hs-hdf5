#include <bindings.h>
#include <hdf5.h>

-- |The POSIX unbuffered file driver using only the HDF5 public
-- API and with a few optimizations: the lseek() call is made
-- only when the current file position is unknown or needs to be
-- changed based on previous I/O through this driver (don't mix
-- I/O from this driver with I/O from other parts of the
-- application to the same file).
module Bindings.HDF5.Raw.H5FD.Sec2 where

import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)

import Bindings.HDF5.Raw.H5
import Bindings.HDF5.Raw.H5I

#if defined(H5FD_sec2_init)
# mangle_ident "H5FD_SEC2"
  :: HId_t
# mangle_ident "H5FD_SEC2"
    = unsafePerformIO (#mangle_ident "H5FD_sec2_init")

-- |Initialize this driver by registering the driver with the library.
--
-- > hid_t H5FD_sec2_init(void);
# ccall H5FD_sec2_init, IO <hid_t>
#endif

#if H5_VERSION_LE(1,8,18)
-- |Shut down the VFD.
--
-- > void H5FD_sec2_term(void);
#ccall H5FD_sec2_term, IO ()
#endif
 
-- |Modify the file access property list to use the H5FD_SEC2
-- driver.  There are no driver-specific properties.
--
-- > herr_t H5Pset_fapl_sec2(hid_t fapl_id);
#ccall H5Pset_fapl_sec2, <hid_t> -> IO <herr_t>

