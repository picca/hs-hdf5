#include <bindings.h>
#include <H5Fpublic.h>

module Bindings.HDF5.Raw.H5F where

import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

import Bindings.HDF5.Raw.H5
import Bindings.HDF5.Raw.H5AC
import Bindings.HDF5.Raw.H5I

import Foreign.Ptr.Conventions

-- * Types and constants

-- ** Flags for 'h5f_create' and 'h5f_open'
-- These are the bits that can be passed to the 'flags' argument of
-- H5Fcreate() and H5Fopen(). Use the bit-wise OR operator (|) to combine
-- them as needed.  As a side effect, they call H5check_version() to make sure
-- that the application is compiled with a version of the hdf5 header files
-- which are compatible with the library to which the application is linked.
-- We're assuming that these constants are used rather early in the hdf5
-- session.

-- |absence of rdwr => rd-only
#num H5F_ACC_RDONLY

-- |open for read and write
#num H5F_ACC_RDWR

-- |overwrite existing files
#num H5F_ACC_TRUNC

-- |fail if file already exists
#num H5F_ACC_EXCL

-- |print debug info
#num H5F_ACC_DEBUG

-- |create non-existing files
#num H5F_ACC_CREAT

-- |Value passed to 'h5p_set_elink_acc_flags' to cause flags to be taken from the
-- parent file.
#num H5F_ACC_DEFAULT

-- ** Flags for 'h5f_get_obj_count' and 'h5f_get_obj_ids' calls

-- |File objects
#num H5F_OBJ_FILE

-- |Dataset objects
#num H5F_OBJ_DATASET

-- |Group objects
#num H5F_OBJ_GROUP

-- |Named datatype objects
#num H5F_OBJ_DATATYPE

-- |Attribute objects
#num H5F_OBJ_ATTR

#num H5F_OBJ_ALL

-- |Restrict search to objects opened through current file ID
#num H5F_OBJ_LOCAL

#newtype_const hsize_t, H5F_FAMILY_DEFAULT

#ifdef H5_HAVE_PARALLEL

-- |Use this constant string as the MPI_Info key to set 'h5f_mpio' debug flags.
-- To turn on 'h5f_mpio' debug flags, set the MPI_Info value with this key to
-- have the value of a string consisting of the characters that turn on the
-- desired flags.
#str H5F_MPIO_DEBUG_KEY

#endif /* H5_HAVE_PARALLEL */

-- |The difference between a single file and a set of mounted files
#newtype H5F_scope_t

-- |specified file handle only
#newtype_const H5F_scope_t, H5F_SCOPE_LOCAL

-- |entire virtual file
#newtype_const H5F_scope_t, H5F_SCOPE_GLOBAL

-- |Unlimited file size for 'h5p_set_external'
#newtype_const hsize_t, H5F_UNLIMITED

-- |How does file close behave?
#newtype H5F_close_degree_t, Eq

-- |Use the degree pre-defined by underlining VFL
#newtype_const H5F_close_degree_t, H5F_CLOSE_DEFAULT

-- |file closes only after all opened objects are closed
#newtype_const H5F_close_degree_t, H5F_CLOSE_WEAK

-- |if no opened objects, file is close; otherwise, file close fails
#newtype_const H5F_close_degree_t, H5F_CLOSE_SEMI

-- |if there are opened objects, close them first, then close file
#newtype_const H5F_close_degree_t, H5F_CLOSE_STRONG


-- |Types of allocation requests. The values larger than 'h5fd_MEM_DEFAULT'
-- should not change other than adding new types to the end. These numbers
-- might appear in files.
#newtype H5F_mem_t, Eq
-- |Data should not appear in the free list.
-- Must be negative.
#newtype_const H5F_mem_t, H5FD_MEM_NOLIST
-- |Value not yet set.  Can also be the
-- datatype set in a larger allocation
-- that will be suballocated by the library.
-- Must be zero.
#newtype_const H5F_mem_t, H5FD_MEM_DEFAULT
-- |Superblock data
#newtype_const H5F_mem_t, H5FD_MEM_SUPER
-- |B-tree data
#newtype_const H5F_mem_t, H5FD_MEM_BTREE
-- |Raw data (content of datasets, etc.)
#newtype_const H5F_mem_t, H5FD_MEM_DRAW
-- |Global heap data
#newtype_const H5F_mem_t, H5FD_MEM_GHEAP
-- |Local heap data
#newtype_const H5F_mem_t, H5FD_MEM_LHEAP
-- |Object header data
#newtype_const H5F_mem_t, H5FD_MEM_OHDR
-- |Sentinel value - must be last
#num H5FD_MEM_NTYPES


-- |Library's file format versions
#newtype H5F_libver_t

-- |Use the earliest possible format for storing objects
#newtype_const H5F_libver_t, H5F_LIBVER_EARLIEST

-- |Use the latest possible format available for storing objects
#newtype_const H5F_libver_t, H5F_LIBVER_LATEST

-- * Public functions

-- |Check the file signature to detect an HDF5 file.
--
-- [Bugs:] This function is not robust: it only uses the default file
--         driver when attempting to open the file when in fact it
--         should use all known file drivers.
--
-- > htri_t H5Fis_hdf5(const char *filename);
#ccall H5Fis_hdf5, CString -> IO <htri_t>

-- |This is the primary function for creating HDF5 files. The
-- 'flags' parameter determines whether an existing file will be
-- overwritten or not.  All newly created files are opened for
-- both reading and writing.  All flags may be combined with the
-- bit-wise OR operator (@ .|. @ from "Data.Bits") to change the
-- behavior of the file create call.
--
-- The more complex behaviors of a file's creation and access
-- are controlled through the file-creation and file-access
-- property lists.  The value of 'h5p_DEFAULT' for a template
-- value indicates that the library should use the default
-- values for the appropriate template.
--
-- See also: "Bindings.HDF5.Raw.H5F" for the list of supported flags.
-- "Bindings.HDF5.Raw.H5P" for the list of file creation and file
-- access properties.
--
-- On success, returns a file ID.  On failure, returns a negative value.
--
-- > hid_t  H5Fcreate(const char *filename, unsigned flags,
-- >        hid_t create_plist, hid_t access_plist);
#ccall H5Fcreate, CString -> CUInt -> <hid_t> -> <hid_t> -> IO <hid_t>

-- |This is the primary function for accessing existing HDF5
-- files.  The 'flags' argument determines whether writing to an
-- existing file will be allowed or not.  All flags may be
-- combined with the bit-wise OR operator (@ .|. @ from "Data.Bits")
-- to change the behavior of the file open call.  The more complex
-- behaviors of a file's access are controlled through the file-access
-- property list.
--
-- See Also: "Bindings.HDF5.Raw.H5F" for a list of possible values for 'flags'.
--
-- On success, returns a file ID.  On failure, returns a negative value.
--
-- > hid_t  H5Fopen(const char *filename, unsigned flags,
-- >        hid_t access_plist);
#ccall H5Fopen, CString -> CUInt -> <hid_t> -> IO <hid_t>

-- |Reopen a file.  The new file handle which is returned points
-- to the same file as the specified file handle.  Both handles
-- share caches and other information.  The only difference
-- between the handles is that the new handle is not mounted
-- anywhere and no files are mounted on it.
--
-- On success, returns a file ID.  On failure, returns a negative value.
--
-- > hid_t  H5Freopen(hid_t file_id);
#ccall H5Freopen, <hid_t> -> IO <hid_t>

-- |Flushes all outstanding buffers of a file to disk but does
-- not remove them from the cache.  The 'object_id' can be a file,
-- dataset, group, attribute, or named data type.
--
-- Returns non-negative on success / negative on failure
--
-- > herr_t H5Fflush(hid_t object_id, H5F_scope_t scope);
#ccall H5Fflush, <hid_t> -> <H5F_scope_t> -> IO <herr_t>

-- |This function closes the file specified by 'file_id' by
-- flushing all data to storage, and terminating access to the
-- file through 'file_id'.  If objects (e.g., datasets, groups,
-- etc.) are open in the file then the underlying storage is not
-- closed until those objects are closed; however, all data for
-- the file and the open objects is flushed.
--
-- Returns non-negative on success / negative on failure
--
-- > herr_t H5Fclose(hid_t file_id);
#ccall H5Fclose, <hid_t> -> IO <herr_t>

-- |Get an atom for a copy of the file-creation property list for
-- this file. This function returns an atom with a copy of the
-- properties used to create a file.
--
-- On success, returns a template ID.
-- On failure, returns a negative value.
--
-- > hid_t  H5Fget_create_plist(hid_t file_id);
#ccall H5Fget_create_plist, <hid_t> -> IO <hid_t>

-- |Returns a copy of the file access property list of the
-- specified file.
--
-- NOTE: If you are going to overwrite information in the copied
-- property list that was previously opened and assigned to the
-- property list, then you must close it before overwriting the values.
--
-- On success, returns an Object ID for a copy of the file access
-- property list.  On failure, returns a negative value.
--
-- > hid_t  H5Fget_access_plist(hid_t file_id);
#ccall H5Fget_access_plist, <hid_t> -> IO <hid_t>

-- |Public API to retrieve the file's 'intent' flags passed
-- during 'h5f_open'.
--
-- Returns non-negative on success / negative on failure
--
-- > herr_t H5Fget_intent(hid_t file_id, unsigned * intent);
#ccall H5Fget_intent, <hid_t> -> Out CUInt -> IO <herr_t>

-- |Returns the number of opened object IDs (files, datasets, groups
-- and datatypes) in the same file.
--
-- Returns non-negative on success, negative on failure.
--
-- > ssize_t H5Fget_obj_count(hid_t file_id, unsigned types);
#ccall H5Fget_obj_count, <hid_t> -> CUInt -> IO <ssize_t>

-- |Returns a list of opened object IDs.
--
-- Returns non-negative on success, negative on failure
--
-- > ssize_t H5Fget_obj_ids(hid_t file_id, unsigned types, size_t max_objs, hid_t *obj_id_list);
#ccall H5Fget_obj_ids, <hid_t> -> CUInt -> <size_t> -> OutArray <hid_t> -> IO <ssize_t>

-- |Returns a pointer to the file handle of the low-level file driver.
--
-- Returns non-negative on success, negative on failure
--
-- > herr_t H5Fget_vfd_handle(hid_t file_id, hid_t fapl, void **file_handle);
#ccall H5Fget_vfd_handle, <hid_t> -> <hid_t> -> Out (Ptr CFile) -> IO <herr_t>

-- |Mount file 'child_id' onto the group specified by 'loc_id' and
-- 'name' using mount properties 'plist_id'.
--
-- Returns non-negative on success, negative on failure
--
-- > herr_t H5Fmount(hid_t loc, const char *name, hid_t child, hid_t plist);
#ccall H5Fmount, <hid_t> -> CString -> <hid_t> -> <hid_t> -> IO <herr_t>

-- |Given a mount point, dissassociate the mount point's file
-- from the file mounted there.  Do not close either file.
--
-- The mount point can either be the group in the parent or the
-- root group of the mounted file (both groups have the same
-- name).  If the mount point was opened before the mount then
-- it's the group in the parent, but if it was opened after the
-- mount then it's the root group of the child.
--
-- Returns non-negative on success, negative on failure
--
-- > herr_t H5Funmount(hid_t loc, const char *name);
#ccall H5Funmount, <hid_t> -> CString -> IO <herr_t>

-- |Retrieves the amount of free space in the file.
-- Returns a negative value on failure.
--
-- hssize_t H5Fget_freespace(hid_t file_id);
#ccall H5Fget_freespace, <hid_t> -> IO <hssize_t>

-- |Retrieves the file size of the HDF5 file. This function
-- is called after an existing file is opened in order
-- to learn the true size of the underlying file.
--
-- Returns non-negative on success, negative on failure
--
-- > herr_t H5Fget_filesize(hid_t file_id, hsize_t *size);
#ccall H5Fget_filesize, <hid_t> -> Out <hsize_t> -> IO <herr_t>

-- |If a buffer is provided (via the buf_ptr argument) and is
-- big enough (size in buf_len argument), load *buf_ptr with
-- an image of the open file whose ID is provided in the
-- file_id parameter, and return the number of bytes copied
-- to the buffer.
--
-- If the buffer exists, but is too small to contain an image
-- of the indicated file, return a negative number.
--
-- Finally, if no buffer is provided, return the size of the
-- buffer needed.  This value is simply the eoa of the target
-- file.
--
-- Note that any user block is skipped.
--
-- Also note that the function may not be used on files
-- opened with either the split/multi file driver or the
-- family file driver.
--
-- In the former case, the sparse address space makes the
-- get file image operation impractical, due to the size of
-- the image typically required.
--
-- In the case of the family file driver, the problem is
-- the driver message in the super block, which will prevent
-- the image being opened with any driver other than the
-- family file driver -- which negates the purpose of the
-- operation.  This can be fixed, but no resources for
-- this now.
--
-- Return:      Success:        Bytes copied / number of bytes needed.
--              Failure:        negative value
--
-- > ssize_t H5Fget_file_image(hid_t file_id, void * buf_ptr, size_t buf_len);
#ccall H5Fget_file_image, <hid_t> -> InArray a -> <size_t> -> IO <ssize_t>

-- |Retrieves the current automatic cache resize configuration
-- from the metadata cache, and return it in 'config_ptr'.
--
-- Note that the 'version' field of 'config_ptr' must be correctly
-- filled in by the caller.  This allows us to adapt for
-- obsolete versions of the structure.
--
-- Returns non-negative on success, negative on failure
--
-- > herr_t H5Fget_mdc_config(hid_t file_id,
-- >        H5AC_cache_config_t * config_ptr);
#ccall H5Fget_mdc_config, <hid_t> -> Out <H5AC_cache_config_t> -> IO <herr_t>

-- |Sets the current metadata cache automatic resize
-- configuration, using the contents of the instance of
-- 'H5AC_cache_config_t' pointed to by 'config_ptr'.
--
-- Returns non-negative on success, negative on failure
--
-- > herr_t H5Fset_mdc_config(hid_t file_id,
-- >        H5AC_cache_config_t * config_ptr);
#ccall H5Fset_mdc_config, <hid_t> -> In <H5AC_cache_config_t> -> IO <herr_t>

-- |Retrieves the current hit rate from the metadata cache.
-- This rate is the overall hit rate since the last time
-- the hit rate statistics were reset either manually or
-- automatically.
--
-- Returns non-negative on success, negative on failure
--
-- > herr_t H5Fget_mdc_hit_rate(hid_t file_id, double * hit_rate_ptr);
#ccall H5Fget_mdc_hit_rate, <hid_t> -> Out CDouble -> IO <herr_t>

-- |Retrieves the maximum size, minimum clean size, current
-- size, and current number of entries from the metadata
-- cache associated with the specified file.  If any of
-- the ptr parameters are NULL, the associated datum is
-- not returned.
--
-- Returns non-negative on success, negative on failure
--
-- > herr_t H5Fget_mdc_size(hid_t file_id,
-- >        size_t * max_size_ptr,
-- >        size_t * min_clean_size_ptr,
-- >        size_t * cur_size_ptr,
-- >        int * cur_num_entries_ptr);
#ccall H5Fget_mdc_size, <hid_t> -> Out <size_t> -> Out <size_t> -> Out <size_t> -> Out CInt -> IO <herr_t>

-- |Reset the hit rate statistic whose current value can
-- be obtained via the 'h5f_get_mdc_hit_rate' call.  Note
-- that this statistic will also be reset once per epoch
-- by the automatic cache resize code if it is enabled.
--
-- It is probably a bad idea to call this function unless
-- you are controlling cache size from your program instead
-- of using our cache size control code.
--
-- Returns non-negative on success, negative on failure
--
-- > herr_t H5Freset_mdc_hit_rate_stats(hid_t file_id);
#ccall H5Freset_mdc_hit_rate_stats, <hid_t> -> IO <herr_t>

-- |Gets the name of the file to which object OBJ_ID belongs.
-- If 'name' is non-NULL then write up to 'size' bytes into that
-- buffer and always return the length of the entry name.
-- Otherwise 'size' is ignored and the function does not store the name,
-- just returning the number of characters required to store the name.
-- If an error occurs then the buffer pointed to by 'name' (NULL or non-NULL)
-- is unchanged and the function returns a negative value.
--
-- Note:  This routine returns the name that was used to open the file,
-- not the actual name after resolving symlinks, etc.
--
-- Returns the length of the file name (_not_ the length of the data
-- copied into the output buffer) on success, or a negative value on failure.
--
-- > ssize_t H5Fget_name(hid_t obj_id, char *name, size_t size);
#ccall H5Fget_name, <hid_t> -> OutArray CChar -> <size_t> -> IO <ssize_t>

-- |#. Get storage size for superblock extension if there is one
--
--  #. Get the amount of btree and heap storage for entries in the SOHM table if there is one.
--
--  #. Consider success when there is no superblock extension and/or SOHM table
--
-- Returns non-negative on success, negative on failure
--
-- |Releases the external file cache associated with the
-- provided file, potentially closing any cached files
-- unless they are held open from somewhere else.
--
-- Returns non-negative on success, negative on failure
--
-- > herr_t H5Fclear_elink_file_cache(hid_t file_id);
#ccall H5Fclear_elink_file_cache, <hid_t> -> IO <herr_t>

#if H5_HAVE_PARALLEL

-- |Sets the atomicity mode
--
-- Returns non-negative on success, negative on failure
-- > herr_t H5Fset_mpi_atomicity(hid_t file_id, hbool_t flag);
#ccall H5Fset_mpi_atomicity, <hid_t> -> <hbool_t> -> IO <herr_t>

-- |Returns the atomicity mode
--
-- Returns non-negative on success, negative on failure
-- > herr_t H5Fget_mpi_atomicity(hid_t file_id, hbool_t *flag);
#ccall H5Fget_mpi_atomicity, <hid_t> -> Out <hbool_t> -> IO <herr_t>

#endif

#if H5_VERSION_GE(1,10,0)

-- |indicate that this file is open for writing in a
-- single-writer/multi-reader (SWMR) scenario.  Note that the
-- process(es) opening the file for reading must open the file with
-- RDONLY access, and use the special "SWMR_READ" access flag.
#num H5F_ACC_SWMR_WRITE

-- |indicate that this file is open for reading in a
-- single-writer/multi-reader (SWMR) scenario.  Note that the
-- process(es) opening the file for SWMR reading must also open the
-- file with the RDONLY flag.  */
#num H5F_ACC_SWMR_READ

#starttype H5F_info1_t
#field super_ext_size,  <hsize_t>
#field sohm.hdr_size,   <hsize_t>
#field sohm.msgs_info,  <H5_ih_info_t>
#stoptype

#starttype H5F_info2_t
#field super.version,  CUInt
#field super.super_size, <hsize_t>
#field super.super_ext_size, <hsize_t>
#field free.version, CUInt
#field free.meta_size, <hsize_t>
#field free.tot_space, <hsize_t>
#field sohm.version, CUInt
#field sohm.hdr_size, <hsize_t>
#field sohm.msgs_info, <H5_ih_info_t>
#stoptype

type H5F_info_t = H5F_info1_t

-- |Free space section information
#starttype H5F_sect_info_t

-- |Address of free space section
#field addr, <haddr_t>

-- |Size of free space section
#field size, <hsize_t>

#stoptype

-- |File space handling strategy
#newtype H5F_file_space_type_t, Eq

-- |Default (or current) free space strategy setting
#newtype_const H5F_file_space_type_t, H5F_FILE_SPACE_DEFAULT

-- |Persistent free space managers, aggregators, virtual file driver
#newtype_const H5F_file_space_type_t, H5F_FILE_SPACE_ALL_PERSIST

-- |Non-persistent free space managers, aggregators, virtual file driver
-- This is the library default
#newtype_const H5F_file_space_type_t, H5F_FILE_SPACE_ALL

-- |Aggregators, Virtual file driver
#newtype_const H5F_file_space_type_t, H5F_FILE_SPACE_AGGR_VFD

-- |Virtual file driver
#newtype_const H5F_file_space_type_t, H5F_FILE_SPACE_VFD

#num H5F_FILE_SPACE_NTYPES

-- | Data structure to report the collection of read retries for metadata items with checksum
-- Used by public routine H5Fget_metadata_read_retry_info()
-- TODO check the retries static array
#num H5F_NUM_METADATA_READ_RETRY_TYPES

#starttype H5F_retry_info_t
#field nbins, CUInt
#field retries, Ptr Word32
#stoptype

type H5F_flush_cb_t a = FunPtr (HId_t -> InOut a -> IO HErr_t)

#ccall H5Fformat_convert, <hid_t> -> IO <herr_t>
#ccall H5Fget_info1, <hid_t> -> Out H5F_info1_t -> IO <herr_t>
#ccall H5Fget_info2, <hid_t> -> Out H5F_info2_t -> IO <herr_t>
#ccall H5Fget_mdc_logging_status, <hid_t> -> Out hbool_t -> Out hbool_t -> IO <herr_t>
#ccall H5Fget_metadata_read_retry_info, <hid_t> -> Out H5F_retry_info_t -> IO <herr_t>
#ccall H5Fget_free_sections, <hid_t> -> <H5F_mem_t> -> <size_t> -> Out H5F_sect_info_t -> IO <ssize_t>
#ccall H5Fstart_mdc_logging, <hid_t> -> IO <herr_t>
#ccall H5Fstart_swmr_write, <hid_t> -> IO <herr_t>
#ccall H5Fstop_mdc_logging, <hid_t> -> IO <herr_t>

#endif
