#include <H5version.h>
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-

  h5l_get_info_by_idx           	[ FAIL ]
  h5l_iterate                   	[  OK  ]
  h5l_register                  	[ FAIL ]
  h5l_iterate_by_name           	[  OK  ]
  h5l_unpack_elink_val          	[ FAIL ]
  h5l_get_val_by_idx            	[ FAIL ]
  h5l_create_external           	[  OK  ]
  h5l_exists                    	[  OK  ]
  h5l_move                      	[  OK  ]
  h5l_create_ud                 	[ FAIL ]
  h5l_create_hard               	[  OK  ]
  h5l_is_registered             	[ FAIL ]
  h5l_get_name_by_idx           	[  OK  ]
  h5l_create_soft               	[  OK  ]
  h5l_copy                      	[  OK  ]
  h5l_get_val                   	[  OK  ]
  h5l_visit                     	[  OK  ]
  h5l_get_info                  	[  OK  ]
  h5l_delete_by_idx             	[ FAIL ]
  h5l_visit_by_name             	[  OK  ]
  h5l_delete                    	[  OK  ]
  h5l_unregister                	[ FAIL ]

-}
module Bindings.HDF5.Link
    ( createHardLink
    , createSoftLink
    , createExternalLink

    , getLinkNameByIdx

    , doesLinkExist

    , moveLink
    , copyLink
    , deleteLink

    , LinkType(..)
    , LinkInfo(..)
    , getLinkInfo

    , getSymLinkVal

    , iterateLinks
    , iterateLinksByName

    , visitLinks
    , visitLinksByName
    ) where

import           Bindings.HDF5.Core
import           Bindings.HDF5.Datatype.Internal
import           Bindings.HDF5.Error
import           Bindings.HDF5.Group
import           Bindings.HDF5.PropertyList.LAPL
import           Bindings.HDF5.PropertyList.LCPL
import           Bindings.HDF5.Raw.H5
import           Bindings.HDF5.Raw.H5I
import           Bindings.HDF5.Raw.H5L
import           Bindings.HDF5.Raw.H5P
import           Bindings.HDF5.Raw.Util
import           Control.Exception               (SomeException, finally,
                                                  throwIO, try)
import qualified Data.ByteString                 as BS
import           Data.IORef
import           Foreign
import           Foreign.C
import           Foreign.Ptr.Conventions

{-# ANN module "HLint: ignore Use camelCase" #-}

createHardLink :: (Location src, Location dst) => src -> BS.ByteString -> dst -> BS.ByteString -> Maybe LCPL -> Maybe LAPL -> IO ()
createHardLink src srcName dst dstName lcpl lapl =
    withErrorCheck_ $
        BS.useAsCString srcName $ \csrcName ->
            BS.useAsCString dstName $ \cdstName ->
                h5l_create_hard (hid src) csrcName (hid dst) cdstName
                    (maybe h5p_DEFAULT hid lcpl)
                    (maybe h5p_DEFAULT hid lapl)

createSoftLink :: Location dst => BS.ByteString -> dst -> BS.ByteString -> Maybe LCPL -> Maybe LAPL -> IO ()
createSoftLink srcName dst dstName lcpl lapl =
    withErrorCheck_ $
        BS.useAsCString srcName $ \csrcName ->
            BS.useAsCString dstName $ \cdstName ->
                h5l_create_soft csrcName (hid dst) cdstName
                    (maybe h5p_DEFAULT hid lcpl)
                    (maybe h5p_DEFAULT hid lapl)

createExternalLink :: Location loc => BS.ByteString -> BS.ByteString -> loc -> BS.ByteString -> Maybe LCPL -> Maybe LAPL -> IO ()
createExternalLink file obj loc name lcpl lapl =
    withErrorCheck_ $
        BS.useAsCString file $ \cfile ->
            BS.useAsCString obj $ \cobj ->
                BS.useAsCString name $ \cname ->
                    h5l_create_external cfile cobj (hid loc) cname (maybe h5p_DEFAULT hid lcpl) (maybe h5p_DEFAULT hid lapl)

getLinkNameByIdx :: Location loc =>  loc -> BS.ByteString -> IndexType -> IterOrder -> HSize -> Maybe LAPL -> IO BS.ByteString
getLinkNameByIdx loc group indexType order idx lapl =
  withOutByteString' $ \cname nameSize ->
  BS.useAsCString group $ \cgroup ->
  h5l_get_name_by_idx (hid loc) cgroup (indexTypeCode indexType) (iterOrderCode order) (hSize idx) cname nameSize (maybe h5p_DEFAULT hid lapl)

doesLinkExist :: Location loc => loc -> BS.ByteString -> Maybe LAPL -> IO Bool
doesLinkExist loc name lapl =
    htriToBool $
        BS.useAsCString name $ \cname ->
            h5l_exists (hid loc) cname (maybe h5p_DEFAULT hid lapl)

moveLink :: (Location src, Location dst) => src -> BS.ByteString -> dst -> BS.ByteString -> Maybe LCPL -> Maybe LAPL -> IO ()
moveLink  src srcName dst dstName lcpl lapl =
    withErrorCheck_ $
        BS.useAsCString srcName $ \csrcName ->
            BS.useAsCString dstName $ \cdstName ->
                h5l_move (hid src) csrcName (hid dst) cdstName
                    (maybe h5p_DEFAULT hid lcpl)
                    (maybe h5p_DEFAULT hid lapl)

copyLink :: (Location src, Location dst) => src -> BS.ByteString -> dst -> BS.ByteString -> Maybe LCPL -> Maybe LAPL -> IO ()
copyLink  src srcName dst dstName lcpl lapl =
    withErrorCheck_ $
        BS.useAsCString srcName $ \csrcName ->
            BS.useAsCString dstName $ \cdstName ->
                h5l_copy (hid src) csrcName (hid dst) cdstName
                    (maybe h5p_DEFAULT hid lcpl)
                    (maybe h5p_DEFAULT hid lapl)

deleteLink :: Location t => t -> BS.ByteString -> Maybe LAPL -> IO ()
deleteLink loc name lapl =
    withErrorCheck_ $
        BS.useAsCString name $ \cname ->
            h5l_delete (hid loc) cname (maybe h5p_DEFAULT hid lapl)

data LinkType
    = External
    | Hard
    | Soft
    | OtherLinkType !H5L_type_t
    deriving (Eq, Ord, Read, Show)

linkTypeFromCode :: H5L_type_t -> LinkType
linkTypeFromCode c
    | c == h5l_TYPE_EXTERNAL    = External
    | c == h5l_TYPE_HARD        = Hard
    | c == h5l_TYPE_SOFT        = Soft
    | c >= h5l_TYPE_UD_MIN      = OtherLinkType c
    | otherwise                 = error ("Unknown link type: " ++ show c)


data LinkInfo = LinkInfo
    { linkType        :: LinkType
    , linkCOrderValid :: Bool
    , linkCOrder      :: Int64
    , linkCSet        :: CSet
    , linkValSize     :: CSize
    } deriving (Eq, Ord, Read, Show)

#if H5Fget_info_vers == 1
readLinkInfo :: H5L_info_t -> LinkInfo
readLinkInfo i  = LinkInfo
    { linkType          = linkTypeFromCode (h5l_info_t'type i)
    , linkCOrderValid   = hboolToBool (h5l_info_t'corder_valid i)
    , linkCOrder        = h5l_info_t'corder i
    , linkCSet          = cSetFromCode (h5l_info_t'cset i)
    , linkValSize       = h5l_info_t'u'val_size i
    }
#else
readLinkInfo :: H5L_info2_t -> LinkInfo
readLinkInfo i  = LinkInfo
    { linkType          = linkTypeFromCode (h5l_info2_t'type i)
    , linkCOrderValid   = hboolToBool (h5l_info2_t'corder_valid i)
    , linkCOrder        = h5l_info2_t'corder i
    , linkCSet          = cSetFromCode (h5l_info2_t'cset i)
    , linkValSize       = h5l_info2_t'u'val_size i
    }
#endif

getLinkInfo :: Location loc => loc -> BS.ByteString -> Maybe LAPL -> IO LinkInfo
getLinkInfo loc name lapl =
    fmap readLinkInfo $
        withOut_ $ \info ->
            withErrorCheck_ $
                BS.useAsCString name $ \cname ->
#if (H5Fget_info_vers == 1)
                    h5l_get_info (hid loc) cname info (maybe h5p_DEFAULT hid lapl)
#else
                    h5l_get_info2 (hid loc) cname info (maybe h5p_DEFAULT hid lapl)
#endif

getSymLinkVal :: Location loc => loc -> BS.ByteString -> Maybe LAPL -> IO BS.ByteString
getSymLinkVal loc name mb_lapl =
    BS.useAsCString name $ \cname -> do
        let lapl = maybe h5p_DEFAULT hid mb_lapl
        info <- withOut_ $ \info ->
            withErrorCheck_ $
#if (H5Fget_info_vers == 1)
                    h5l_get_info (hid loc) cname info lapl
#else
                    h5l_get_info2 (hid loc) cname info lapl
#endif

#if (H5Fget_info_vers == 1)
        let n = h5l_info_t'u'val_size info
#else
        let n = h5l_info2_t'u'val_size info
#endif

        buf <- mallocBytes (fromIntegral n)

        withErrorCheck_ $
            h5l_get_val (hid loc) cname (OutArray buf) n lapl
        -- TODO: this will leak memory if an exception is thrown

        BS.packCStringLen (buf, fromIntegral n)


foreign import ccall "wrapper" wrap_H5L_iterate_t
#if (H5Fget_info_vers == 1)
    :: (HId_t -> CString -> In H5L_info_t -> InOut a -> IO HErr_t)
    -> IO (FunPtr (HId_t -> CString -> In H5L_info_t -> InOut a -> IO HErr_t))
#else
    :: (HId_t -> CString -> In H5L_info2_t -> InOut a -> IO HErr_t)
    -> IO (FunPtr (HId_t -> CString -> In H5L_info2_t -> InOut a -> IO HErr_t))
#endif    

with_iterate_t :: (Group -> BS.ByteString -> LinkInfo -> IO HErr_t)
#if (H5Fget_info_vers == 1)
     -> (H5L_iterate_t () -> InOut () -> IO HErr_t)
#else
     -> (H5L_iterate2_t () -> InOut () -> IO HErr_t)
#endif
     -> IO HErr_t
with_iterate_t op f = do
    exception1 <- newIORef Nothing :: IO (IORef (Maybe SomeException))

    op1 <- wrap_H5L_iterate_t $ \grp name (In link) _opData -> do
        name1 <- BS.packCString name
        link1 <- peek link
        result <- try (op (uncheckedFromHId grp) name1 (readLinkInfo link1))
        case result of
            Left exc -> do
                writeIORef exception1 (Just exc)
                return maxBound
            Right x -> return x

    result <- f op1 (InOut nullPtr) `finally` freeHaskellFunPtr op1

    if result == maxBound
        then do
            exception2 <- readIORef exception1
            maybe (return result) throwIO exception2

        else return result

-- TODO : It would be nice if we didn't expose HErr_t in these callback functions.
--        Decide whether we want Either or Exceptions.
iterateLinks :: Location t => t -> IndexType -> IterOrder -> Maybe HSize -> (Group -> BS.ByteString -> LinkInfo -> IO HErr_t) -> IO HSize
iterateLinks loc indexType order startIndex op =
    fmap HSize $
        withInOut_ (maybe 0 hSize startIndex) $ \ioStartIndex ->
            withErrorCheck_ $
                with_iterate_t op $ \iop opData ->
#if (H5Fget_info_vers == 1)
                    h5l_iterate (hid loc) (indexTypeCode indexType) (iterOrderCode order) ioStartIndex iop opData
#else
                    h5l_iterate2 (hid loc) (indexTypeCode indexType) (iterOrderCode order) ioStartIndex iop opData
#endif

iterateLinksByName :: Location t => t -> BS.ByteString -> IndexType -> IterOrder -> Maybe HSize -> Maybe LAPL -> (Group -> BS.ByteString -> LinkInfo -> IO HErr_t) -> IO HSize
iterateLinksByName loc groupName indexType order startIndex lapl op =
    fmap HSize $
        withInOut_ (maybe 0 hSize startIndex) $ \ioStartIndex ->
            withErrorCheck_ $
                with_iterate_t op $ \iop opData ->
                    BS.useAsCString groupName $ \cgroupName ->
#if (H5Fget_info_vers == 1)
                        h5l_iterate_by_name (hid loc) cgroupName (indexTypeCode indexType) (iterOrderCode order) ioStartIndex iop opData (maybe h5p_DEFAULT hid lapl)
#else
                        h5l_iterate_by_name2 (hid loc) cgroupName (indexTypeCode indexType) (iterOrderCode order) ioStartIndex iop opData (maybe h5p_DEFAULT hid lapl)
#endif

visitLinks :: Location t => t -> IndexType -> IterOrder -> (Group -> BS.ByteString -> LinkInfo -> IO HErr_t) -> IO ()
visitLinks loc indexType order op =
    withErrorCheck_ $
        with_iterate_t op $ \iop opData ->
#if (H5Fget_info_vers == 1)
            h5l_visit (hid loc) (indexTypeCode indexType) (iterOrderCode order) iop opData
#else
            h5l_visit2 (hid loc) (indexTypeCode indexType) (iterOrderCode order) iop opData
#endif

visitLinksByName :: Location t => t -> BS.ByteString -> IndexType -> IterOrder -> Maybe LAPL -> (Group -> BS.ByteString -> LinkInfo -> IO HErr_t) -> IO ()
visitLinksByName loc groupName indexType order lapl op =
    withErrorCheck_ $
        with_iterate_t op $ \iop opData ->
            BS.useAsCString groupName $ \cgroupName ->
#if (H5Fget_info_vers == 1)
                h5l_visit_by_name (hid loc) cgroupName (indexTypeCode indexType) (iterOrderCode order) iop opData (maybe h5p_DEFAULT hid lapl)
#else
                h5l_visit_by_name2 (hid loc) cgroupName (indexTypeCode indexType) (iterOrderCode order) iop opData (maybe h5p_DEFAULT hid lapl)
#endif
