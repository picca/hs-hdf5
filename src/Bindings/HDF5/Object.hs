{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bindings.HDF5.Object
    ( ObjectId
    , Object(..)
    , ObjectType(..)

    , objectTypeOf
    , objectTypeOf1

    , uncheckedCastObject
    , castObject

    , openObject
    , getObjectType

    , linkObject
    , closeObject
    , copyObject

    , doesObjectExist
    ) where

import Data.Maybe

import Bindings.HDF5.Core
import Bindings.HDF5.Error
import Bindings.HDF5.PropertyList.LAPL
import Bindings.HDF5.PropertyList.LCPL
import Bindings.HDF5.PropertyList.OCPYPL
import Bindings.HDF5.Raw.H5I
import Bindings.HDF5.Raw.H5O
import Bindings.HDF5.Raw.H5P
import qualified Data.ByteString as BS
import Foreign.Storable

newtype ObjectId = ObjectId HId_t
    deriving (Eq, HId, FromHId, HDFResultType, Storable)

class (HId t, FromHId t) => Object t where
    staticObjectType :: Tagged t (Maybe ObjectType)

instance Object ObjectId where
    staticObjectType = Tagged Nothing

objectTypeOf :: Object t => t -> Maybe ObjectType
objectTypeOf = f staticObjectType
    where
        f :: Tagged t a -> t -> a
        f = const . unTagged

objectTypeOf1 :: Object t => c t -> Maybe ObjectType
objectTypeOf1 = f staticObjectType
    where
        f :: Tagged t a -> c t -> a
        f = const . unTagged

uncheckedCastObject :: (Object a, Object b) => a -> b
uncheckedCastObject = uncheckedFromHId . hid

castObject :: (Object src, Object dst) => src -> IO (Maybe dst)
castObject = castTo staticObjectType
    where
        castTo :: (Object a, Object b) => Tagged b (Maybe ObjectType) -> a -> IO (Maybe b)
        castTo (Tagged Nothing)        src = return (Just (uncheckedCastObject src)) -- cast to ObjectId always succeeds
        castTo (Tagged (Just dstType)) src = do
            srcType <- getObjectType src
            return $! if srcType == dstType
                then Just (uncheckedCastObject src)
                else Nothing

openObject :: Location loc => loc -> BS.ByteString -> Maybe LAPL -> IO ObjectId
openObject loc name lapl =
    fmap ObjectId $
        BS.useAsCString name $ \cname ->
            withErrorCheck $
                h5o_open (hid loc) cname (maybe h5p_DEFAULT hid lapl)

data ObjectType
    = FileObj
    | GroupObj
    | DatatypeObj
    | DataspaceObj
    | DatasetObj
    | AttrObj
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

objectTypeCode :: ObjectType -> H5I_type_t
objectTypeCode FileObj      = h5i_FILE
objectTypeCode GroupObj     = h5i_GROUP
objectTypeCode DatatypeObj  = h5i_DATATYPE
objectTypeCode DataspaceObj = h5i_DATASPACE
objectTypeCode DatasetObj   = h5i_DATASET
objectTypeCode AttrObj      = h5i_ATTR

objectTypeFromCode :: H5I_type_t -> ObjectType
objectTypeFromCode c =
    fromMaybe (error ("Unknown object type code: " ++ show c))
                  (lookup c codes)
        where
          codes = [ (objectTypeCode x, x) | x <- [minBound .. maxBound]]

getObjectType :: Object obj => obj -> IO ObjectType
getObjectType obj =
    fmap objectTypeFromCode $
        withErrorCheck $
            h5i_get_type (hid obj)

linkObject :: (Object obj, Location loc) => obj -> loc -> BS.ByteString -> Maybe LCPL -> Maybe LAPL -> IO ()
linkObject obj loc name lcpl lapl =
    withErrorCheck_ $
        BS.useAsCString name $ \cname ->
            h5o_link (hid obj) (hid loc) cname (maybe h5p_DEFAULT hid lcpl) (maybe h5p_DEFAULT hid lapl)

closeObject :: Object obj => obj -> IO ()
closeObject obj =
    withErrorCheck_ $
        h5o_close (hid obj)

copyObject :: (Location src, Location dst) => src -> BS.ByteString -> dst -> BS.ByteString -> Maybe OCPYPL -> Maybe LCPL -> IO ()
copyObject src srcName dst dstName ocpypl lcpl =
    withErrorCheck_ $
        BS.useAsCString srcName $ \csrcName ->
            BS.useAsCString dstName $ \cdstName ->
                h5o_copy (hid src) csrcName (hid dst) cdstName
                    (maybe h5p_DEFAULT hid ocpypl)
                    (maybe h5p_DEFAULT hid lcpl)

doesObjectExist :: Location loc => loc -> BS.ByteString -> Maybe LAPL -> IO Bool
doesObjectExist loc name lapl =
    htriToBool $
        BS.useAsCString name $ \cname ->
            h5o_exists_by_name (hid loc) cname (maybe h5p_DEFAULT hid lapl)
