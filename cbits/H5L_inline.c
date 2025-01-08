#include <H5Lpublic.h>
#include <bindings.cmacros.h>

#if defined(H5Lget_info_vers)
# if H5Lget_info_vers == 1
BC_INLINE4(H5Lget_info, hid_t, const char *, H5L_info_t *, hid_t, herr_t)
# else
BC_INLINE4(H5Lget_info, hid_t, const char *, H5L_info_t *, hid_t, herr_t)
# endif
#endif

#if defined(H5Lget_info_by_idx_vers)
# if H5Lget_info_by_idx_vers == 1
BC_INLINE7(H5Lget_info_by_idx, hid_t, const char *, H5_index_t, H5_iter_order_t, hsize_t, H5L_info1_t *, hid_t, herr_t)
# else
BC_INLINE7(H5Lget_info_by_idx, hid_t, const char *, H5_index_t, H5_iter_order_t, hsize_t, H5L_info2_t *, hid_t, herr_t)
# endif
#endif

#if defined(H5Literate_vers)
# if H5Literate_vers == 1
BC_INLINE6(H5Literate,  hid_t, H5_index_t, H5_iter_order_t, hsize_t *, H5L_iterate1_t, void *, herr_t)
# elif H5Literate_vers == 2
BC_INLINE6(H5Literate,  hid_t, H5_index_t, H5_iter_order_t, hsize_t *, H5L_iterate2_t, void *, herr_t)
# else
#  error TODO
# endif
#endif

#if defined(H5Literate_by_name_vers)
# if H5Literate_by_name_vers == 1
BC_INLINE8(H5Literate_by_name,  hid_t, const char *, H5_index_t, H5_iter_order_t, hsize_t *, H5L_iterate1_t, void *, hid_t, herr_t)
# elif H5Literate_by_name_vers == 2
BC_INLINE8(H5Literate_by_name,  hid_t, const char *, H5_index_t, H5_iter_order_t, hsize_t *, H5L_iterate2_t, void *, hid_t, herr_t)
# else
#  error TODO
# endif
#endif

#if defined(H5Lvisit_vers)
# if H5Lvisit_vers == 1
BC_INLINE5(H5Lvisit,  hid_t, H5_index_t, H5_iter_order_t, H5L_iterate1_t, void *, herr_t)
# elif H5Lvisit_vers == 2
BC_INLINE5(H5Lvisit,  hid_t, H5_index_t, H5_iter_order_t, H5L_iterate2_t, void *, herr_t)
# else
#  error TODO
# endif
#endif

#if defined(H5Lvisit_by_name_vers)
# if H5Lvisit_by_name_vers == 1
BC_INLINE7(H5Lvisit_by_name, hid_t, const char *, H5_index_t, H5_iter_order_t, H5L_iterate1_t, void *, hid_t, herr_t)
# elif H5Lvisit_by_name_vers == 2
BC_INLINE7(H5Lvisit_by_name, hid_t, const char *, H5_index_t, H5_iter_order_t, H5L_iterate2_t, void *, hid_t, herr_t)
# else
#  error TODO
# endif
#endif
