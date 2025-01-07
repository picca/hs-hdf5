#include <H5Opublic.h>
#include <bindings.cmacros.h>

#if H5_VERSION_GE(1, 10, 3)
BC_INLINE2(H5Oget_info, hid_t, H5O_info_t *, herr_t);
BC_INLINE7(H5Oget_info_by_idx, hid_t, const char *, H5_index_t, H5_iter_order_t, hsize_t, H5O_info_t *,  hid_t, herr_t);
BC_INLINE4(H5Oget_info_by_name, hid_t, const char *, H5O_info_t *, hid_t, herr_t);
BC_INLINE5(H5Ovisit, hid_t, H5_index_t, H5_iter_order_t, H5O_iterate_t, void *, herr_t);
BC_INLINE7(H5Ovisit_by_name, hid_t, const char *, H5_index_t, H5_iter_order_t, H5O_iterate_t, void *, hid_t, herr_t);
#else
#define inline_H5Oget_info H5Oget_info
#define inline_H5Oget_info_by_idx H5Oget_info_by_idx
#define inline_H5Oget_info_by_name H5Oget_info_by_name
#define inline_H5Ovisit H5Ovisit
#define inline_H5Ovisit_by_name H5Ovisit_by_name
}
#endif
