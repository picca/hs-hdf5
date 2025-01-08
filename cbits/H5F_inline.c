#include <H5Fpublic.h>
#include <bindings.cmacros.h>

#if H5Fget_info_vers == 1
BC_INLINE2(H5Fget_info, hid_t, H5F_info1_t *, herr_t);
#else
BC_INLINE2(H5Fget_info, hid_t, H5F_info2_t *, herr_t);
#endif
