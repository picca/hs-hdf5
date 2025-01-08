#include <H5Rpublic.h>
#include <bindings.cmacros.h>

#if H5Rdereference_vers == 1
BC_INLINE3(H5Rdereference, hid_t, H5R_type_t, void *, hid_t);
#else
BC_INLINE4(H5Rdereference, hid_t, hid_t, H5R_type_t, void *, hid_t);
#endif
