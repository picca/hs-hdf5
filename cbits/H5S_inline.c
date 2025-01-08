#include <H5Spublic.h>
#include <bindings.cmacros.h>

#if H5_VERSION_GE(1, 12, 0)
BC_INLINE3(H5Sencode, hid_t, void *, size_t *, herr_t);
#else
#define inline_H5Sencode H5Sencode
#endif
