#include "HsFFI.h"
#ifdef __cplusplus
extern "C" {
#endif
extern void iocba_push(HsStablePtr a1, HsPtr a2, HsInt32 a3);
extern void iocba_data_cb(HsStablePtr a1, HsPtr a2, HsInt32 a3);
extern void iocba_alert_cb(HsStablePtr a1, HsInt32 a2);
extern void iocba_handshake_cb(HsStablePtr a1);
extern HsInt iocba_select_protocol_cb(HsStablePtr a1, HsPtr a2, HsInt a3);
#ifdef __cplusplus
}
#endif

