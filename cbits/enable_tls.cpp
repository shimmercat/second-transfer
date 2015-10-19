
#include "botan_all.h"

#include "../SecondTransfer/TLS/Botan_stub.h"

#include <cstdlib>

namespace second_transfer {

// Just because of the conversions, but it may be a handy place for 
// other stuff later.
void output_dn_cb (void* io_callbacks, const char a[], size_t sz)
{
    iocba_push(io_callbacks, (char*)a, sz);
}


} // namespace

extern "C" void botan_receive_data(
    void* tls_channel,
    char* data,
    int length )
{
    Botan::TLS::Channel* channel = (Botan::TLS::Channel*) tls_channel;
    channel->received_data( (const unsigned char*) data, length);
}

extern "C" void iocba_cleartext_push(
    void* tls_channel,
    char* data,
    int length )
{
    Botan::TLS::Channel* channel = (Botan::TLS::Channel*) tls_channel;
    channel->send( (const unsigned char*) data, length);
}
