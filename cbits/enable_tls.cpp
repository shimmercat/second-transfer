
#include "botan_all.h"

#include "../SecondTransfer/TLS/Botan_stub.h"

#include <cstdlib>

namespace second_transfer {


class output_dn_t{
    void* io_callbacks;
public:
    output_dn_t(
        void* io_callbacks
    ): io_callbacks(io_callbacks)
    {}

    void operator() (const char a[], size_t sz)
    {
        iocba_push(io_callbacks, (char*)a, sz);
    }
};

} // namespace

extern "C" void botan_receive_data(
    void* tls_channel,
    char* data,
    int length )
{
    Botan::TLS::Channel* channel = (Botan::TLS::Channel*) tls_channel;
    channel->received_data( (const unsigned char*) data, length);
}

