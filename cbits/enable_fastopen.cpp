
#include <cstdint>
#include <cstdio>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/tcp.h>

extern "C" void iocba_enable_fastopen(int32_t sfd)
{
    int qlen = 5; // ??
    setsockopt(sfd, SOL_TCP, TCP_FASTOPEN, &qlen, sizeof(qlen));
}
