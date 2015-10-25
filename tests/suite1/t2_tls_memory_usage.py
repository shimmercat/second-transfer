
from suite_tools import *

from ssl import wrap_socket


def test_for_leaks():
    # Let's test for leaks first
    p_server = compile_and_invoke( "t2_tls_leaks.hs" )
    ld = LeakDetector(p_server.pid)
    sockets = []
    time.sleep(1.0)
    for i in range(200):
        conn = wrap_socket( socket.create_connection(("127.0.0.1", 8090)) )
        sockets.append(conn)

    repeats = 12
    exercises = 50

    for i in range(repeats):
        ld.sample()
        nsockets = []
        for i in range(exercises):
            conn = wrap_socket( socket.create_connection(("127.0.0.1", 8090)) )
            nsockets.append(conn)
        for ns in nsockets:
            ns.close()

    n = repeats * exercises
    re = rconnect()
    ensure(re.counter("socket-close-called")>1, "not even a socket-close called")
    ensure(re.counter("socket-close-called")>= n, "not enough closed sockets")
    ensure(re.counter("socket-shutdown-executed")>= n, "not enough sockets shutdown")
    ensure(re.counter("socket-close-executed")>= n, "not enough sockets closed")
    ensure(re.counter("stable-pointer-freed")>1, "we are leaking stable pointers")

    ensure(not ld.leaks(sample_size=repeats//2), "It's leaking memory!")
    os.kill(-p_server.pid, signal.SIGTERM)


if __name__ == '__main__':
    test_for_leaks()