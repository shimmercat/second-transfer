from __future__ import print_function
from sys import stderr
import subprocess as sp
import threading
import socket
import time
import os
import signal
import redis
from shlex import (split)

from catch_memory_leaks import LeakDetector
from suite_tools import *


# Do we run out of available sockets?
def main():
    p_server = compile_and_invoke("t1_accept_exceptions.hs")
    restarts = 0
    sockets = []
    time.sleep(1.0)
    for i in range(1025):
        try:
            conn = socket.create_connection(("127.0.0.1", 3037))
            if restarts == 1:
                restarts = 2
            #conn.recv(1)
        except socket.error:
            print("Closing sockets at i=",i)
            for s in sockets:
                s.close()
            sockets = []
            restarts = 1
        else:
            sockets.append(conn)
    re = rconnect()

    ensure(re.counter("listening"), "not listening")
    ensure(re.counter("resource-exhausted")>0, "resource-exhausted not reported")

    ensure(restarts == 2, "restarts ok")

    os.kill(-p_server.pid, signal.SIGTERM)


if __name__ == '__main__':
    main()
