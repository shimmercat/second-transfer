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

def invoke(s, **kwargs):
    pieces = split(s)
    p = sp.Popen(pieces, **kwargs)
    return p

def ensure(p, msg):
    if not p:
        print(msg, file=stderr)
        raise SystemExit(1)

# Do we run out of available sockets?
def try_exhaust_sockets():
    p_server = spin_server()
    restarts = 0
    ld = LeakDetector(p_server.pid)
    try:
        sockets = []
        time.sleep(1.0)
        for i in range(1025):
            try:
                conn = socket.create_connection(("127.0.0.1", 3037))
                ld.sample()
                if restarts == 1:
                    restarts = 2
                #conn.recv(1)
            except socket.error:
                ensure(not ld.leaks(), "It's leaking memory!")
                ld = LeakDetector(p_server.pid)
                for s in sockets:
                    s.close()
                sockets = []
                restarts = 1
            else:
                sockets.append(conn)
        reconn = redis.StrictRedis(db=3)

        ensure(int(reconn.get("listening") or 0), "not listening")
        ensure(int(reconn.get("resource-exhausted") or 0)>0, "resource-exhausted not reported")
        ensure(restarts == 2, "restarts ok")
    finally:
        try:
            # Grr
            #os.system("pkill ghc")
            os.kill(-p_server.pid, signal.SIGTERM)
        except OSError as e:
            print(e)
            pass

def spin_server():
    return invoke("stack runghc t1_accept_exceptions", start_new_session=True)

def main():
    try_exhaust_sockets()



if __name__ == '__main__':
    main()
