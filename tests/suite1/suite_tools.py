from __future__ import print_function
from sys import stderr
import subprocess as sp
import threading
import socket
import time
import os
import os.path
import signal
import redis
from shlex import (split)
from catch_memory_leaks import LeakDetector

def invoke(s, **kwargs):
    pieces = split(s)
    p = sp.Popen(pieces, **kwargs)
    return p

ERRORS = 0

def ensure(p, msg):
    global ERRORS
    if not p:
        print(msg, file=stderr)
        ERRORS += 1

def compile_and_invoke( hs_name ):
    cmdline = "stack ghc -- -XCPP -I../../macros -pgmPcpphs -optP--cpp -threaded {0}".format(hs_name)
    print(cmdline)
    p = invoke(cmdline)
    retcode = p.wait()
    if retcode != 0:
        raise RuntimeError("Compiling {0} failed".format(hs_name))
    exe_name, _ = os.path.splitext(hs_name)
    cmd_path = './' + exe_name
    p = invoke(cmd_path, start_new_session=True)
    return p


class RedisUtil(object):
    def __init__(self, conn):
        self._conn = conn

    def counter(self, counter_name):
        w = self._conn.get(counter_name)
        if w is None or w == "":
            return 0
        return int(w)

    @property
    def redis(self):
        return self._conn

def rconnect():
    reconn = redis.StrictRedis(db=3)
    return RedisUtil(reconn)

