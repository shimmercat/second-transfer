#!/bin/bash
LOCAL_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
stack ghc -- -package second-transfer --make $LOCAL_DIR/Main.hs -o $LOCAL_DIR/socks5_tcp_server
