#!/bin/bash
LOCAL_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
stack ghc -- -package second-transfer  -threaded -lstdc++ -lbotan-1.11 --make $LOCAL_DIR/Main.hs -o $LOCAL_DIR/simple_tls_server
