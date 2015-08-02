#!/bin/bash
THIS_DIR=$(dirname `readlink -f ${BASH_SOURCE[0]}`)
source makeenv
export PATH=$THIS_DIR/env-scripts/bin-deceipt:$PATH
echo $PATH
emacs $@
