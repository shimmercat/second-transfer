#!/bin/bash

# Need to change dirs to get the test certificates
pushd ../../
#attempt-bust +RTS -hy -xt -i0.1 -RTS & 
#attempt-bust +RTS -S -RTS & 
valgrind --tool=massif attempt-bust & 
ppid="$!"
echo "Pid: $ppid"

for i in {1..500}
do
    curl -s -X GET --http2 -k https://www.httpdos.com:9043/ # > /dev/null
    #sleep 0.1
    #echo "."
done

kill -s SIGINT $ppid
sleep 5.0
kill -s SIGTERM $ppid || echo "term not needed"
sleep 5.0
kill -s SIGKILL $ppid || echo "kill not needed"
#wait
popd
