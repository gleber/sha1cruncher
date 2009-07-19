#!/bin/bash

for i in `cat ~/.hosts.erlang | sed s/%.*//g | sed s/.$//g`; do 
        echo ">> "$i" <<"
        scp csrc/sha1 $i:/tmp
        ssh $i 'chmod 0700 /tmp/sha1'
done
