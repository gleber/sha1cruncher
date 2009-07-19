#!/bin/bash

for i in `cat ~/.hosts.erlang | sed s/%.*//g | sed s/.$//g`; do 
        echo ">> "$i" <<"
        ssh $i "ps ax | grep python | grep sha1 | grep -v grep | awk '{print \$1}' | xargs kill -s 9"
done
