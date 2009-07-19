#!/bin/bash

for i in `cat ~/.hosts.erlang | sed s/%.*//g | sed s/.$//g`; do 
        echo ">> "$i" <<"
        ssh-copy-id -i ~/.ssh/id_rsa.pub $i
        ssh $i 'chmod 0600 ~/.erlang.cookie'
        scp ~/.erlang.cookie $i:~
        ssh $i 'chmod 0600 ~/.erlang.cookie'
        scp csrc/sha1 $i:/tmp
        ssh $i 'chmod 0700 /tmp/sha1'
done
