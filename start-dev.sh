#!/bin/sh
cd `dirname $0`
exec erl +A 128 -name master -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl \
        -mnesia dir db -mnesia dump_log_write_threshold 1000000 \
        -s main

