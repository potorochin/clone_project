#!/bin/sh

exec erl -sname autorization \
    -config sys \
	-pa ebin/ deps/*/ebin \
    -boot start_sasl \
    -s autorization
