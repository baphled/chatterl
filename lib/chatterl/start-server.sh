#!/bin/sh
cd `dirname $0`

exec erl -pa $PWD/ebin -s chatterl -s reloader -boot start_sasl
