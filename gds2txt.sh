#! /bin/sh
set -e
erl -noshell -pa ./ebin -run gds2txt -extra "$@"
