#! /bin/sh
set -e
erl -noshell -pa ./ebin -run txt2gds -extra "$@"
