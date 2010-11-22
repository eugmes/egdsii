DESCRIPTION
===========

This is a simple Erlang library for reading GDSII (aka GDS 2) files.

The repository also includes programs that can be used to
convert a GDSII file to a text representation and back to GDS,
called gds2txt and txt2gds.

USAGE
=====

To compile the library and the test program run `make`. Test
program can be used as follows:

    ./gds2txt.sh /path/to/file.gds > file.txt

Conversion back to GDS format can be done using txt2gds:

    ./txt2gds.sh -o out.gds file.txt
