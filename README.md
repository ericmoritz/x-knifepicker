x-knifepicker
=============

A tool for me to find a Swiss Army Knife

Victorinox makes too many knives, so to combat analysis paralysis, I
wrote this spider and knife scoring code.

To run
--------

    rebar get-deps compile
    erl -pa deps/*/ebin ebin/ -noinput -s inets -eval 'spider:download_products(Url, Dir), init:stop().'

Where Url is the the "View All" URL on a Victorinox category page and Dir is a directory to dump JSON files to.
