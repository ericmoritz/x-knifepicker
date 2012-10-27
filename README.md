x-knifepicker
=============

A tool for me to find a Swiss Army Knife

Victorinox makes too many knives, so to combat analysis paralysis, I
wrote this spider and knife scoring code.

To run
--------

    rebar get-deps compile
    erl -pa deps/*/ebin ebin -noshell -run spider download_products [Url, Dir]

Where Url is the the "View All" URL on a Victorinox category page and Dir is a directory to dump JSON files to.
