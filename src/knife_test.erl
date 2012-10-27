% -*- erlang -*-

-module(knife_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

name_test() ->
    ?assertEqual(
       "Mechanic",
       knife:name(
         knife:parse(
           spider_fixture:knife()
          )
        )
    ).


tools_test() ->
    ?assertEqual(
       [
        "large blade",
        "small blade",
        "screwdriver",
        "cap lifter with",
        "-screwdriver",
        "-wire stripper",
        "reamer",
        "toothpick",
        "pliers"
       ], 
       knife:tools(
         knife:parse(
           spider_fixture:knife()
          )
        )
    ).
