% -*- erlang -*-
-module(spider_fixture).

-export([knife/1, knife/0, index_page/1, index_page/0]).

-define(EUNIT_PRIVDIR, "../priv/").

%% Public

knife() ->
    knife(?EUNIT_PRIVDIR).

knife(PrivDir) ->
    load_fixture(PrivDir, "fixture_knife.html").

index_page() ->
    index_page(?EUNIT_PRIVDIR).

index_page(PrivDir) ->
    load_fixture(PrivDir, "fixture_index.html").

%% Internal
load_fixture(PrivDir, Filename) ->
    Path = lists:flatten(
             [PrivDir, Filename]
            ),
    {ok, Data} = file:read_file(Path),
    Data.

    
