% -*- erlang -*-
-module(knife).

-export(
   [
    parse/1,
    name/1,
    tools/1
   ]
).

parse(HTMLSource) ->
    mochiweb_html:parse(HTMLSource).

name(Tree) ->
    [{_, Attrs, _}] = mochiweb_xpath:execute("/html/head/meta[@property='og:title']", Tree),
    clean(
      unicode:characters_to_list(
        proplists:get_value(<<"content">>, Attrs)
       )).

tools(Tree) ->
    lists:map(
      fun({_, _, CData}) -> clean(unicode:characters_to_list(CData)) end,
      mochiweb_xpath:execute("//div[@class='innerproductSpecificationinfo']/ol/li", Tree)
      ).

clean(Str) ->
    string:strip(
      normalize_chars(Str)
     ).

% converts nbsp to a space
normalize_chars(Str) ->
    lists:map(
      fun(160) -> 32;
         (C) -> C
      end,
      Str
      ).
