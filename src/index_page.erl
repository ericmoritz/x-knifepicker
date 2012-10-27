% -*- erlang -*-
-module(index_page).

-export(
   [       
           parse/1,
           product_urls/1
   ]
).

parse(HTMLSource) ->
    mochiweb_html:parse(HTMLSource).

product_urls(Tree) ->
    lists:map(
      fun({_, Attribs, _}) -> 
              unicode:characters_to_list(
                proplists:get_value(<<"href">>, Attribs)
               )
      end,
      mochiweb_xpath:execute("//div[@class='product_Title_NameDIV']/a", Tree)
    ).

