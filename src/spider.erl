% -*- erlang -*-
%%

-module(spider).
-compile(export_all).

%% downloads product info to the directory provided
-spec download_products(string(), string()) -> ok.
download_products(IndexUrl, Dir) ->
    Await = spider_index(IndexUrl, product_callback(Dir)),
    Await().

%% returns a callback function that takes a Product propslist
%% The returned function saves the product to Dir/{product_id}.json
-spec product_callback(string()) -> function().
product_callback(Dir) ->
    fun(Product) ->
            Url = unicode:characters_to_list(
                    proplists:get_value(url, Product)),
            Key = lists:last(string:tokens(Url, "/")),
            JsonPath = lists:flatten([
                                      Dir,
                                      Key,
                                      ".json"
                                     ]),
            JsonPacket = mochijson2:encode({struct, Product}),
            file:write_file(JsonPath, JsonPacket)
    end.

%% spiders an index URL, calling Callback with product data
%% returns a function which blocks until all the product urls are processed
-spec spider_index(string(), function()) -> function().
spider_index(IndexUrl, Callback) ->
    HTML = fetch_url(IndexUrl),
    ProductUrls = index_page:product_urls(
                    index_page:parse(HTML)
                   ),
    Ref = make_ref(),
    N = spawn_workers(
          IndexUrl,
          Callback,
          Ref,
          0,
          ProductUrls
     ),

    % Return a function that blocks until done
    fun() -> accum_result(Ref, N) end.

%% Internal
spawn_workers(_, _, _, N, []) ->
    N;
spawn_workers(IndexUrl, Callback, Ref, N, [ProductUrl|Rest]) ->
    Self = self(),
    spawn(fun() ->
                  process_knife_url(
                    full_product_url(IndexUrl, ProductUrl),
                    Ref, Callback, Self
                   )
          end),
    spawn_workers(IndexUrl, Callback, Ref, N+1, Rest).

process_knife_url(Url, Ref, Callback, Pid) ->
    Knife = download_knife(Url),
    Callback(Knife),
    io:format("Got ~p~n", [Knife]),
    Pid ! {product, Ref}.

accum_result(_Ref, 0) ->
    ok;
accum_result(Ref, N) ->
    receive
        {product, Ref} ->
            accum_result(Ref, N-1)
    end.

knife_data(Url, HTML) ->
    Knife = knife:parse(HTML),
    %% We need the chardata as binaries so that mochijson can encode them as strings
    [
     {url, unicode:characters_to_binary(Url)},
     {name, unicode:characters_to_binary(knife:name(Knife))},
     {tools, lists:map(
               fun unicode:characters_to_binary/1,
               knife:tools(Knife)
              )}
    ].
    
download_knife(Url) ->
    HTML = fetch_url(Url),
    knife_data(Url, HTML).


full_product_url(IndexUrl, ProductPath) ->
    {Scheme, Netloc, _, _, _} = mochiweb_util:urlsplit(IndexUrl),
    {Path, Query, Fragment} = mochiweb_util:urlsplit_path(ProductPath),
    mochiweb_util:urlunsplit({Scheme, Netloc, Path, Query, Fragment}).

fetch_url(Url) ->
    {ok, {Status, _, Body}} = httpc:request(Url),
    % cause a badmatch error if we get a non 200
    {{_, 200, _}, Url} = {Status, Url},
    Body.
