% -*- erlang -*-
-module(index_page_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


product_urls_test() ->
    ?assertEqual(
       [
        "/us/product/Swiss-Army-Knives/Category/Everyday-Use/Spartan/53151",
        "/us/product/Swiss-Army-Knives/Category/Everyday-Use/Super-Tinker-United-States-Flag/53342",
        "/us/product/Swiss-Army-Knives/Category/Do-It-Yourself/Tinker/53101",
        "/us/product/Swiss-Army-Knives/Category/Everyday-Use/Classic-SD-United-States-Flag/54216",
        "/us/product/Swiss-Army-Knives/Category/Do-It-Yourself/Small-Tinker/53133",
        "/us/product/Swiss-Army-Knives/Category/Do-It-Yourself/Craftsman/53721",
        "/us/product/Swiss-Army-Knives/Category/Hunting/Mechanic/55441",
        "/us/product/Swiss-Army-Knives/Category/Do-It-Yourself/Handyman/53722",
        "/us/product/Swiss-Army-Knives/Category/Hunting/CyberTool-29/54919",
        "/us/product/Swiss-Army-Knives/Category/Do-It-Yourself/CyberTool-34/53919",
        "/us/product/Swiss-Army-Knives/Category/Do-It-Yourself/Deluxe-Tinker/53481",
        "/us/product/Swiss-Army-Knives/Category/Hunting/Special-Edition-Nylon-Guys-Spartan/55153"
       ],
       index_page:product_urls(
         index_page:parse(spider_fixture:index_page())
        )
      ).

