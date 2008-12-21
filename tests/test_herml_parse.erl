-module(test_herml_parse).

-author("kevin@hypotheticalabs.com").

-include_lib("eunit/include/eunit.hrl").

tag_decl_test_() ->
  [?_assertMatch({ok, {tag_decl, [{tag_name, "td"}]}}, lex_and_parse("%td")),
   ?_assertMatch({ok, {tag_decl, [{singleton, true},
                                  {tag_name, "td"}]}}, lex_and_parse("%td/")),
   ?_assertMatch({ok, {tag_decl, [{tag_name, "td"},
                                  {id, "foo"}]}}, lex_and_parse("%td#foo")),
   ?_assertMatch({ok, {tag_decl, [{singleton, true},
                                  {tag_name, "td"},
                                  {id, "foo"}]}}, lex_and_parse("%td#foo/")),
   ?_assertMatch({ok, {tag_decl, [{tag_name, "td"},
                                  {class, "row"}]}}, lex_and_parse("%td.row")),
   ?_assertMatch({ok, {tag_decl, [{singleton, true},
                                  {tag_name, "td"},
                                  {class, "row"}]}}, lex_and_parse("%td.row/")),
   ?_assertMatch({ok, {tag_decl, [{tag_name, "td"},
                                  {id, "foo"},
                                  {class, "row"}]}}, lex_and_parse("%td#foo.row")),
   ?_assertMatch({ok, {tag_decl, [{singleton, true},
                                  {tag_name, "td"},
                                  {id, "foo"},
                                  {class, "row"}]}}, lex_and_parse("%td#foo.row/")),
   ?_assertMatch({ok, {tag_decl, [{tag_name, "td"},
                                  {id, "foo123"},
                                  {class, "row5"}]}}, lex_and_parse("%td#foo123.row5")),
   ?_assertMatch({ok, {tag_decl, [{tag_name, "td"},
                                  {id, "foo123"},
                                  {class, "row5"}]}}, lex_and_parse("%td.row5#foo123")),
   ?_assertMatch({ok, {tag_decl, [{singleton, true},
                                  {tag_name, "td"},
                                  {id, "foo123"},
                                  {class, "row5"}]}}, lex_and_parse("%td.row5#foo123/")),
   ?_assertMatch({ok, {var_ref, "Message"}}, lex_and_parse("@Message"))].

attr_list_test_() ->
  [?_assertMatch({ok, {tag_decl, [{tag_name, "td"},
                                  {width, "200"}]}}, lex_and_parse("%td[{width, '200'}]")),
   ?_assertMatch({ok, {tag_decl, [{tag_name, "td"},
                                  {id, "foo123"},
                                  {width, "200"}]}}, lex_and_parse("%td#foo123[{width, '200'}]")),
   ?_assertMatch({ok, {tag_decl, [{tag_name, "td"},
                                  {id, "foo123"},
                                  {class, "row"},
                                  {width, "200"}]}}, lex_and_parse("%td#foo123.row[{width, '200'}]")),
   ?_assertMatch({ok, {tag_decl, [{tag_name, "td"},
                                  {id, "foo123"},
                                  {class, "row"},
                                  {width, "200"}]}}, lex_and_parse("%td.row#foo123[{width, '200'}]")),
   ?_assertMatch({ok, {tag_decl, [{tag_name, "td"},
                                  {id, "foo123"},
                                  {width, "200"},
                                  {color, "red"}]}}, lex_and_parse("%td#foo123[{width, '200'}, {color, 'red'}]")),
   ?_assertMatch({ok, {tag_decl, [{tag_name, "td"},
                                  {id, "foo123"},
                                  {width, "200%"},
                                  {color, "red"}]}}, lex_and_parse("%td#foo123[{width, '200%'}, {color, 'red'}]")),
   ?_assertMatch({ok, {tag_decl, [{tag_name, "td"},
                                  {id, "foo123"},
                                  {width, {var_ref, "Width"}},
                                  {color, "red"}]}}, lex_and_parse("%td#foo123[{width, @Width}, {color, 'red'}]")),
   ?_assertMatch({ok, {tag_decl, [{singleton, true},
                                  {tag_name, "td"},
                                  {id, "foo123"},
                                  {width, {var_ref, "Width"}},
                                  {color, "red"}]}}, lex_and_parse("%td#foo123[{width, @Width}, {color, 'red'}]/")),
   ?_assertMatch({ok, {tag_decl, [{tag_name, "td"},
                                  {id, "foo123"},
                                  {fun_call, user, default_attr}]}}, lex_and_parse("%td#foo123[@user:default_attr]")),
   ?_assertMatch({ok, {tag_decl, [{tag_name, "td"}, 
                                  {fun_call, user, default_attr}, 
                                  {onclick, "alert()"}]}}, lex_and_parse("%td[@user:default_attr, {onclick, 'alert()'}]"))].



default_div_test_() ->
  [?_assertMatch({ok, {tag_decl, [{tag_name, "div"},
                                  {id, "foo123"}]}}, lex_and_parse("#foo123")),
   ?_assertMatch({ok, {tag_decl, [{tag_name, "div"},
                                  {class, "row"}]}}, lex_and_parse(".row")),
   ?_assertMatch({ok, {tag_decl, [{tag_name, "div"},
                                  {id, "foo123"},
                                  {class, "row"}]}}, lex_and_parse("#foo123.row")),
   ?_assertMatch({ok, {tag_decl, [{tag_name, "div"},
                                  {id, "foo123"},
                                  {class, "row"}]}}, lex_and_parse(".row#foo123"))].

default_div_attr_list_test_() ->
  [?_assertMatch({ok, {tag_decl, [{tag_name, "div"},
                                  {id, "foo123"},
                                  {width, "200"}]}}, lex_and_parse("#foo123[{width, '200'}]")),
   ?_assertMatch({ok, {tag_decl, [{tag_name, "div"},
                                  {class, "row"},
                                  {width, "200"}]}}, lex_and_parse(".row[{width, '200'}]")),
   ?_assertMatch({ok, {tag_decl, [{tag_name, "div"},
                                  {id, "foo123"},
                                  {class, "row"},
                                  {width, "200"}]}}, lex_and_parse("#foo123.row[{width, '200'}]")),
   ?_assertMatch({ok, {tag_decl, [{tag_name, "div"},
                                  {id, "foo123"},
                                  {class, "row"},
                                  {width, "200"}]}}, lex_and_parse(".row#foo123[{width, '200'}]"))].

doctype_test_()->
  [?_assertMatch({ok, {doctype, "Transitional"}}, lex_and_parse("!!!")),
   ?_assertMatch({ok, {doctype, "Strict"}}, lex_and_parse("!!! Strict")),
   ?_assertMatch({ok, {doctype, "1.1"}}, lex_and_parse("!!! 1.1"))].

lex_and_parse(Text) ->
  {ok, T, _} = herml_scan:string(Text),
  herml_parse:parse(T).
