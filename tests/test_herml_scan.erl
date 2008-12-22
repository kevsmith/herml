-module(test_herml_scan).

-author("kevin@hypotheticalabs.com").

-include_lib("eunit/include/eunit.hrl").

tokenizing_test_() ->
  [?_assertMatch({ok, [{tag_start, _, _}], _}, herml_scan:string("%")),
   ?_assertMatch({ok, [{class_start, _, _}], _}, herml_scan:string(".")),
   ?_assertMatch({ok, [{id_start, _, _}], _}, herml_scan:string("#")),
   ?_assertMatch({ok, [{chr, _, "abc"}], _}, herml_scan:string("abc")),
   ?_assertMatch({ok, [{quote, _, _},
                       {chr, _, "testing"},
                       {quote, _, _}], _}, herml_scan:string("'testing'")),
   ?_assertMatch({ok, [{lcurly, _, _}], _}, herml_scan:string("{")),
   ?_assertMatch({ok, [{rcurly, _, _}], _}, herml_scan:string("}")),
   ?_assertMatch({ok, [{lbrace, _, _}], _}, herml_scan:string("[")),
   ?_assertMatch({ok, [{rbrace, _, _}], _}, herml_scan:string("]")),
   ?_assertMatch({ok, [{at, _, _}], _}, herml_scan:string("@")),
   ?_assertMatch({ok, [{comma, _, _}], _}, herml_scan:string(",")),
   ?_assertMatch({ok, [{number, _, 123}], _}, herml_scan:string("123")),
   ?_assertMatch({ok, [{colon, _, ":"}], _}, herml_scan:string(":")),
   ?_assertMatch({ok, [{slash, _, "/"}], _}, herml_scan:string("/")),
   ?_assertMatch({ok, [{dash, _, "-"}], _}, herml_scan:string("-")),
   ?_assertMatch({ok, [{chr, _, "abc"},
                       {dash, _, "-"},
                       {chr, _, "def"}], _}, herml_scan:string("abc-def")),
   ?_assertMatch({ok, [{lt, _, "<"}], _}, herml_scan:string("<")),
   ?_assertMatch({ok, [{gt, _, ">"}], _}, herml_scan:string(">"))].
