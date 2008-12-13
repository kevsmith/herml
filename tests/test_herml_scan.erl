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
   ?_assertMatch({ok, [{number, _, 123}], _}, herml_scan:string("123"))].
