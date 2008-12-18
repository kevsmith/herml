-module(test_herml_parser).

-author("kevin@hypotheticalabs.com").
-author("seancribbs@gmail.com").

-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
  [?_assertThrow({invalid_nesting, _}, herml_parser:string("Some text\n  Some nested text"))].