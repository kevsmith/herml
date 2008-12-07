-module(test_suite).

-author("kevin@hypotheticalabs.com").

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
  [{module, test_herml_scan},
   {module, test_herml_parse},
   {module, test_herml_reader}].
