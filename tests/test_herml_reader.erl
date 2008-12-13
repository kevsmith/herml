-module(test_herml_reader).

-author("kevin@hypotheticalabs.com").

-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
  [?_assertMatch([{"This is a test file.", []},
                  {"It tests line breaking.", []},
                  {"There should be three lines.", []}],
                 herml_reader:file("tests/examples/simple_file.herml"))].

nesting_test_() ->
  [?_assertMatch([{"This is a line.",
                   [{"  This is another line.",[]},
                    {"  This line is on the same line as the other one.",
                     [{"    This is the innermost one.", []},
                      { "    This is a continuation.",[]}]}]},
                  {"This is the second group.",
                   [{"  This is another line in the second group.",[]},
                    {"  This line is on the same line in the second group.",
                     [{"    This is the innermost line of the second group.",
                       []}]}]},
                  {"Third group one.",[]},
                  {"Third group two.",[]},
                  {"Third group three.",
                   [{"  Third group four.",[]}]}],
                 herml_reader:file("tests/examples/simple_nesting.herml"))].

error_test_() ->
  [?_assertThrow({error, bad_indent, _}, herml_reader:file("tests/examples/bad_indent.herml"))].
