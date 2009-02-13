-module(test_herml_reader).

-author("kevin@hypotheticalabs.com").

-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
  [?_assertMatch([{0, "This is a test file.", []},
                  {0, "It tests line breaking.", []},
                  {0, "There should be three lines.", []}],
                 herml_reader:file("tests/examples/simple_file.herml"))].

nesting_test_() ->
  [?_assertMatch([{0, "This is a line.",
                   [{1, "  This is another line.",[]},
                    {1, "  This line is on the same line as the other one.",
                     [{2, "    This is the innermost one.", []},
                      {2, "    This is a continuation.",[]}]}]},
                  {0, "This is the second group.",
                   [{1, "  This is another line in the second group.",[]},
                    {1, "  This line is on the same line in the second group.",
                     [{2, "    This is the innermost line of the second group.",
                       []}]}]},
                  {0, "Third group one.",[]},
                  {0, "Third group two.",[]},
                  {0, "Third group three.",
                   [{1, "  Third group four.",[]}]}],
                 herml_reader:file("tests/examples/simple_nesting.herml"))].

error_test_() ->
  [?_assertThrow({error, bad_indent, _}, herml_reader:file("tests/examples/bad_indent.herml"))].
