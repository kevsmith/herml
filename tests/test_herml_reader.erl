-module(test_herml_reader).

-author("kevin@hypotheticalabs.com").

-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
  [?_assertMatch([{node, 0, "This is a test file.", []},
                  {node, 0, "It tests line breaking.", []},
                  {node, 0, "There should be three lines.", []}],
                 herml_reader:file("tests/examples/simple_file.herml"))].

nesting_test_() ->
  [?_assertMatch([{node,0,"This is a line.",
                   [{node,1,"  This is another line.",[]},
                    {node,1,"  This line is on the same line as the other one.",
                     [{node,2,
                       "    This is the innermost one.", []},
                      {node,2, "    This is a continuation.",[]}]}]},
                  {node,0,"This is the second group.",
                   [{node,1,"  This is another line in the second group.",[]},
                    {node,1,
                     "  This line is on the same line in the second group.",
                     [{node,2,
                       "    This is the innermost line of the second group.",
                       []}]}]},
                  {node,0,"Third group one.",[]},
                  {node,0,"Third group two.",[]},
                  {node,0,"Third group three.",
                   [{node,1,"  Third group four.",[]}]}],
                 herml_reader:file("tests/examples/simple_nesting.herml"))].

error_test_() ->
  [?_assertThrow({error, bad_indent, _}, herml_reader:file("tests/examples/bad_indent.herml"))].
