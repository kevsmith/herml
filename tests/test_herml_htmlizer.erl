-module(test_herml_htmlizer).

-author("kevin@hypotheticalabs.com").

-include_lib("eunit/include/eunit.hrl").

-export([default_attr/1]).
-export([emit_single/1, emit_multi/2]).
-export([emit_single/2, emit_multi/3]).

default_attr(Env) ->
  {[{color, "red"},
    {class, "foo"}], Env}.

emit_single(Arg) ->
  Arg.

emit_multi(Arg1, Arg2) ->
  [Arg1, Arg2].

emit_single(Arg, Env) ->
  {Arg ++ "!", Env}.

emit_multi(Arg1, Arg2, Env) ->
  {[Arg1 ++ "!", Arg2], Env}.



render_test_() ->
  [check("tests/examples/hello_world"),
   check("tests/examples/message", [{"Message", "This is a test"}]),
   check("tests/examples/message2", [{"Message", "This is a test"}]),
   check("tests/examples/default_attr"),
   check("tests/examples/call_single"),
   check("tests/examples/call_single_env"),
   check("tests/examples/call_multi"),
   check("tests/examples/call_multi_params", [{"Foo", "This is foo"}]),
   check("tests/examples/call_multi_env"),
   check("tests/examples/horizontal_rule"),
   check("tests/examples/close_empty"),
   check("tests/examples/doctypes"),
   check("tests/examples/multiple_classes"),
   check("tests/examples/sort_attributes"),
   check("tests/examples/style_attribute"),
   check("tests/examples/simple_loop", [{"Users", ["kevsmith", "seancribbs"]}]),
   check("tests/examples/loop_with_ignores", [{"Users", [{1, "kevsmith"}, {2, "seancribbs"}]}]),
   check("tests/examples/structured_loop", [{"Users", [{1, "kevsmith"}, {2, "seancribbs"}]}])].

iteration_match_test_() ->
  [
    iteration_bad_match("tests/examples/structured_loop", [{"Users", [{1, "kevsmith"}, {2, "seancribbs", "foobar"}]}])
  ].

iteration_bad_match(File, Env) ->
  C = herml_parser:file(File ++ ".herml"),
  ?_assertThrow(bad_match, herml_htmlizer:render(C, Env)).

check(FileName) ->
  check(FileName, []).

check(FileName, Env) ->
  CR = read_file(FileName),
  PR = render_file(FileName, Env),
  ?_assertEqual(CR, PR).

read_file(File) ->
  {ok, C} = file:read_file(File ++ ".render"),
  binary_to_list(C).

render_file(File, Env) ->
  C = herml_parser:file(File ++ ".herml"),
  lists:flatten(herml_htmlizer:render(C, Env)).
