-module(test_herml_htmlizer).

-author("kevin@hypotheticalabs.com").

-include_lib("eunit/include/eunit.hrl").

-export([default_attr/1]).
-export([emit_single/1, emit_multi/2]).
-export([emit_single/2, emit_multi/3]).
-export([attr_key/0, attr_key/1, attr_value/0, attr_value/1]).

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

attr_key() -> "class".
attr_key(Key) when is_list(Key) -> "key" ++ Key.

attr_value() -> "awesome".
attr_value(Value) when is_list(Value) -> "value" ++ Value.

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
   check("tests/examples/dashed_attrs"),
   check("tests/examples/string_and_number_attrs"),
   check("tests/examples/funcall_attrs", [{"Key", "class"}, {"Value", "awesome"}]),
   check("tests/examples/variable_attrs", [{"Key", "class"}, {"Value", "awesome"}]),
   check("tests/examples/simple_loop", [{"Users", ["kevsmith", "seancribbs"]}]),
   check("tests/examples/loop_with_ignores", [{"Users", [{1, "kevsmith"}, {2, "seancribbs"}]}]),
   check("tests/examples/structured_loop", [{"Users", [{1, "kevsmith"}, {2, "seancribbs"}]}]),
   check("tests/examples/tuple_access", [{"Users", [{1, "kevsmith"}, {2, "seancribbs"}]}]),
   check("tests/examples/atom_value", [{"User", undefined}])].

sub_template_test() ->
  {ok, _Pid} = herml_manager:start_link(foo, "tests/examples"),
  {ok, Rendered} = herml_manager:execute_template(foo, "main.herml"),
  {ok, PreRendered} = file:read_file("tests/examples/main.render"),
  herml_manager:shutdown(foo),
  ?assertEqual(binary_to_list(PreRendered), lists:flatten(Rendered)).

iteration_match_test_() ->
  [
    iteration_bad_match("tests/examples/structured_loop", [{"Users", [{1, "kevsmith"}, {2, "seancribbs", "foobar"}]}])
  ].

iteration_bad_match(File, Env) ->
  C = herml_parser:file(File ++ ".herml"),
  ?_assertThrow(bad_match, herml_htmlizer:render(C, Env, 0)).

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
  lists:flatten(herml_htmlizer:render(C, Env, 0)).
