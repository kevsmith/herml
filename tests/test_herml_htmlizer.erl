-module(test_herml_htmlizer).

-author("kevin@hypotheticalabs.com").

-include_lib("eunit/include/eunit.hrl").

-export([default_attr/1]).

default_attr(_) ->
  [{color, "red"},
   {class, "foo"}].

render_test_() ->
  [check("tests/examples/hello_world"),
   check("tests/examples/message", [{"Message", "This is a test"}]),
   check("tests/examples/message2", [{"Message", "This is a test"}]),
   check("tests/examples/default_attr"),
   check("tests/examples/horizontal_rule"),
   check("tests/examples/close_empty"),
   check("tests/examples/doctypes"),
   check("tests/examples/multiple_classes"),
   check("tests/examples/sort_attributes"),
   check("tests/examples/style_attribute")].

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
