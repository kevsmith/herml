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
   check("tests/examples/default_attr")].

check(FileName) ->
  fun() ->
      CR = read_file(FileName),
      PR = render_file(FileName),
      ?_assertMatch(CR, PR) end.

check(FileName, Env) ->
  fun() ->
      CR = read_file(FileName),
      PR = render_file(FileName, Env),
      ?_assertMatch(CR, PR) end.


read_file(File) ->
  {ok, C} = file:read_file(File ++ ".render"),
  binary_to_list(C).

render_file(File) ->
  C = herml_parser:file(File ++ ".herml"),
  herml_htmlizer:render(C).

render_file(File, Env) ->
  C = herml_parser:file(File ++ ".herml"),
  herml_htmlizer:render(C, Env).
