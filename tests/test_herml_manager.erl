-module(test_herml_manager).

-author("kevin@hypotheticalabs.com").

-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_MGR, herml_mgr).

dev_mode_test_() ->
  {setup, fun() -> herml_manager:start_link(?DEFAULT_MGR, "tests/examples", [{development, true}]) end,
   fun({ok, _P}) -> herml_manager:shutdown(?DEFAULT_MGR),
                  file:delete("tests/examples/mutate.herml") end,
   [fun() ->
        file:write_file("tests/examples/mutate.herml", "hello, world"),
        {ok, R} = herml_manager:execute_template(?DEFAULT_MGR, "mutate.herml", []),
        ?assertMatch("hello, world\n", string:strip(lists:flatten(R))),
        file:write_file("tests/examples/mutate.herml", "goodbye, world"),
        {ok, R1} = herml_manager:execute_template(?DEFAULT_MGR, "mutate.herml", []),
        ?assertMatch("goodbye, world\n", string:strip(lists:flatten(R1))) end]}.

prod_mode_test_() ->
  {setup, fun() -> herml_manager:start_link(?DEFAULT_MGR, "tests/examples") end,
   fun({ok, _P}) -> herml_manager:shutdown(?DEFAULT_MGR),
                  file:delete("tests/examples/mutate.herml") end,
   [fun() ->
        file:write_file("tests/examples/mutate.herml", "hello, world"),
        {ok, R} = herml_manager:execute_template(?DEFAULT_MGR, "mutate.herml", []),
        ?assertMatch("hello, world\n", string:strip(lists:flatten(R))),
        file:write_file("tests/examples/mutate.herml", "goodbye, world"),
        {ok, R1} = herml_manager:execute_template(?DEFAULT_MGR, "mutate.herml", []),
        ?assertMatch("hello, world\n", string:strip(lists:flatten(R1))) end]}.
