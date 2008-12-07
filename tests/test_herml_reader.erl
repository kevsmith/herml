-module(test_herml_reader).

-author("kevin@hypotheticalabs.com").

-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
  [?_assertMatch([{herml_node,text,[{body,"This is a test file."}],[]},
                  {herml_node,text,[{body,"It tests line breaking."}],[]},
                  {herml_node,text,
                   [{body,"There should be three lines."}],
                   []}], herml_reader:file("tests/examples/simple_file.herml"))].

nesting_test_() ->
  [?_assertMatch([{herml_node,text,
                   [{body,"This is a line."}],
                   [{herml_node,text,[{body,"  This is another line."}],[]},
                    {herml_node,text,
                     [{body,"  This line is on the same line as the other one."}],
                     [{herml_node,text,
                       [{body,"    This is the innermost one."}],
                       []}]}]},
                  {herml_node,text,
                   [{body,"This is the second group."}],
                   [{herml_node,text,
                     [{body,"  This is another line in the second group."}],
                     []},
                    {herml_node,text,
                     [{body,
                       "  This line is on the same line in the second group."}],
                     [{herml_node,text,
                       [{body,
                         "    This is the innermost line of the second group."}],
                       []}]}]},
                  {herml_node,text,[{body,"Third group one."}],[]},
                  {herml_node,text,
                   [{body,"Third group two."}],
                   [{herml_node,text,
                     [{body,"  Third group three."}],
                     [{herml_node,text,[{body,"    Third group four."}],[]}]}]}],
                 herml_reader:file("tests/examples/simple_nesting.herml"))].

consolidation_test_() ->
  [?_assertMatch([{herml_node, tag,
                   [{tag_name, "div"}, {id,"title"},{id,"5"},{class,"big"}],[]}],
                 herml_reader:file("tests/examples/consolidate_peers.herml"))].

div_test_() ->
  [?_assertMatch([{herml_node, tag,
                   [{tag_name, "div"},
                    {id,"content"}],
                   [{herml_node, tag,[{tag_name, "div"}, {class,"title"}],[]}]}],
                 herml_reader:file("tests/examples/div_snippet1.herml")),
  ?_assertMatch([{herml_node, tag,
                  [{tag_name, "div"},
                   {id,"content"}],
                  [{herml_node, tag,
                    [{tag_name, "div"},
                     {class,"title"}],
                    [{herml_node,text,[{body,"    Login"}],[]}]}]}],
                herml_reader:file("tests/examples/div_snippet2.herml")),
  ?_assertMatch([{herml_node, tag,
                  [{tag_name, "div"},
                   {id,"content"},{attr,"one"}],
                  [{herml_node, tag,
                    [{tag_name, "div"},
                     {class,"title"}],
                    [{herml_node,text,[{body,"    Login"}],[]}]}]}],
                herml_reader:file("tests/examples/div_snippet3.herml"))].

generic_tag_test_() ->
  [?_assertMatch([{herml_node,tag,
                   [{tag_name,"html"}],
                   [{herml_node,tag,
                     [{tag_name,"body"}],
                     [{herml_node,tag,
                       [{tag_name,"div"},{id,"message"}],
                       [{herml_node,text,[{body,"      Hello, world!"}],[]}]}]}]}],
                 herml_reader:file("tests/examples/basic_html.herml"))].

error_test_() ->
  [?_assertThrow({bad_indent, _}, herml_reader:file("tests/examples/bad_indent.herml")),
   ?_assertThrow({bad_list_terminator, _}, herml_reader:file("tests/examples/bad_list1.herml"))].
