-module(herml_html_render).

-include("tree.hrl").

-author("kevin@hypotheticalabs.com").

-export([render/1]).

render(Tree) ->
  render(Tree, 0, []).

%% Internal functions
render([H|T], Indent, Acc) ->
  Acc1 = case H#herml_node.type of
           tag ->
             render_tag(H, Indent, Acc);
           text ->
             render_text(H, Indent, Acc)
         end,
  render(T, Indent, Acc1);
render([], _Indent, Acc) ->
  lists:flatten(Acc).

render_text(H, Indent, Acc) ->
  Acc ++ generate_indent(Indent) ++
    proplists:get_value(body, H#herml_node.attrs) ++ "\n".

render_tag(H, Indent, Acc) ->
  Acc1 = Acc ++ generate_indent(Indent),
  if
    length(H#herml_node.children) == 0 ->
      Acc1 ++ build_tag_begin(H) ++ "/>\n";
    true ->
      Acc2 = Acc1 ++ build_tag_begin(H) ++ ">\n",
      Acc3 = render(H#herml_node.children, Indent + 1, Acc2),
      Acc3 ++ generate_indent(Indent) ++ build_end_tag(H) ++ "\n"
  end.

build_tag_begin(H) ->
  #herml_node{attrs=Attrs} = H,
  Buf = "<" ++ proplists:get_value(tag_name, Attrs),
  if
    length(Attrs) > 1 ->
      lists:foldl(fun({Name, Value}, Acc) ->
                      case Name of
                        tag_name ->
                          Acc;
                        _ ->
                          Acc ++ " " ++ atom_to_list(Name) ++ "=\"" ++ Value ++ "\""
                      end end, Buf, Attrs);
    true ->
      Buf
  end.

build_end_tag(H) ->
  "</" ++ proplists:get_value(tag_name, H#herml_node.attrs) ++ ">".

generate_indent(0) ->
  "";
generate_indent(Level) ->
  lists:flatten(lists:map(fun(_) ->
                              "  " end, lists:seq(1, Level))).
